from __future__ import print_function

import sys
import zmq
import json
import threading
import signal
import traceback

from .shell import ASShell

from IPython.core.pylabtools import import_pylab


class ASKernel(object):

  def __init__(self, address='tcp://*:20000', num_threads=30):
    self.init_shell()
    self.init_zmq(address, num_threads)

  def init_shell(self):
    init_ns = self.get_initial_ns()
    self.shell = ASShell(user_ns=init_ns)

  def init_zmq(self, address, num_threads):
    self.context = zmq.Context()
    self.url_client = address
    self.url_worker = "inproc://workers"
    self.num_threads = max(num_threads, 1)

  def get_initial_ns(self):
    import matplotlib
    matplotlib.use('Agg')
    from AS.stdlib import *
    from AS.kernel.serialize import * # this is imported for serialization code injection
    from AS.functions.openExcel import *
    import matplotlib._pylab_helpers
    import matplotlib.pyplot as plt
    import cPickle
    import base64
    ns = locals()
    return ns

  def listen(self):
  # the kernel topology:
  # 
  # [async requests]                                             [worker queue]  
  #                                                                     
  # ------>                                                        ---> worker 1 
  # ------>  Router (tcp://*:20000) ---> Dealer (inproc://workers) ---> worker 2 
  # ------>                                                        ---> worker n 

    print('\nKernel listening at address:', self.url_client, '\n', file=sys.__stdout__)

    # bind addresses
    self.router = self.context.socket(zmq.ROUTER)
    self.router.bind(self.url_client)
    self.dealer = self.context.socket(zmq.DEALER)
    self.dealer.bind(self.url_worker)
    self.threads = []

    # launch worker thread pool
    for i in range(self.num_threads):
      thread = threading.Thread(target=self.message_worker, args=(self.url_worker,i))
      self.threads.append(thread)
      thread.start()

    # forwards KeyboardInterrupt to a signal zmq will listen to
    signal.signal(signal.SIGINT, signal.SIG_DFL);
    
    # attach the Router and Dealer in order to forward requests to a 
    # worker queue, as seen in the art above
    zmq.device(zmq.QUEUE, self.router, self.dealer) 

    # clean up when finished
    self.close()

  def message_worker(self, worker_url, thread_id):
    socket = self.context.socket(zmq.REP)
    socket.connect(worker_url)
    while True:
      recvMsg = json.loads(socket.recv())
      print('processing', recvMsg['type'], "on thread", thread_id, file=sys.__stdout__)
      try:
        replyMsg = self.process_message(recvMsg)
        socket.send(json.dumps(replyMsg))
      except KeyboardInterrupt: 
        raise
      except Exception as e:
        replyMsg = {'type': 'error', 'error': repr(e)}
        print("Kernel error: ", e, file=sys.__stdout__)
        socket.send(json.dumps(replyMsg))

  def process_message(self, msg):
  # messages are handled in an exactly isomorphic way with the KernelRequest/KernelReply 
  # types in AS/Kernels/Python.hs in backend. 
    if msg['type'] == 'evaluate':
      result = None
      if msg['scope'] == 'Header':
        result = self.shell.run_header(msg['code'], msg['sheet_id'])
      elif msg['scope'] == 'Cell':
        result = self.shell.run_cell(msg['code'], msg['sheet_id'])
      else:
        raise NotImplementedError
      return self.exec_result_to_msg(result, msg)

    elif msg['type'] == 'evaluate_format':
      result = self.shell.run_raw(msg['code'], msg['sheet_id'])
      # result.result can be any python datatype; we assume any call to "run_raw"
      # expects a string in response. So, invoke repr.
      if result.result:
        result.result = repr(result.result) 
      return self.exec_result_to_msg(result, msg)

    elif msg['type'] == 'get_status':
      raise NotImplementedError

    elif msg['type'] == 'autocomplete':
      raise NotImplementedError

    elif msg['type'] == 'clear':
      self.shell.init_sheet_ns(msg['sheet_id'])
      return {'type': 'clear', 'success': True}

  def exec_result_to_msg(self, result, recvMsg):
    reply = {}
    reply['type'] = recvMsg['type']
    if result.result is not None:
      reply['value'] = result.result # already serialized by the shell
    if result.error_before_exec is not None:
      reply['error'] = repr(result.error_before_exec)
    if result.error_in_exec is not None:
      reply['error'] = repr(result.error_in_exec)
    if len(result.display) > 0:
      reply['display'] = "\n".join(result.display)
    return reply

  def close(self):
    # terminate the threads
    try: 
      self.threads = [t.join(1) for t in self.threads if t.isAlive()]
    except KeyboardInterrupt:
      for t in self.threads:
        t.kill_received = True
    # close the bound devices, and terminate the context.
    self.router.close()
    self.dealer.close()
    self.context.term()
