from __future__ import print_function

import sys
import zmq
import json
import threading
import traceback

from .shell import ASShell

from IPython.core.pylabtools import import_pylab


class ASKernel(object):

  def __init__(self, address='tcp://*:20000', worker_threads=30):
    self.init_shell()
    self.init_zmq(address, worker_threads)

  def init_shell(self):
    init_ns = self.get_initial_ns()
    self.shell = ASShell(user_ns=init_ns)

  def init_zmq(self, address, worker_threads):
    self.context = zmq.Context()
    self.url_client = address
    self.url_worker = "inproc://workers"
    self.worker_threads = max(worker_threads, 1)

  def get_initial_ns(self):
    import matplotlib
    matplotlib.use('Agg')
    from AS.stdlib import *
    from AS.kernel.serialize import * # this is imported for serialization code injection
    import matplotlib._pylab_helpers
    import matplotlib.pyplot as plt
    import cPickle
    import base64
    ns = locals()
    import_pylab(ns)
    return ns

  def listen(self):
    print('\nKernel listening at address:', self.url_client, '\n', file=sys.__stdout__)

    # bind addresses
    clients = self.context.socket(zmq.ROUTER)
    clients.bind(self.url_client)
    workers = self.context.socket(zmq.DEALER)
    workers.bind(self.url_worker)

    # launch worker thread pool
    for i in range(self.worker_threads):
      thread = threading.Thread(target=self.message_worker, args=(self.url_worker,i))
      thread.start()
    zmq.device(zmq.QUEUE, clients, workers)

    # clean up when finished
    clients.close()
    workers.close()
    self.context.term()

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
