from __future__ import print_function

import os
import sys
import zmq
import json
import threading
import signal
import traceback
import ctypes
import shortid
import random
import subprocess
import re
from time import sleep
from datetime import datetime

from .shell import ASShell
from  AS.errors import TimeoutException

from IPython.core.pylabtools import import_pylab

#-------------------------------------------------------------------------------
# Constants

NUM_WORKERS         = 50
NUM_WORKERS_LIMIT   = 200
LOGGING_ON          = True
LOG_DIR             = "./logs/"
WORKER_REGISTRATION_TIMEOUT = 3.0

# ZMQ message delimiter
EMPTY = b""

# the number of additional workers created when high load is detected
LOAD_RESPONSE_DELTA = 5

#-------------------------------------------------------------------------------
# Helpers

sid = shortid.ShortId()
def id_gen():
  return sid.generate()

if LOGGING_ON:
  #  Initialize logging
  if not os.path.exists(LOG_DIR):
    os.makedirs(LOG_DIR)
  logFile = LOG_DIR + "[pykernel]" + datetime.utcnow().isoformat() + '.txt'
  log = open(logFile, 'a')

# Print/log utility functions
def puts(msg):
  print(msg, file=sys.__stdout__)
  if LOGGING_ON:
    log.write(repr(msg) + '\n')

def putsTimed(msg):
  msg_ = msg + ' [' + datetime.utcnow().isoformat() + ']'
  puts(msg_)

#-------------------------------------------------------------------------------
# Worker class

# stoppable thread subclass
class ASThread(threading.Thread):

  def set_id(self, worker_id):
    self.worker_id = worker_id

  # a workaround to actually kill threads
  # exc_obj is the class of the exception raised in the target thread
  def terminate(self, exc_obj):
    res = ctypes.pythonapi.PyThreadState_SetAsyncExc(
      ctypes.c_long(self.ident), 
      ctypes.py_object(exc_obj)
    )

  def set_working_status(self, is_working, message_id=None): # status: boolean
    self.working = is_working
    self.workingMessage = message_id

  def is_working(self):
    return self.working

#-------------------------------------------------------------------------------
# Kernel class


# the kernel topology:
# 
# [async requests]                                                  [workers]  
#                                                                     
# ------>                                                         ---> worker 1 
# ------>  Router  <---> Poller <---> Router                      ---> worker 2 
# ------>                                ^                        ---> worker n 
#                                  (load balancing)                                          

class ASKernel(object):
  def __init__(self, num_workers=NUM_WORKERS):
    self.init_shell()
    self.context = zmq.Context()
    self.workers = {} # Map WorkerId Worker
    self.num_workers = num_workers
    self.any_workers_registered = False

  def init_shell(self):
    init_ns = self.get_initial_ns()
    self.shell = ASShell(user_ns=init_ns)

  # Initial imports. We import AS.kernel.serialize for serialization 
  # code injection
  def get_initial_ns(self):
    imports = '''
import matplotlib
matplotlib.use('Agg')
from AS.stdlib import *
from AS.kernel.serialize import * 
from AS.functions.openExcel import *
from AS.SQL import *
from AS.object_view import *
import matplotlib._pylab_helpers
import matplotlib.pyplot as plt
import cPickle
import base64
    '''
    ns = {}
    exec imports in ns
    return ns

  def processFrontend(self, frontend, backend, events):
    # poll for messages sent from clients
    goodEvents = frontend in events and events[frontend] == zmq.POLLIN
    if (goodEvents and self.any_workers_registered):
      [client_addr, req] = frontend.recv_multipart()
      msg = json.loads(req)
      if msg['type'] == 'get_status':
        message_id = msg['message_id']
        self.handle_get_status_request(message_id, frontend, client_addr)
      else:
        # check if we can do work before promising to serve a request
        selected_worker_addr = self.get_available_worker_address()

        # all workers engaged; scale up.
        if (selected_worker_addr is None):
          if (self.num_workers < NUM_WORKERS_LIMIT):
            for _ in range(LOAD_RESPONSE_DELTA):
              new_worker_id = id_gen()
              self.install_worker(new_worker_id)
            self.num_workers += LOAD_RESPONSE_DELTA
            puts("SCALED UP: " + str(self.num_workers) + " workers")

        elif (selected_worker_addr == 'UNREGISTERED'):
          puts("Got frontend events, but no registered workers are available.")     

        else:
          # route client request to the selected worker
          backend.send_multipart([
            selected_worker_addr, 
            EMPTY, client_addr, 
            EMPTY, req
          ])

  def processBackend(self, frontend, backend, events):
    # poll for messages sent from workers
    if (backend in events and events[backend] == zmq.POLLIN):
      message = backend.recv_multipart()

      # If 'READY' message, register the network ID of a ready worker
      # invariant: an unregistered worker is never assigned work.
      if (message[2] == 'READY'):
        nid = message[0]
        wid = message[4]
        puts('REGISTERED WORKER worker_id: ' + wid + ', network_id: ' + nid)
        self.any_workers_registered = True
        self.workers[wid].network_id = nid

      # Else, route reply back to client.
      else:
        client_addr = message[2]
        reply = message[4]
        # puts('sent back reply: ' + repr(reply))
        frontend.send_multipart([client_addr, reply])

  def checkAndDie(self):
    if not self.any_workers_registered:
      puts('WORKERS NOT REGISTERED IN TIME; DESTROYING KERNEL')
      if LOGGING_ON:
        log.close()
      os.system('kill %d' % os.getpid())

  def listen(self, address='tcp://*:20000'):
    # respond to pkill
    signal.signal(signal.SIGTERM, signal.SIG_DFL)

    self.url_client = address
    self.url_worker = "inproc://workers"

    puts('Kernel listening at address: ' + self.url_client)

    # bind addresses
    frontend = self.context.socket(zmq.ROUTER)
    frontend.bind(self.url_client)

    backend = self.context.socket(zmq.ROUTER)
    backend.bind(self.url_worker)

    # poller
    poller = zmq.Poller()
    poller.register(backend, zmq.POLLIN)
    poller.register(frontend, zmq.POLLIN)

    # launch worker pool
    for _ in range(self.num_workers):
      worker_id = id_gen()
      worker = self.install_worker(worker_id)
    puts('CREATED ' + str(self.num_workers) + ' WORKERS.')

    t = threading.Timer(WORKER_REGISTRATION_TIMEOUT, self.checkAndDie)
    t.start()

    # main server loop
    while True:
      try:
        events = dict(poller.poll())
        self.processFrontend(frontend, backend, events)
        self.processBackend(frontend, backend, events)
      except KeyboardInterrupt:
        # The zmq poller sometime spuriously receives a SIGINT 
        # despite no action from the user.
        # The workaround for now is to discard keyboardinterrupts.
        puts("KeyboardInterrupt!")
        continue
      except Exception as e:
        puts("poll loop got exception: " + repr(e))
        continue
    # clean up when finished
    self.close(frontend, backend)

  def install_worker(self, worker_id):
    worker = ASThread(target=self.worker_routine, args=(worker_id,))
    self.workers[worker_id] = worker
    worker.set_id(worker_id)
    worker.set_working_status(False)
    worker.start()

  # return: Maybe (Either 'UNREGISTERED' NetworkId)
  # The NetworkId and WorkerId of a worker are different, but unique to 
  # each worker.
  # NetworkId is the address ZMQ uses internally to route messages;
  # WorkerId is a uuid used by the kernel to store and recall workers.
  def get_available_worker_address(self): 
    start = random.randint(0, self.num_workers-2)
    idx = start + 1
    isFirstIter = True
    workers = self.workers.values()
    while(idx != start+1 or isFirstIter):
      worker = workers[idx]
      if not worker.is_working():
        if hasattr(worker, 'network_id'):
          return worker.network_id
        else:
          return 'UNREGISTERED'
      idx = (idx + 1) % self.num_workers
      isFirstIter = False
    return None

  def handle_get_status_request(self, message_id, frontend, addr):
    puts("POKE REQUEST: " + message_id)
    isWorking = lambda mid, w: w.working and w.workingMessage == mid
    workers = filter(lambda w: isWorking(message_id, w), self.workers.values())
    if len(workers) > 0:
      puts("STILL PROCESSING REPLY")
      resp = {'type': 'still_processing'}
      frontend.send_multipart([addr, json.dumps(resp)])

  def halt_message(self, message_id):
    ##  Halts all threads working on a given message_id as follows:
    # - terminate 'myself'
    # - replace 'myself' in the thread pool with a new worker
    puts("HALTING MESSAGE: " + message_id)

    for worker in self.workers.values():
      if worker.workingMessage == message_id:
        replacementWorker = self.install_worker(worker.worker_id)
        worker.terminate(TimeoutException)

  def worker_routine(self, worker_id):
    socket = self.context.socket(zmq.REQ)
    socket.connect(self.url_worker)
    socket.identity = worker_id.encode('ascii')
    socket.send_multipart(['READY', EMPTY, worker_id])

    while True:
      # bookkeeping: reset the working status.
      self.workers[worker_id].set_working_status(False)

      client_addr, empty, request = socket.recv_multipart()
      assert empty == EMPTY
      client_msg = json.loads(request)

      # bookkeeping: mark that this worker is currently working on a message.
      if 'message_id' in client_msg:
        message_id = client_msg['message_id']
      else:
        message_id = None
      self.workers[worker_id].set_working_status(True, message_id)

      putsTimed('worker ' + worker_id + ' PROCESSING')
      puts(repr(client_msg))

      try:
        reply_msg = self.process_message(client_msg, worker_id)
        putsTimed("worker " + worker_id + " FINISHED, sending message")
        socket.send_multipart([client_addr, EMPTY, json.dumps(reply_msg)])
      except KeyboardInterrupt: 
        raise
      except Exception as e:
        try: 
          reply_msg = {'type': 'generic_error', 'error': repr(e)}
          puts("Worker " + worker_id + " error: " + repr(e))
          socket.send_multipart([client_addr, EMPTY, json.dumps(reply_msg)])
        except: 
          traceback.print_exc()
          pass
        traceback.print_exc()
        
      # Build up logs in buffer during work, then flush to file during each 
      # worker's dead zone (the period between having finished work and becoming 
      # non-idle). This is a tradeoff between increasing evaluation time and 
      # increasing the number of workers, favoring the latter.
      if LOGGING_ON:
        log.flush()

  def process_message(self, msg, worker_id):
  # messages are handled in an exactly isomorphic way with the 
  # KernelRequest/KernelReply types in AS/Kernels/Python.hs in backend. 

    if msg['type'] == 'evaluate':
      result = None
      if msg['scope'] == 'Header':
        result = self.shell.run_header(msg['code'], msg['workbook_id'])
      elif msg['scope'] == 'Cell':
        result = self.shell.run_cell(msg['code'], msg['workbook_id'])
      else:
        raise NotImplementedError
      return self.exec_result_to_msg(result, msg)

    elif msg['type'] == 'evaluate_format':
      result = self.shell.run_raw(msg['code'], msg['workbook_id'])
      if result.result:
        # evaluate_format's reply type is string
        result.result = repr(result.result) 
      return self.exec_result_to_msg(result, msg)

    elif msg['type'] == 'open_workbook':
      if not self.shell.has_workbook(msg['workbook_id']):
        self.shell.run_header(msg['code'], msg['workbook_id'])
      return {'type': 'generic_success'}
 
    elif msg['type'] == 'get_status':
      raise NotImplementedError

    elif msg['type'] == 'autocomplete':
      raise NotImplementedError

    elif msg['type'] == 'clear':
      self.shell.init_workbook_ns(msg['workbook_id'])
      return {'type': 'generic_success'}

    elif msg['type'] == 'halt_message':
      self.halt_message(msg['message_id'])
      return {'type': 'generic_success'}

  def exec_result_to_msg(self, result, client_msg):
    reply = {}
    reply['type'] = client_msg['type']
    if result.result is not None:
      reply['value'] = result.result # already serialized by the shell
    if result.error_before_exec is not None:
      reply['error'] = repr(result.error_before_exec)
    if result.error_in_exec is not None:
      reply['error'] = repr(result.error_in_exec)
    if len(result.display) > 0:
      reply['display'] = "\n".join(result.display)
    return reply

  def close(self, frontend, backend):
    puts('Normal kernel exit.')
    if LOGGING_ON:
      log.close()
    # wait for devices to finish polling
    time.sleep(1)
    # terminate the threads
    try: 
      self.workers = [t.join(1) for t in self.workers if t.isAlive()]
    except KeyboardInterrupt:
      for t in self.workers:
        t.terminate(TimeoutException)
    # close the bound devices, and terminate the context.
    frontend.close()
    backend.close()
    self.context.term()

  def kill_port(self, port):
    popen = subprocess.Popen(['netstat', '-lpn'],
                             shell=False,
                             stdout=subprocess.PIPE)
    (data, err) = popen.communicate()

    pattern = "^tcp.*((?:{0})).* (?P<pid>[0-9]*)/.*$"
    pattern = pattern.format(')|(?:'.join([str(port)]))
    prog = re.compile(pattern)
    for line in data.split('\n'):
        match = re.match(prog, line)
        if match:
            pid = match.group('pid')
            subprocess.call(['kill', '-9', pid])
