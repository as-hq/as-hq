import zmq
import json

from .shell import ASShell


class ASKernel(object):

  def __init__(self, host='localhost', port=20000):
    self.init_shell()
    self.init_zmq(host, port)

  def init_shell(self):
    ns = self.get_initial_ns()
    self.shell = ASShell(ns)

  def init_zmq(self, host, port):
    context = zmq.Context()
    self.socket = context.socket(zmq.REP)
    self.url = "tcp://" + host + ":" + str(port)
    self.socket.bind(self.url)
    
  def get_initial_ns(self):
    from AS.stdlib import *
    import matplotlib._pylab_helpers
    return locals()

  def handle_incoming(self):
    recvMsg = self.socket.recvJson()
    replyMsg = self.process_message(recvMsg)
    self.socket.send(json.dumps(replyMsg))

  def process_message(self, msg):
    if msg['type'] is 'evaluate_cell':
      result = None
      if msg['scope'] is 'Header':
        result = self.shell.run_header(msg['code'], msg['sheet_id'])
      elif msg['scope'] is 'Cell':
        result = self.shell.run_cell(msg['code'], msg['sheet_id'])
      return self.exec_result_to_msg(result)

    elif msg['type'] is 'get_status':
      raise NotImplementedError

    elif msg['type'] is 'autocomplete':
      raise NotImplementedError

  def exec_result_to_msg(self, result):
    reply = {}
    result_dict = result.__dict__
    if 'result' in result_dict:
      reply['value'] = result_dict['result']
    if 'error_in_exec' in result_dict:
      reply['error'] = result_dict['error_in_exec']
    return reply

