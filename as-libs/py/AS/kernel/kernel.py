import zmq
import json

from AS.stdlib import serialize
from .shell import ASShell


class ASKernel(object):

  def __init__(self, host='*', port=20000):
    self.init_shell()
    self.init_zmq(host, port)

  def init_shell(self):
    init_ns = self.get_initial_ns()
    self.shell = ASShell(user_ns=init_ns)

  def init_zmq(self, host, port):
    context = zmq.Context()
    self.socket = context.socket(zmq.REP)
    self.url = 'tcp://' + host + ':' + str(port)
    print '\nKernel listening at address:', self.url, '\n'
    self.socket.bind(self.url)

  def get_initial_ns(self):
    from AS.stdlib import *
    import matplotlib._pylab_helpers
    return locals()

  def handle_incoming(self):
    recvMsg = json.loads(self.socket.recv())
    replyMsg = self.process_message(recvMsg)
    print "sending reply:", replyMsg
    self.socket.send(json.dumps(replyMsg))

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

    elif msg['type'] is 'get_status':
      raise NotImplementedError

    elif msg['type'] is 'autocomplete':
      raise NotImplementedError

  def exec_result_to_msg(self, result, recvMsg):
    reply = {}
    reply['type'] = recvMsg['type']
    if result.result is not None:
      reply['value'] = serialize(result.result)
    if result.error_before_exec is not None:
      reply['error'] = repr(result.error_before_exec)
    if result.error_in_exec is not None:
      reply['error'] = repr(result.error_in_exec)
    if len(result.display) > 0:
      reply['display'] = "\n".join(result.display)
    return reply

