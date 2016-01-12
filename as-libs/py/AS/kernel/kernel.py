import zmq
import json

from AS.stdlib import serialize
from .shell import ASShell


class ASKernel(object):

  def __init__(self, address='tcp://*:20000'):
    self.init_shell()
    self.init_zmq(address)

  def init_shell(self):
    init_ns = self.get_initial_ns()
    self.shell = ASShell(user_ns=init_ns)
    self.shell.serializer = serialize
    self.shell.serialize_post_execute = True

  def init_zmq(self, address):
    context = zmq.Context()
    self.socket = context.socket(zmq.REP)
    self.address = address
    print '\nKernel listening at address:', self.address, '\n'
    self.socket.bind(self.address)

  def get_initial_ns(self):
    from AS.stdlib import *
    from AS.kernel.serialize import wrap_value # this is imported for serialization code injection
    import matplotlib._pylab_helpers
    import matplotlib.pyplot as plt
    return locals()

  def handle_incoming(self):
    recvMsg = json.loads(self.socket.recv())
    replyMsg = self.process_message(recvMsg)
    # print "sending reply:", replyMsg
    self.socket.send(json.dumps(replyMsg))

  def process_message(self, msg):
    print 'processing', msg['type']
    if msg['type'] == 'evaluate':
      result = None
      if msg['scope'] == 'Header':
        result = self.shell.run_header(msg['code'], msg['sheet_id'])
      elif msg['scope'] == 'Cell':
        result = self.shell.run_cell(msg['code'], msg['sheet_id'])
      else:
        raise NotImplementedError
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
