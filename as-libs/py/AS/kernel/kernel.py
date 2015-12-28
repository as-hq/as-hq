import getpass
import sys
import traceback
import zmq

from IPython.core import release
from IPython.core.interactiveshell import InteractiveShell
from ipython_genutils.py3compat import builtin_mod, PY3
from IPython.utils.tokenutil import token_at_cursor, line_at_cursor
from traitlets import Instance, Type, Any, List

from ipykernel.comm import CommManager
from ipykernel.kernelbase import Kernel as KernelBase
from ipykernel.serialize import serialize_object, unpack_apply_message

from .shell import ASShell

#-----------------------------------------------------------------------------
#  Kernel
#-----------------------------------------------------------------------------

class ASKernel(object):

  def __init__(self):
    # init python eval shell
    self.base_ns = self.getInitialNs()
    # shells with independent namespaces, indexed by sheetid
    self.shells = {} 
    # init zmq
    context = zmq.Context()
    self.socket = context.socket(zmq.REP)
    self.socket.bind("tcp://*:20000")

  def getInitialNs():
    from AS.stdlib import *
    from AS.ui.styling import *
    # from AS.tests.min import *
    from AS.instruments.ETF import ETF
    from AS.instruments.Stock import Stock
    from AS.ui.plot import *
    #import json
    # from AS.excel.pycel.excelcompiler import *
    # from AS.excel.pycel.excellib import * # mapping from excel to python
    from sys import exc_info
    from AS.iterable import ASIterable

    import json
    import matplotlib._pylab_helpers
    return locals()

  def createShell(namespace, sheetId):
    raise NotImplementedError

  # String -> SheetId -> ExecutionResult
  def run(code, sheetId):
    raise NotImplementedError
    # self.shells[sheetId].run_cell(...) 

  # String -> SheetId -> ExecutionResult
  def run_isolated(code, sheetId):
    raise NotImplementedError
    # self.shells[sheetId].run_cell(...) 

  def handleIncoming(self):
    recv_message = self.socket.recvJson()
    reply_message = self.processMessage(recv_message)
    self.socket.send(reply_message)

  # KernelMessage -> KernelResponse
  def processMessage(self, msg):
    resp = {}
    if msg['type'] is 'evaluate':
      if msg['scope'] is 'Local':
        raise NotImplementedError
    elif msg['type'] is 'get_status':
      raise NotImplementedError
    elif msg['type'] is 'autocomplete':
      raise NotImplementedError

