from __future__ import print_function
import sys

from traitlets import Instance, Type
from IPython.core.displayhook import DisplayHook

class ASDisplayHook(DisplayHook):
    """A custom displayhook to replace sys.displayhook.

    This class does many things, but the basic idea is that it is a callable
    that gets called anytime user code returns a value.
    """

    # This stores the most recent execution result, which is used 
    # when the last line of user code is an evaluable expression.
    exec_result = None

    def __init__(self, shell=None, cache_size=1000, silent=False, **kwargs):
      super(ASDisplayHook, self).__init__(shell, cache_size, **kwargs)
      self.silent = silent

    def fill_exec_result(self, result):
      self.exec_result = result

    def write_output_prompt(self):
      if self.silent:
        return
      else:
        super(ASDisplayHook, self).write_output_prompt()

    def write_format_data(self, format_dict, md_dict=None):
      if self.silent:
        return
      else:
        super(ASDisplayHook, self).write_format_data(format_dict, md_dict)