from __future__ import print_function
import sys

from traitlets import Instance, Type
from IPython.core.displayhook import DisplayHook

class ASDisplayHook(DisplayHook):
    """A custom displayhook to replace sys.displayhook.

    This class does many things, but the basic idea is that it is a callable
    that gets called anytime user code returns a value.
    """
    exec_result = Instance('AS.kernel.shell.ASExecutionResult',
                           allow_none=True)

    def __init__(self, shell=None, cache_size=1000, silent=False, **kwargs):
      super(ASDisplayHook, self).__init__(shell, cache_size, **kwargs)
      self.silent = silent

    def write_output_prompt(self):
      if self.silent:
        return
      else:
        super(ASDisplayHook, self).write_output_prompt()

    def write_format_data(self):
      if self.silent:
        return
      else:
        super(ASDisplayHook, self).write_format_data()