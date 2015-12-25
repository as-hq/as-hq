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
from ipykernel.zmqshell import ZMQInteractiveShell

#-----------------------------------------------------------------------------
#  Shell
#-----------------------------------------------------------------------------

import os
import atexit
from pickleshare import PickleShareDB
from traitlets.config.configurable import SingletonConfigurable
from IPython.core.interactiveshell import InteractiveShell
from IPython.utils import py3compat

class ASShell(InteractiveShell):
    def __init__(self, ipython_dir=None, profile_dir=None,
    			user_module=None, user_ns=None,
    			custom_exceptions=((), None), **kwargs):

        # This is where traits with a config_key argument are updated
        # from the values on config.
        SingletonConfigurable.__init__(self, **kwargs)
        # super(InteractiveShell, self).__init__(**kwargs)
        self.configurables = [self]

        # These are relatively independent and stateless
        self.init_ipython_dir(ipython_dir)
        self.init_profile_dir(profile_dir)
        self.init_instance_attrs()
        self.init_environment()
        
        # Check if we're in a virtualenv, and set up sys.path.
        self.init_virtualenv()

        # Create namespaces (user_ns, user_global_ns, etc.)
        self.init_create_namespaces(user_module, user_ns)
        # This has to be done after init_create_namespaces because it uses
        # something in self.user_ns, but before init_sys_modules, which
        # is the first thing to modify sys.
        # TODO: When we override sys.stdout and sys.stderr before this class
        # is created, we are saving the overridden ones here. Not sure if this
        # is what we want to do.
        self.save_sys_module_state()
        self.init_sys_modules()

        # While we're trying to have each part of the code directly access what
        # it needs without keeping redundant references to objects, we have too
        # much legacy code that expects ip.db to exist.
        self.db = PickleShareDB(os.path.join(self.profile_dir.location, 'db'))

        self.init_history()
        self.init_encoding()
        self.init_prefilter()

        self.init_syntax_highlighting()
        self.init_hooks()
        self.init_events()
        self.init_pushd_popd_magic()
        # self.init_traceback_handlers use to be here, but we moved it below
        # because it and init_io have to come after init_readline.
        self.init_user_ns()
        self.init_logger()
        self.init_builtins()

        # The following was in post_config_initialization
        self.init_inspector()
        # init_readline() must come before init_io(), because init_io uses
        # readline related things.
        self.init_readline()
        # We save this here in case user code replaces raw_input, but it needs
        # to be after init_readline(), because PyPy's readline works by replacing
        # raw_input.
        if py3compat.PY3:
            self.raw_input_original = input
        else:
            self.raw_input_original = raw_input
        # init_completer must come after init_readline, because it needs to
        # know whether readline is present or not system-wide to configure the
        # completers, since the completion machinery can now operate
        # independently of readline (e.g. over the network)
        self.init_completer()
        # TODO: init_io() needs to happen before init_traceback handlers
        # because the traceback handlers hardcode the stdout/stderr streams.
        # This logic in in debugger.Pdb and should eventually be changed.
        self.init_io()
        self.init_traceback_handlers(custom_exceptions)
        self.init_prompts()
        self.init_display_formatter()
        self.init_display_pub()
        self.init_data_pub()
        self.init_displayhook()
        self.init_magics()
        self.init_alias()
        self.init_logstart()
        self.init_pdb()
        self.init_extension_manager()
        self.init_payload()
        self.init_deprecation_warnings()
        self.hooks.late_startup_hook()
        self.events.trigger('shell_initialized', self)
        atexit.register(self.atexit_operations)


