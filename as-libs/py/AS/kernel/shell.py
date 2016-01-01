import sys
import ast
import copy

from IPython.core.interactiveshell import InteractiveShell, ExecutionResult, softspace
from IPython.core.prompts import PromptManager
from IPython.utils.py3compat import builtin_mod

class ASShell(InteractiveShell):

#-----------------------------------------------------------------------------
#  Single initialization
#-----------------------------------------------------------------------------

  def init_create_namespaces(self, user_module=None, user_ns=None):

        # Create the namespace where the user will operate.  user_ns is
        # normally the only one used, and it is passed to the exec calls as
        # the locals argument.  But we do carry a user_global_ns namespace
        # given as the exec 'globals' argument,  This is useful in embedding
        # situations where the ipython shell opens in a context where the
        # distinction between locals and globals is meaningful.  For
        # non-embedded contexts, it is just the same object as the user_ns dict.
        if (user_ns is not None) or (user_module is not None):
            self.default_user_namespaces = False
        self.user_module, self.user_ns = self.prepare_user_module(user_module, user_ns)

        # A record of hidden variables we have added to the user namespace, so
        # we can list later only variables defined in actual interactive use.
        self.user_ns_hidden = {}

        # This is the cache used for 'main' namespaces
        self._main_mod_cache = {}

        # The set of namespaces corresponding to sheets
        self.sheet_nss = {}

        # A table holding all the namespaces IPython deals with, so that
        # introspection facilities can search easily.
        self.ns_table = {'user_global':self.user_module.__dict__,
                         'user_local':self.user_ns,
                         'builtin':builtin_mod.__dict__
                         }

  def init_prompts(self):
    self.prompt_manager = PromptManager(shell=self, parent=self)
    self.configurables.append(self.prompt_manager)

#-----------------------------------------------------------------------------
#  Run-time initialization
#-----------------------------------------------------------------------------

  def init_sheet_ns(self, sheet_id):
    self.sheet_nss[sheet_id] = copy.copy(self.user_global_ns)

#-----------------------------------------------------------------------------
#  Evaluation
#-----------------------------------------------------------------------------
  
  def run_header(self, raw_header, sheet_id):
    # self.init_sheet_ns(sheet_id)
    return self.run_block(raw_header, sheet_id, isolated=False)

  def run_cell(self, raw_cell, sheet_id):
    return self.run_block(raw_cell, sheet_id, isolated=True)

  def run_block(self, raw_cell, sheet_id, store_history=False, silent=False, shell_futures=True, isolated=False):
    """
    Run a complete code block.

    Parameters
    ----------
    sheet_id: str
      The namespace id to run the code against.
    raw_cell : str
      The code (including IPython code such as %magic functions) to run.
    store_history : bool
      If True, the raw and translated cell will be stored in IPython's
      history. For user code calling back into IPython's machinery, this
      should be set to False.
    silent : bool
      If True, avoid side-effects, such as implicit displayhooks and
      and logging.  silent=True forces store_history=False.
    shell_futures : bool
      If True, the code will share future statements with the interactive
      shell. It will both be affected by previous __future__ imports, and
      any __future__ imports in the code will affect the shell. If False,
      __future__ imports are not shared in either direction.
    isolated: bool
      If True, run code in the user namespace without adding to it.

    Returns
    -------
    result : :class:`ExecutionResult`
    """
    result = ExecutionResult()

    # initialize the sheet if it doesn't exist
    # needsrefactor this should be called eagerly (e.g. on sheet open) instead of lazily (on eval)
    if not (sheet_id in self.sheet_nss):
      self.init_sheet_ns(sheet_id)

    if (not raw_cell) or raw_cell.isspace():
        return result
    
    if silent or isolated:
        store_history = False

    if store_history:
        result.execution_count = self.execution_count

    def error_before_exec(value):
        result.error_before_exec = value
        return result

    self.events.trigger('pre_execute')
    if not silent:
        self.events.trigger('pre_run_cell')

    # If any of our input transformation (input_transformer_manager or
    # prefilter_manager) raises an exception, we store it in this variable
    # so that we can display the error after logging the input and storing
    # it in the history.
    preprocessing_exc_tuple = None
    try:
        # Static input transformations
        cell = self.input_transformer_manager.transform_cell(raw_cell)
    except SyntaxError:
        preprocessing_exc_tuple = sys.exc_info()
        cell = raw_cell  # cell has to exist so it can be stored/logged
    else:
        if len(cell.splitlines()) == 1:
            # Dynamic transformations - only applied for single line commands
            with self.builtin_trap:
                try:
                    # use prefilter_lines to handle trailing newlines
                    # restore trailing newline for ast.parse
                    cell = self.prefilter_manager.prefilter_lines(cell) + '\n'
                except Exception:
                    # don't allow prefilter errors to crash IPython
                    preprocessing_exc_tuple = sys.exc_info()

    # Store raw and processed history
    if store_history:
        self.history_manager.store_inputs(self.execution_count,
                                          cell, raw_cell)
    if not silent:
        self.logger.log(cell, raw_cell)

    # Display the exception if input processing failed.
    if preprocessing_exc_tuple is not None:
        print "GOT HERE"
        self.showtraceback(preprocessing_exc_tuple)
        if store_history:
            self.execution_count += 1
        return error_before_exec(preprocessing_exc_tuple[2])

    # Our own compiler remembers the __future__ environment. If we want to
    # run code with a separate __future__ environment, use the default
    # compiler
    compiler = self.compile if shell_futures else CachingCompiler()

    with self.builtin_trap:
        cell_name = self.compile.cache(cell, self.execution_count)

        with self.display_trap:
            # Compile to bytecode
            try:
                code_ast = compiler.ast_parse(cell, filename=cell_name)
            except IndentationError as e:
                self.showindentationerror()
                if store_history:
                    self.execution_count += 1
                return error_before_exec(e)
            except (OverflowError, SyntaxError, ValueError, TypeError,
                    MemoryError) as e:
                self.showsyntaxerror()
                if store_history:
                    self.execution_count += 1
                return error_before_exec(e)

            # Apply AST transformations
            try:
                code_ast = self.transform_ast(code_ast)
            except InputRejected as e:
                self.showtraceback()
                if store_history:
                    self.execution_count += 1
                return error_before_exec(e)

            # Give the displayhook a reference to our ExecutionResult so it
            # can fill in the output value.
            self.displayhook.exec_result = result

            # Execute the user code
            interactivity = "none" if silent else self.ast_node_interactivity
            self.run_ast_nodes(code_ast.body, 
                                cell_name,
                                sheet_id,
                                interactivity=interactivity, 
                                compiler=compiler, 
                                result=result,
                                isolated=isolated)

            # Reset this so later displayed values do not modify the
            # ExecutionResult
            self.displayhook.exec_result = None

            self.events.trigger('post_execute')
            if not silent:
                self.events.trigger('post_run_cell')

    if store_history:
        # Write output to the database. Does nothing unless
        # history output logging is enabled.
        self.history_manager.store_output(self.execution_count)
        # Each cell is a *single* input, regardless of how many lines it has
        self.execution_count += 1

    return result

  def run_ast_nodes(self, nodelist, cell_name, sheet_id, interactivity='last_expr',
                    compiler=compile, result=None, isolated=False):
    """Run a sequence of AST nodes. The execution mode depends on the
    interactivity parameter.

    Parameters
    ----------
    nodelist : list
      A sequence of AST nodes to run.
    cell_name : str
      Will be passed to the compiler as the filename of the cell. Typically
      the value returned by ip.compile.cache(cell).
    sheet_id : str
      The namespace to run the cell in.
    interactivity : str
      'all', 'last', 'last_expr' or 'none', specifying which nodes should be
      run interactively (displaying output from expressions). 'last_expr'
      will run the last node interactively only if it is an expression (i.e.
      expressions in loops or other blocks are not displayed. Other values
      for this parameter will raise a ValueError.
    compiler : callable
      A function with the same interface as the built-in compile(), to turn
      the AST nodes into code objects. Default is the built-in compile().
    result : ExecutionResult, optional
      An object to store exceptions that occur during execution.

    Returns
    -------
    True if an exception occurred while running code, False if it finished
    running.
    """
    if not nodelist:
        return

    if interactivity == 'last_expr':
        if isinstance(nodelist[-1], ast.Expr):
            interactivity = "last"
        else:
            interactivity = "none"

    if interactivity == 'none':
        to_run_exec, to_run_interactive = nodelist, []
    elif interactivity == 'last':
        to_run_exec, to_run_interactive = nodelist[:-1], nodelist[-1:]
    elif interactivity == 'all':
        to_run_exec, to_run_interactive = [], nodelist
    else:
        raise ValueError("Interactivity was %r" % interactivity)

    try:
      # the target namespace builds up for every line of cell code, 
      # and is initially empty if run_cell is specified as running in isolated mode.
        sheet_ns = self.sheet_nss[sheet_id]
        target_ns = {} if isolated else sheet_ns
        source_ns = sheet_ns
        for i, node in enumerate(to_run_exec):
            mod = ast.Module([node])
            code = compiler(mod, cell_name, "exec")
            if self.run_code(code, source_ns, target_ns, result):
                return True

        for i, node in enumerate(to_run_interactive):
            mod = ast.Interactive([node])
            code = compiler(mod, cell_name, "single")
            if self.run_code(code, source_ns, target_ns, result):
                return True
        if not isolated:
          # add the newly created variables back into the sheet namespace
          self.sheet_nss[sheet_id] = target_ns
        # Flush softspace
        if softspace(sys.stdout, 0):
            print()

    except:
        # It's possible to have exceptions raised here, typically by
        # compilation of odd code (such as a naked 'return' outside a
        # function) that did parse but isn't valid. Typically the exception
        # is a SyntaxError, but it's safest just to catch anything and show
        # the user a traceback.

        # We do only one try/except outside the loop to minimize the impact
        # on runtime, and also because if any node in the node list is
        # broken, we should stop execution completely.
        print "GOT HERE"
        if result:
            result.error_before_exec = sys.exc_info()[1]
        self.showtraceback()
        return True

    return False

  def run_code(self, code_obj, source_ns, target_ns, result=None):
    """Execute a code object.

    When an exception occurs, self.showtraceback() is called to display a
    traceback.

    Parameters
    ----------
    code_obj : code object
      A compiled code object, to be executed
    source_ns : dict
      the source namespace to run exec() against
    target_ns : dict
      the target namespace to inject newly created variables into
    result : ExecutionResult, optional
      An object to store exceptions that occur during execution.

    Returns
    -------
    False : successful execution.
    True : an error occurred.
    """
    # Set our own excepthook in case the user code tries to call it
    # directly, so that the IPython crash handler doesn't get triggered
    old_excepthook, sys.excepthook = sys.excepthook, self.excepthook

    # we save the original sys.excepthook in the instance, in case config
    # code (such as magics) needs access to it.
    self.sys_excepthook = old_excepthook
    outflag = 1  # happens in more places, so it's easier as default
    try:
        try:
            self.hooks.pre_run_code_hook()
            exec(code_obj, source_ns, target_ns)
        finally:
            # Reset our crash handler in place
            sys.excepthook = old_excepthook
    except SystemExit as e:
        if result is not None:
            result.error_in_exec = e
        self.showtraceback(exception_only=True)
        warn("To exit: use 'exit', 'quit', or Ctrl-D.", level=1)
    except self.custom_exceptions:
        etype, value, tb = sys.exc_info()
        if result is not None:
            result.error_in_exec = value
        self.CustomTB(etype, value, tb)
    except:
        print "GOT HERE"
        if result is not None:
            result.error_in_exec = sys.exc_info()[1]
        self.showtraceback()
    else:
        outflag = 0
    return outflag

def get_formatted_traceback(self, exc_tuple=None, filename=None, tb_offset=None,
                exception_only=False):
  """Display the exception that just occurred.

  If nothing is known about the exception, this is the method which
  should be used throughout the code for presenting user tracebacks,
  rather than directly invoking the InteractiveTB object.

  A specific showsyntaxerror() also exists, but this method can take
  care of calling it if needed, so unless you are explicitly catching a
  SyntaxError exception, don't try to analyze the stack manually and
  simply call this method."""

  try:
      stb = None
      # catch exceptions while trying to get the exception
      try:
          etype, value, tb = self._get_exc_info(exc_tuple)
      except ValueError:
          return 'No traceback available to show.'
      
      if issubclass(etype, SyntaxError):
          # Though this won't be called by syntax errors in the input
          # line, there may be SyntaxError cases with imported code.
          stb = self.SyntaxTB.structured_traceback(etype, value, [])
      elif etype is UsageError:
          self.show_usage_error(value)
      else:
          if exception_only:
              stb = ['An exception has occurred, use %tb to see '
                     'the full traceback.\n']
              stb.extend(self.InteractiveTB.get_exception_only(etype,
                                                               value))
          else:
              try:
                  # Exception classes can customise their traceback - we
                  # use this in IPython.parallel for exceptions occurring
                  # in the engines. This should return a list of strings.
                  stb = value._render_traceback_()
              except Exception:
                  stb = self.InteractiveTB.structured_traceback(etype,
                                      value, tb, tb_offset=tb_offset)

          return self.InteractiveTB.stb2text(stb)

  except KeyboardInterrupt:
      self.write_err('\n' + self.get_exception_only())
