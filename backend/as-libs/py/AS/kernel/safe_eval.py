### Adapted from http://code.activestate.com/recipes/496746-restricted-safe-eval/  

import inspect, ast
import thread, time

EVAL_TIMEOUT = 10
#----------------------------------------------------------------------
# Module globals.
#----------------------------------------------------------------------

# List of all AST node classes in ast.py.
all_ast_nodes = \
    [name for (name, obj) in inspect.getmembers(ast)
     if inspect.isclass(obj) and issubclass(obj, ast.AST)]

# List of all builtin functions and types (ignoring exception classes).
all_builtins = \
    [name for (name, obj) in inspect.getmembers(__builtins__)
     if inspect.isbuiltin(obj) or (inspect.isclass(obj) and \
                                   not issubclass(obj, Exception))]

#----------------------------------------------------------------------
# Utilties.
#----------------------------------------------------------------------

def classname(obj):
    return obj.__class__.__name__

def is_valid_ast_node(name):
    return name in all_ast_nodes

def get_node_lineno(node):
    return (node.lineno) and node.lineno or 0
       
#----------------------------------------------------------------------
# Restricted AST nodes & builtins.
#----------------------------------------------------------------------

# Deny evaluation of code if the AST contain any of the following nodes:
unallowed_ast_nodes = [
#   'Add', 'And',
#   'AssAttr', 'AssList', 'AssName', 'AssTuple',
#   'Assert', 'Assign', 'AugAssign',
#   'Backquote',
#   'Bitand', 'Bitor', 'Bitxor', 'Break',
#   'CallFunc', 'Class', 'Compare', 'Const', 'Continue',
#   'Decorators', 'Dict', 'Discard', 'Div',
#   'Ellipsis', 'EmptyNode',
    'Exec',
#   'Expression', 'FloorDiv',
#   'For',
#   'From',
#   'Function',
#   'GenExpr', 'GenExprFor', 'GenExprIf', 'GenExprInner',
    # 'Getattr', 
    'Global', 
#   'If',
#   'Import',
#   'Invert',
#   'Keyword', 'Lambda', 'LeftShift',
#   'List', 'ListComp', 'ListCompFor', 'ListCompIf', 'Mod',
#   'Module',
#   'Mul', 'Name', 'Node', 'Not', 'Or', 'Pass', 'Power',
#   'Print', 'Printnl',
    'Raise',
#    'Return', 'RightShift', 'Slice', 'Sliceobj',
#   'Stmt', 'Sub', 'Subscript',
    'TryExcept', 'TryFinally',
#   'Tuple', 'UnaryAdd', 'UnarySub',
#   'While','Yield'
]

# Deny evaluation of code if it tries to access any of the following builtins:
unallowed_builtins = [
#   'abs', 'apply', 'basestring', 'bool', 'buffer',
#   'callable', 'chr', 'classmethod', 'cmp', 'coerce',
    'compile',
#   'complex',
    'delattr',
#   'dict',
    'dir',
#   'divmod', 'enumerate',
    'eval', 'execfile', 'file',
#   'filter', 'float', 'frozenset',
    'getattr', 'globals', 'hasattr',
#    'hash', 'hex', 'id',
    'input',
#   'int', 'intern', 'isinstance', 'issubclass', 'iter',
#   'len', 'list',
    'locals',
#   'long', 'map', 'max', 'min', 'object', 'oct',
    'open',
#   'ord', 'pow', 'property', 'range',
    'raw_input',
#   'reduce',
    'reload',
#   'repr', 'reversed', 'round', 'set',
    'setattr',
#   'slice', 'sorted', 'staticmethod',  'str', 'sum', 'super',
#   'tuple', 'type', 'unichr', 'unicode',
    'vars',
#    'xrange', 'zip'
]

unallowed_imports = [
    'os',
    'sys',
    'subprocess',
    'docker',
    'threading',
    'multiprocessing',
    'cStringIO',
    'ast',
    'inspect',
    'compiler',
    'time'
]

# verify validity of restrictions.
for ast_name in unallowed_ast_nodes:
    assert(is_valid_ast_node(ast_name))

def is_unallowed_ast_node(kind):
    return kind in unallowed_ast_nodes

def is_unallowed_builtin(name):
    return name in unallowed_builtins

#----------------------------------------------------------------------
# Restricted attributes.
#----------------------------------------------------------------------

# In addition to these we deny access to all lowlevel attrs (__xxx__).
unallowed_attr = [
    'im_class', 'im_func', 'im_self',
    'func_code', 'func_defaults', 'func_globals', 'func_name',
    'tb_frame', 'tb_next',
    'f_back', 'f_builtins', 'f_code', 'f_exc_traceback',
    'f_exc_type', 'f_exc_value', 'f_globals', 'f_locals',
    'exit'
    ]

unallowed_attr.extend(unallowed_imports)

def is_unallowed_attr(name):
    return (name[:2] == '__' and name[-2:] == '__') or \
           (name in unallowed_attr)

def is_unallowed_import(name):
    return name in unallowed_imports

#----------------------------------------------------------------------
# Safe eval exceptions
#----------------------------------------------------------------------

class SafeEvalError(Exception):
    """
    Base class for all which occur while walking the AST.

    Attributes:
      errmsg = short decription about the nature of the error
      lineno = line offset to where error occured in source code
    """
    def __init__(self, errmsg, lineno):
        self.errmsg, self.lineno = errmsg, lineno
    def __str__(self):
        return "line %d : %s" % (self.lineno, self.errmsg)

class SafeEvalASTNodeError(SafeEvalError):
    "Expression/statement in AST evaluates to a restricted AST node type."
    pass
class SafeEvalBuiltinError(SafeEvalError):
    "Expression/statement in tried to access a restricted builtin."
    pass
class SafeEvalAttrError(SafeEvalError):
    "Expression/statement in tried to access a restricted attribute."
    pass

class SafeEvalImportError(SafeEvalError):
    "Restricted import detected in expression."
    pass

#----------------------------------------------------------------------
# Safe eval exceptions
#----------------------------------------------------------------------

class SafeEvalVisitor(ast.NodeVisitor):
    """
    Data-driven visitor which walks the AST for some code and makes
    sure it doesn't contain any expression/statements which are
    declared as restricted in 'unallowed_ast_nodes'. We'll also make
    sure that there aren't any attempts to access/lookup restricted
    builtin declared in 'unallowed_builtins'. By default we also won't
    allow access to lowlevel stuff which can be used to dynamically
    access non-local envrioments.
    """

    def __init__(self):
        # Set failure callbacks for disallowed AST node types. 
        for ast_name in all_ast_nodes:
            # Don't reset any overridden callbacks.
            if getattr(self, 'visit_' + ast_name, None): continue
            if is_unallowed_ast_node(ast_name):
                setattr(self, 'visit_' + ast_name, self.fail)

    def visit_Import(self, node):
        for alias in node.names:
            if is_unallowed_import(alias.name):
                lineno = get_node_lineno(node)
                raise SafeEvalImportError( \
                    "restricted import: '%s'" % alias.name, lineno)

    def visit_ImportFrom(self, node):
        if is_unallowed_import(node.module):
            lineno = get_node_lineno(node)
            raise SafeEvalImportError( \
                "import '%s' denied" % node.module, lineno)

    def visit_Name(self, node):
        "Disallow any attempts to access a restricted builtin/attr."
        name = node.id
        lineno = get_node_lineno(node)
        if is_unallowed_builtin(name):
            raise SafeEvalBuiltinError( \
                "access to builtin '%s' is denied" % name, lineno)
        elif is_unallowed_attr(name):
            raise SafeEvalAttrError( \
                "access to attribute '%s' is denied" % name, lineno)
               
    def visit_Getattr(self, node, *args):
        "Disallow any attempts to access a restricted attribute."
        name = node.attrname
        lineno = get_node_lineno(node)
        if is_unallowed_attr(name):
            raise SafeEvalAttrError( \
                "access to attribute '%s' is denied" % name, lineno)
            
    def fail(self, node):
        "Default callback for unallowed AST nodes."
        lineno = get_node_lineno(node)
        raise SafeEvalASTNodeError( \
            "execution of '%s' statements is denied" % classname(node),
            lineno)

#----------------------------------------------------------------------
# Safe 'eval' replacement.
#----------------------------------------------------------------------

class SafeEvalException(Exception):
    "Base class for all safe-eval related errors."
    pass

class SafeEvalTimeoutException(SafeEvalException):
    """
    Exception class for reporting that code evaluation execeeded
    the given timelimit.

    Attributes:
      timeout = time limit in seconds
    """
    def __init__(self, timeout):
        self.timeout = timeout
    def __str__(self):
        return "Timeout limit execeeded (%s secs) during exec" % self.timeout

def exec_timed(code, context, timeout_secs=EVAL_TIMEOUT):
    """
    Dynamically execute 'code' using 'context' as the global enviroment.
    SafeEvalTimeoutException is raised if execution does not finish within
    the given timelimit.
    """
    if EVAL_TIMEOUT < 0:
        exec code in context
        return

    signal_finished = False
    
    def alarm(secs):
        def wait(secs):
            for n in xrange(timeout_secs):
                time.sleep(1)
                if signal_finished: break
            else:
                thread.interrupt_main()
        thread.start_new_thread(wait, (secs,))

    try:
        alarm(timeout_secs)
        exec code in context
        signal_finished = True
    except KeyboardInterrupt:
        raise SafeEvalTimeoutException(timeout_secs)

