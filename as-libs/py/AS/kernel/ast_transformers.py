from __future__ import print_function
import ast
import sys

def wrap_serialize(node):
    ''' 
    creates a new AST node by wrapping the value of an ast.Expr object in 
    a call to AS.stdlib.serialize
    this is so that we can execute serialize() in the origin namespace,
    which is necessary for cPickle pickling of classes 
    '''
    # print(ast.dump(node), file=sys.__stdout__)
    if isinstance(node, ast.Expr):
        # 'serialize' is expected to exist in the origin namespace
        call = ast.Call(func=ast.Name(id='wrap_value', ctx=ast.Load()),
                        args=[node.value],
                        keywords=[])
        newnode = ast.Expr(value=call)
        newnode = ast.copy_location(newnode, node)
        ast.fix_missing_locations(newnode)
        return newnode
    else:
        # can't wrap non-Expr node
        return node