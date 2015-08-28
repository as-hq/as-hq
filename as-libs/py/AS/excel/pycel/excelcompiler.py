from excellib import *
import excellib as excellib
from math import *
import string as string
import collections as collections
from tokenizer import ExcelParser, f_token
from networkx.classes.digraph import DiGraph

class ASTNode(object):
    """A generic node in the AST"""
    def __init__(self,token):
        super(ASTNode,self).__init__()
        self.token = token
        self.isVolatile = False
    def __str__(self):
        return self.token.tvalue
    def __getattr__(self,name):
        return getattr(self.token,name)
    def children(self,ast):
        args = ast.predecessors(self)
        args = sorted(args,key=lambda x: ast.node[x]['pos'])
        return args
    def parent(self,ast):
        args = ast.successors(self)
        return args[0] if args else None
    def emit(self,ast,context=None):
        """Emit code"""
        self.token.tvalue
    
class OperatorNode(ASTNode):
    def __init__(self,*args):
        super(OperatorNode,self).__init__(*args)
        # convert the operator to python equivalents
        self.opmap = {
                 "^":"**",
                 "=":"==",
                 "&":"+",
                 "":"+" #union
                 }
    def emit(self,ast,context=None):
        xop = self.tvalue
        # Get the arguments
        args = self.children(ast)
        op = self.opmap.get(xop,xop)
        if self.ttype == "operator-prefix":
            return "-" + args[0].emit(ast,context=context)
        parent = self.parent(ast)
        if op == "**":
            if parent and parent.tvalue.lower() == "linest": 
                return args[0].emit(ast,context=context)
        #TODO silly hack to work around the fact that None < 0 is True (happens on blank cells)
        if op == "<" or op == "<=":
            aa = args[0].emit(ast,context=context)
            ss = "(" + aa + " if " + aa + " is not None else float('inf'))" + op + args[1].emit(ast,context=context)
        elif op == ">" or op == ">=":
            aa = args[1].emit(ast,context=context)
            ss =  args[0].emit(ast,context=context) + op + "(" + aa + " if " + aa + " is not None else float('inf'))"
        else:
            ss = args[0].emit(ast,context=context) + op + args[1].emit(ast,context=context)
        #avoid needless parentheses
        if parent and not isinstance(parent,FunctionNode):
            ss = "("+ ss + ")" 
        return ss

class OperandNode(ASTNode):
    def __init__(self,*args):
        super(OperandNode,self).__init__(*args)
    def emit(self,ast,context=None):
        t = self.tsubtype
        if t == "logical":
            return str(self.tvalue.lower() == "true")
        elif t == "text" or t == "error":
            #if the string contains quotes, escape them
            val = self.tvalue.replace('"','\\"')
            return '"' + val + '"'
        else:
            return str(self.tvalue)

class RangeNode(OperandNode):
    """Represents a spreadsheet cell or range, e.g., A5 or B3:C20"""
    def __init__(self,*args):
        super(RangeNode,self).__init__(*args)
    def get_cells(self):
        return resolve_range(self.tvalue)[0]
    def emit(self,ast,context=None):
        return str(self.tvalue)
        '''
        rng = self.tvalue # .replace('$','')
        sheet = context.curcell.sheet + "!" if context else ""
        if is_range(rng):
            sh,start,end = split_range(rng)
            if sh:
                str = rng
            else:
                str =  sheet + rng 
        else:
            sh,col,row = split_address(rng)
            if sh:
                str =  rng 
            else:
                str = sheet + rng 
                
        return str
        '''
    
class FunctionNode(ASTNode):
    """AST node representing a function call"""
    def __init__(self,*args):
        super(FunctionNode,self).__init__(*args)
        self.numargs = 0
        # Map excel functions to their respective Python ones
        self.funmap = excellib.FUNCTION_MAP
        self.volatileList = excellib.VOLATILE_LIST
    def emit(self,ast,context=None):
        fun = self.tvalue.lower()
        if fun in self.volatileList:
            self.isVolatile = True
        str = ''
        args = self.children(ast)
        if fun == "atan2": # swap arguments
            str = "atan2(%s,%s)" % (args[1].emit(ast,context=context),args[0].emit(ast,context=context))
        elif fun == "pi": # constant, no parens
            str = "pi"
        elif fun == "if":  # inline the if
            if len(args) == 2:
                str = "%s if %s else 0" %(args[1].emit(ast,context=context),args[0].emit(ast,context=context))
            elif len(args) == 3:
                str = "(%s if %s else %s)" % (args[1].emit(ast,context=context),args[0].emit(ast,context=context),args[2].emit(ast,context=context))
            else:
                raise Exception("if with %s arguments not supported" % len(args))
        elif fun == "row":
            str = separate(args[0].emit(ast,context=context))[1]
        elif fun == "col":
            str = colToInt(separate(args[0].emit(ast,context=context))[0])
        elif fun == "offset":
            if (len(args)==3):
                str = offset(args[0].emit(ast,context=context),args[1].emit(ast,context=context),args[2].emit(ast,context=context))
            elif (len(args)==5):
                str = offset(args[0].emit(ast,context=context),args[1].emit(ast,context=context),args[2].emit(ast,context=context),args[3].emit(ast,context=context),args[4].emit(ast,context=context))
            else:
                raise TypeError("Incorrect number of parameters to OFFSET function")
        elif fun == "array":
            str += 'arr(['
            if len(args) == 1:
                # only one row
                str += args[0].emit(ast,context=context)
            else:
                # multiple rows
                str += ",".join(['arr([' + n.emit(ast,context=context) + '])' for n in args])       
            str += '])'
        elif fun == "arrayrow":
            #simply create a list
            str += ",".join([n.emit(ast,context=context) for n in args])
        elif fun == "linest" or fun == "linestmario":
            str = fun + "(" + ",".join([n.emit(ast,context=context) for n in args])
            if not context:
                degree,coef = -1,-1
            else:
                #linests are often used as part of an array formula spanning multiple cells,
                #one cell for each coefficient.  We have to figure out where we currently are
                #in that range
                degree,coef = get_linest_degree(context.excel,context.curcell)
            # if we are the only linest (degree is one) and linest is nested -> return vector
            # else return the coef.
            if degree == 1 and self.parent(ast):
                if fun == "linest":
                    str += ",degree=%s)" % degree
                else:
                    str += ")"
            else:
                if fun == "linest":
                    str += ",degree=%s)[%s]" % (degree,coef-1)
                else:
                    str += ")[%s]" % (coef-1)
        elif fun == "and":
            str = "all([" + ",".join([n.emit(ast,context=context) for n in args]) + "])"
        elif fun == "or":
            str = "any([" + ",".join([n.emit(ast,context=context) for n in args]) + "])"
        else:
            # map to the correct name
            f = self.funmap.get(fun,fun)
            str = f + "(" + ",".join([n.emit(ast,context=context) for n in args]) + ")"

        return str

#-------------------------------------------------------------------------------------------------
# Pre-Eval substitutions

# separate AA33 into (AA,33) 
def separate(s):
    for i,c in enumerate(s):
        if c.isdigit():
            if i==0:
                return ValueError("Invalid Excel Reference: "+s)
            return (s[0:i],s[i:len(s)])
    return ValueError("Invalid Excel Reference: "+s)

# convert an excel column like AB to an integer
def colToInt(col):
    num = 0
    for c in col:
        if c in string.ascii_letters:
            num = num * 26 + (ord(c.upper()) - ord('A')) + 1
    return num

# inverse of colToInt
def intToCol(col):
    letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    result = []
    while col:
        col, rem = divmod(col-1, 26)
        result[:0] = letters[rem]
    return ''.join(result) 

# dealing with OFFSET in excel
def offset(ref,row,col,numRow=1,numCol=1):
    (c,r)=separate(ref)
    if (numRow<1) or (numCol<1):
        raise ValueError("The dimensions of an OFFSET must be positive")
    topLeft = (intToCol(colToInt(c)+int(col)),str(int(r)+int(row)))
    bottomRight = (intToCol(colToInt(topLeft[0])+int(numCol)-1),str(int(topLeft[1])+int(numRow)-1))
    if (numRow==1) and (numCol==1):
        return topLeft[0]+topLeft[1]
    else:
        return topLeft[0]+topLeft[1]+":"+bottomRight[0]+bottomRight[1]

#-------------------------------------------------------------------------------------------------

def create_node(t):
    """Simple factory function"""
    if t.ttype == "operand":
        if t.tsubtype == "range":
            return RangeNode(t)
        else:
            return OperandNode(t)
    elif t.ttype == "function":
        return FunctionNode(t)
    elif t.ttype.startswith("operator"):
        return OperatorNode(t)
    else:
        return ASTNode(t)

class Operator:
    """Small wrapper class to manage operators during shunting yard"""
    def __init__(self,value,precedence,associativity):
        self.value = value
        self.precedence = precedence
        self.associativity = associativity

def shunting_yard(expression):
    """
    Tokenize an excel formula expression into reverse polish notation
    
    Core algorithm taken from wikipedia with varargs extensions from
    http://www.kallisti.net.nz/blog/2008/02/extension-to-the-shunting-yard-algorithm-to-allow-variable-numbers-of-arguments-to-functions/
    """
    #remove leading =
    if expression.startswith('='):
        expression = expression[1:]
        
    p = ExcelParser();
    p.parse(expression)

    # insert tokens for '(' and ')', to make things clearer below
    tokens = []
    for t in p.tokens.items:
        if t.ttype == "function" and t.tsubtype == "start":
            t.tsubtype = ""
            tokens.append(t)
            tokens.append(f_token('(','arglist','start'))
        elif t.ttype == "function" and t.tsubtype == "stop":
            tokens.append(f_token(')','arglist','stop'))
        elif t.ttype == "subexpression" and t.tsubtype == "start":
            t.tvalue = '('
            tokens.append(t)
        elif t.ttype == "subexpression" and t.tsubtype == "stop":
            t.tvalue = ')'
            tokens.append(t)
        else:
            tokens.append(t)

    #print "tokens: ", "|".join([x.tvalue for x in tokens])

    #http://office.microsoft.com/en-us/excel-help/calculation-operators-and-precedence-HP010078886.aspx
    operators = {}
    operators[':'] = Operator(':',8,'left')
    operators[''] = Operator(' ',8,'left')
    operators[','] = Operator(',',8,'left')
    operators['u-'] = Operator('u-',7,'left') #unary negation
    operators['%'] = Operator('%',6,'left')
    operators['^'] = Operator('^',5,'left')
    operators['*'] = Operator('*',4,'left')
    operators['/'] = Operator('/',4,'left')
    operators['+'] = Operator('+',3,'left')
    operators['-'] = Operator('-',3,'left')
    operators['&'] = Operator('&',2,'left')
    operators['='] = Operator('=',1,'left')
    operators['<'] = Operator('<',1,'left')
    operators['>'] = Operator('>',1,'left')
    operators['<='] = Operator('<=',1,'left')
    operators['>='] = Operator('>=',1,'left')
    operators['<>'] = Operator('<>',1,'left')
            
    output = collections.deque()
    stack = []
    were_values = []
    arg_count = []
    
    for t in tokens:
        if t.ttype == "operand":

            output.append(create_node(t))
            if were_values:
                were_values.pop()
                were_values.append(True)
                
        elif t.ttype == "function":

            stack.append(t)
            arg_count.append(0)
            if were_values:
                were_values.pop()
                were_values.append(True)
            were_values.append(False)
            
        elif t.ttype == "argument":
            
            while stack and (stack[-1].tsubtype != "start"):
                output.append(create_node(stack.pop()))   
            
            if were_values.pop(): arg_count[-1] += 1
            were_values.append(False)
            
            if not len(stack):
                raise Exception("Mismatched or misplaced parentheses")
        
        elif t.ttype.startswith('operator'):

            if t.ttype.endswith('-prefix') and t.tvalue =="-":
                o1 = operators['u-']
            else:
                o1 = operators[t.tvalue]

            while stack and stack[-1].ttype.startswith('operator'):
                
                if stack[-1].ttype.endswith('-prefix') and stack[-1].tvalue =="-":
                    o2 = operators['u-']
                else:
                    o2 = operators[stack[-1].tvalue]
                
                if ( (o1.associativity == "left" and o1.precedence <= o2.precedence)
                        or
                      (o1.associativity == "right" and o1.precedence < o2.precedence) ):
                    
                    output.append(create_node(stack.pop()))
                else:
                    break
                
            stack.append(t)
        
        elif t.tsubtype == "start":
            stack.append(t)
            
        elif t.tsubtype == "stop":
            
            while stack and stack[-1].tsubtype != "start":
                output.append(create_node(stack.pop()))
            
            if not stack:
                raise Exception("Mismatched or misplaced parentheses")
            
            stack.pop()

            if stack and stack[-1].ttype == "function":
                f = create_node(stack.pop())
                a = arg_count.pop()
                w = were_values.pop()
                if w: a += 1
                f.num_args = a
                #print f, "has ",a," args"
                output.append(f)

    while stack:
        if stack[-1].tsubtype == "start" or stack[-1].tsubtype == "stop":
            raise Exception("Mismatched or misplaced parentheses")
        
        output.append(create_node(stack.pop()))

    #print "Stack is: ", "|".join(stack)
    #print "Ouput is: ", "|".join([x.tvalue for x in output])
    
    # convert to list
    result = [x for x in output]
    return result
   
def build_ast(expression):
    """build an AST from an Excel formula expression in reverse polish notation"""
    
    #use a directed graph to store the tree
    G = DiGraph()
    
    stack = []
    
    for n in expression:
        # Since the graph does not maintain the order of adding nodes/edges
        # add an extra attribute 'pos' so we can always sort to the correct order
        if isinstance(n,OperatorNode):
            if n.ttype == "operator-infix":
                arg2 = stack.pop()
                arg1 = stack.pop()
                G.add_node(arg1,{'pos':1})
                G.add_node(arg2,{'pos':2})
                G.add_edge(arg1, n)
                G.add_edge(arg2, n)
            else:
                arg1 = stack.pop()
                G.add_node(arg1,{'pos':1})
                G.add_edge(arg1, n)
                
        elif isinstance(n,FunctionNode):
            args = [stack.pop() for _ in range(n.num_args)]
            args.reverse()
            G.add_node(n) # Needed so that functions with no args have a function node in the AST
            for i,a in enumerate(args):
                G.add_node(a,{'pos':i})
                G.add_edge(a,n)
            #for i in range(n.num_args):
            #    G.add_edge(stack.pop(),n)
        else:
            G.add_node(n,{'pos':0})

        stack.append(n)
        
    return G,stack.pop()

class Context(object):
    """A small context object that nodes in the AST can use to emit code"""
    def __init__(self,curcell,excel):
        # the current cell for which we are generating code
        self.curcell = curcell
        # a handle to an excel instance
        self.excel = excel



