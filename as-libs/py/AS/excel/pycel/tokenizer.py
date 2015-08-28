#========================================================================
# Description: Tokenise an Excel formula using an implementation of
#              E. W. Bachtal's algorithm, found here:
#
# http://ewbi.blogs.com/develops/2004/12/excel_formula_p.html
#
#========================================================================

import re
import collections

#========================================================================
# Class: ExcelParserTokens
# Description: Inheritable container for token definitions
#
# Attributes: Self explanatory
#
# Methods: None
#========================================================================
class ExcelParserTokens:
    TOK_TYPE_NOOP           = "noop";
    TOK_TYPE_OPERAND        = "operand";
    TOK_TYPE_FUNCTION       = "function";
    TOK_TYPE_SUBEXPR        = "subexpression";
    TOK_TYPE_ARGUMENT       = "argument";
    TOK_TYPE_OP_PRE         = "operator-prefix";
    TOK_TYPE_OP_IN          = "operator-infix";
    TOK_TYPE_OP_POST        = "operator-postfix";
    TOK_TYPE_WSPACE         = "white-space";
    TOK_TYPE_UNKNOWN        = "unknown"
    
    TOK_SUBTYPE_START       = "start";
    TOK_SUBTYPE_STOP        = "stop";
    TOK_SUBTYPE_TEXT        = "text";
    TOK_SUBTYPE_NUMBER      = "number";
    TOK_SUBTYPE_LOGICAL     = "logical";
    TOK_SUBTYPE_ERROR       = "error";
    TOK_SUBTYPE_RANGE       = "range";
    TOK_SUBTYPE_MATH        = "math";
    TOK_SUBTYPE_CONCAT      = "concatenate";
    TOK_SUBTYPE_INTERSECT   = "intersect";
    TOK_SUBTYPE_UNION       = "union";

#========================================================================
#       Class: f_token 
# Description: Encapsulate a formula token
#
#  Attributes:   tvalue - 
#                 ttype - See token definitions, above, for values
#              tsubtype - See token definitions, above, for values
#
#     Methods: f_token  - __init__()
#========================================================================
class f_token:
    def __init__(self, value, type, subtype):
        self.tvalue   = value
        self.ttype    = type
        self.tsubtype = subtype

    def __str__(self):
        return self.tvalue
#========================================================================
#       Class: f_tokens 
# Description: An ordered list of tokens

#  Attributes:        items - Ordered list 
#                     index - Current position in the list
#
#     Methods: f_tokens     - __init__()
#              f_token      - add()      - Add a token to the end of the list
#              None         - addRef()   - Add a token to the end of the list
#              None         - reset()    - reset the index to -1
#              Boolean      - BOF()      - End of list?
#              Boolean      - EOF()      - Beginning of list?
#              Boolean      - moveNext() - Move the index along one
#              f_token/None - current()  - Return the current token
#              f_token/None - next()     - Return the next token (leave the index unchanged)
#              f_token/None - previous() - Return the previous token (leave the index unchanged)
#========================================================================
class f_tokens:
    def __init__(self):
        self.items = []
        self.index = -1
  
    def add(self, value, type, subtype=""):
        if (not subtype):
            subtype = ""
        token = f_token(value, type, subtype)
        self.addRef(token)
        return token
        
    def addRef(self, token):
        self.items.append(token)
        
    def reset(self):
        self.index = -1
 
    def BOF(self):
        return self.index <= 0

    def EOF(self):
        return self.index >= (len(self.items) - 1)

    def moveNext(self):
        if self.EOF():
            return False
        self.index += 1
        return True
    
    def current(self):
        if self.index == -1:
            return None
        return self.items[self.index]

    def next(self):
        if self.EOF():
            return None
        return self.items[self.index + 1]
    
    def previous(self):
        if self.index < 1:
            return None
        return self.items[self.index -1]

#========================================================================
#       Class: f_tokenStack 
#    Inherits: ExcelParserTokens - a list of token values
# Description: A LIFO stack of tokens
#
#  Attributes:        items - Ordered list 
#
#     Methods: f_tokenStack - __init__()
#              None         - push(token) - Push a token onto the stack
#              f_token/None - pop()       - Pop a token off the stack
#              f_token/None - token()     - Non-destructively return the top item on the stack
#              String       - type()      - Return the top token's type
#              String       - subtype()   - Return the top token's subtype
#              String       - value()     - Return the top token's value
#========================================================================
class f_tokenStack(ExcelParserTokens):
    def __init__(self):
        self.items = []
    
    def push(self, token):
        self.items.append(token)
    
    def pop(self):
        token = self.items.pop()
        return f_token("", token.ttype, self.TOK_SUBTYPE_STOP)
        
    def token(self):
        # Note: this uses Pythons and/or "hack" to emulate C's ternary operator (i.e. cond ? exp1 : exp2)
        return ((len(self.items) > 0) and [self.items[len(self.items) - 1]] or [None])[0]
    
    def value(self):
        return ((self.token()) and [(self.token()).tvalue] or [""])[0]    

    def type(self):
        t = self.token()
        return ((self.token()) and [(self.token()).ttype] or [""])[0]
    
    def subtype(self):
        return ((self.token()) and [(self.token()).tsubtype] or [""])[0]

#========================================================================
#       Class: ExcelParser
# Description: Parse an Excel formula into a stream of tokens

#  Attributes:
#
#     Methods: f_tokens - getTokens(formula) - return a token stream (list)
#========================================================================
class ExcelParser(ExcelParserTokens):
    def getTokens(self, formula):
    
        def currentChar():
            return formula[offset]
    
        def doubleChar():
            return formula[offset:offset+2]
        
        def nextChar():
            # JavaScript returns an empty string if the index is out of bounds,
            # Python throws an IndexError.  We mimic this behaviour here.
            try:
                formula[offset+1]
            except IndexError:
                return ""
            else:            
                return formula[offset+1]
        
        def EOF():
            return offset >= len(formula)
    
        tokens     = f_tokens()
        tokenStack = f_tokenStack()
        offset     = 0
        token      = ""
        inString   = False
        inPath     = False
        inRange    = False
        inError    = False
    
        while (len(formula) > 0):
            if (formula[0] == " "):
                formula = formula[1:]
            else:
                if (formula[0] == "="):
                    formula = formula[1:]
                break;    
    
        # state-dependent character evaluation (order is important)
        while not EOF():
               
            # double-quoted strings
            # embeds are doubled
            # end marks token
            if inString:
                if currentChar() == "\"":
                    if nextChar() == "\"":
                        token += "\""
                        offset += 1
                    else:
                        inString = False
                        tokens.add(token, self.TOK_TYPE_OPERAND, self.TOK_SUBTYPE_TEXT)
                        token = ""
                else:
                    token += currentChar()
                offset += 1
                continue
    
            # single-quoted strings (links)
            # embeds are double
            # end does not mark a token
            if inPath:
                if currentChar() == "'":
                    if nextChar() == "'":
                        token += "'"
                        offset += 1
                    else:
                        inPath = False
                else:
                    token += currentChar()
                offset += 1;
                continue;    
    
            # bracketed strings (range offset or linked workbook name)
            # no embeds (changed to "()" by Excel)
            # end does not mark a token
            if inRange:
                if currentChar() == "]":
                    inRange = False
                token += currentChar()
                offset += 1
                continue
    
            # error values
            # end marks a token, determined from absolute list of values
            if inError:
                token += currentChar()
                offset += 1
                if ",#NULL!,#DIV/0!,#VALUE!,#REF!,#NAME?,#NUM!,#N/A,".find("," + token + ",") != -1:
                    inError = False
                    tokens.add(token, self.TOK_TYPE_OPERAND, self.TOK_SUBTYPE_ERROR)
                    token = ""
                continue;
    
            # scientific notation check
            regexSN = '^[1-9]{1}(\.[0-9]+)?[eE]{1}$';
            if (("+-").find(currentChar()) != -1):
                if len(token) > 1:
                    if re.match(regexSN,token):
                        token += currentChar();
                        offset += 1;
                        continue;
              
            # independent character evaulation (order not important)
            #
            # establish state-dependent character evaluations
            if currentChar() == "\"":
                if len(token) > 0:
                    # not expected
                    tokens.add(token, self.TOK_TYPE_UNKNOWN)
                    token = ""
                inString = True
                offset += 1
                continue
    
            if currentChar() == "'":
                if len(token) > 0:
                    # not expected
                    tokens.add(token, self.TOK_TYPE_UNKNOWN)
                    token = ""
                inPath = True
                offset += 1
                continue
    
            if (currentChar() == "["):
                inRange = True
                token += currentChar()
                offset += 1
                continue
    
            if (currentChar() == "#"):
                if (len(token) > 0):
                    # not expected
                    tokens.add(token, self.TOK_TYPE_UNKNOWN)
                    token = ""
                inError = True
                token += currentChar()
                offset += 1
                continue
    
            # mark start and end of arrays and array rows
            if (currentChar() == "{"):
                if (len(token) > 0):
                    # not expected
                    tokens.add(token, self.TOK_TYPE_UNKNOWN)
                    token = ""
                tokenStack.push(tokens.add("ARRAY", self.TOK_TYPE_FUNCTION, self.TOK_SUBTYPE_START))
                tokenStack.push(tokens.add("ARRAYROW", self.TOK_TYPE_FUNCTION, self.TOK_SUBTYPE_START))
                offset += 1
                continue
    
            if (currentChar() == ";"):
                if (len(token) > 0):
                    tokens.add(token, self.TOK_TYPE_OPERAND)
                    token = ""
                tokens.addRef(tokenStack.pop())
                tokens.add(",", self.TOK_TYPE_ARGUMENT)
                tokenStack.push(tokens.add("ARRAYROW", self.TOK_TYPE_FUNCTION, self.TOK_SUBTYPE_START))
                offset += 1
                continue
    
            if (currentChar() == "}"):
                if (len(token) > 0):
                    tokens.add(token, self.TOK_TYPE_OPERAND)
                    token = ""
                tokens.addRef(tokenStack.pop())
                tokens.addRef(tokenStack.pop())
                offset += 1
                continue
    
            # trim white-space
            if (currentChar() == " "):
                if (len(token) > 0):
                    tokens.add(token, self.TOK_TYPE_OPERAND)
                    token = ""
                tokens.add("", self.TOK_TYPE_WSPACE)
                offset += 1
                while ((currentChar() == " ") and (not EOF())):
                    offset += 1
                continue
    
            # multi-character comparators
            if (",>=,<=,<>,".find("," + doubleChar() + ",") != -1):
                if (len(token) > 0):
                    tokens.add(token, self.TOK_TYPE_OPERAND)
                    token = ""
                tokens.add(doubleChar(), self.TOK_TYPE_OP_IN, self.TOK_SUBTYPE_LOGICAL)
                offset += 2
                continue
    
            # standard infix operators
            if ("+-*/^&=><".find(currentChar()) != -1):
                if (len(token) > 0):
                    tokens.add(token, self.TOK_TYPE_OPERAND)
                    token = ""
                tokens.add(currentChar(), self.TOK_TYPE_OP_IN)
                offset += 1
                continue
    
            # standard postfix operators
            if ("%".find(currentChar()) != -1):
                if (len(token) > 0):
                    tokens.add(token, self.TOK_TYPE_OPERAND)
                    token = ""
                tokens.add(currentChar(), self.TOK_TYPE_OP_POST)
                offset += 1
                continue
    
            # start subexpression or function
            if (currentChar() == "("):
                if (len(token) > 0):
                    tokenStack.push(tokens.add(token, self.TOK_TYPE_FUNCTION, self.TOK_SUBTYPE_START))
                    token = ""
                else:
                    tokenStack.push(tokens.add("", self.TOK_TYPE_SUBEXPR, self.TOK_SUBTYPE_START))
                offset += 1
                continue
    
            # function, subexpression, array parameters
            if (currentChar() == ","):
                if (len(token) > 0):
                    tokens.add(token, self.TOK_TYPE_OPERAND)
                    token = ""
                if (not (tokenStack.type() == self.TOK_TYPE_FUNCTION)):
                    tokens.add(currentChar(), self.TOK_TYPE_OP_IN, self.TOK_SUBTYPE_UNION)
                else:
                    tokens.add(currentChar(), self.TOK_TYPE_ARGUMENT)
                offset += 1
                continue
    
            # stop subexpression
            if (currentChar() == ")"):
                if (len(token) > 0):
                    tokens.add(token, self.TOK_TYPE_OPERAND)
                    token = ""
                tokens.addRef(tokenStack.pop())
                offset += 1
                continue
    
            # token accumulation
            token += currentChar()
            offset += 1
    
        # dump remaining accumulation
        if (len(token) > 0): 
            tokens.add(token, self.TOK_TYPE_OPERAND)
    
        # move all tokens to a new collection, excluding all unnecessary white-space tokens
        tokens2 = f_tokens()
    
        while (tokens.moveNext()):
            token = tokens.current();
    
            if (token.ttype == self.TOK_TYPE_WSPACE):
                if ((tokens.BOF()) or (tokens.EOF())):
                    pass
                elif (not(
                     ((tokens.previous().ttype == self.TOK_TYPE_FUNCTION) and (tokens.previous().tsubtype == self.TOK_SUBTYPE_STOP)) or 
                     ((tokens.previous().ttype == self.TOK_TYPE_SUBEXPR) and (tokens.previous().tsubtype == self.TOK_SUBTYPE_STOP)) or 
                     (tokens.previous().ttype == self.TOK_TYPE_OPERAND)
                    )
                  ):
                    pass
                elif (not(
                     ((tokens.next().ttype == self.TOK_TYPE_FUNCTION) and (tokens.next().tsubtype == self.TOK_SUBTYPE_START)) or
                     ((tokens.next().ttype == self.TOK_TYPE_SUBEXPR) and (tokens.next().tsubtype == self.TOK_SUBTYPE_START)) or
                     (tokens.next().ttype == self.TOK_TYPE_OPERAND)
                     )
                   ):
                    pass
                else:
                    tokens2.add(token.tvalue, self.TOK_TYPE_OP_IN, self.TOK_SUBTYPE_INTERSECT)
                continue
    
            tokens2.addRef(token);
    
        # switch infix "-" operator to prefix when appropriate, switch infix "+" operator to noop when appropriate, identify operand 
        # and infix-operator subtypes, pull "@" from in front of function names
        while (tokens2.moveNext()):
            token = tokens2.current()
            if ((token.ttype == self.TOK_TYPE_OP_IN) and (token.tvalue == "-")):
                if (tokens2.BOF()):
                    token.ttype = self.TOK_TYPE_OP_PRE
                elif (
                   ((tokens2.previous().ttype == self.TOK_TYPE_FUNCTION) and (tokens2.previous().tsubtype == self.TOK_SUBTYPE_STOP)) or
                   ((tokens2.previous().ttype == self.TOK_TYPE_SUBEXPR) and (tokens2.previous().tsubtype == self.TOK_SUBTYPE_STOP)) or
                   (tokens2.previous().ttype == self.TOK_TYPE_OP_POST) or 
                   (tokens2.previous().ttype == self.TOK_TYPE_OPERAND)
                  ):
                    token.tsubtype = self.TOK_SUBTYPE_MATH;
                else:
                    token.ttype = self.TOK_TYPE_OP_PRE
                continue
    
            if ((token.ttype == self.TOK_TYPE_OP_IN) and (token.tvalue == "+")):
                if (tokens2.BOF()):
                    token.ttype = self.TOK_TYPE_NOOP
                elif (
                   ((tokens2.previous().ttype == self.TOK_TYPE_FUNCTION) and (tokens2.previous().tsubtype == self.TOK_SUBTYPE_STOP)) or 
                   ((tokens2.previous().ttype == self.TOK_TYPE_SUBEXPR) and (tokens2.previous().tsubtype == self.TOK_SUBTYPE_STOP)) or 
                   (tokens2.previous().ttype == self.TOK_TYPE_OP_POST) or 
                   (tokens2.previous().ttype == self.TOK_TYPE_OPERAND)
                  ):
                    token.tsubtype = self.TOK_SUBTYPE_MATH
                else:
                    token.ttype = self.TOK_TYPE_NOOP
                continue
    
            if ((token.ttype == self.TOK_TYPE_OP_IN) and (len(token.tsubtype) == 0)):
                if (("<>=").find(token.tvalue[0:1]) != -1):
                    token.tsubtype = self.TOK_SUBTYPE_LOGICAL
                elif (token.tvalue == "&"):
                    token.tsubtype = self.TOK_SUBTYPE_CONCAT
                else:
                    token.tsubtype = self.TOK_SUBTYPE_MATH
                continue
        
            if ((token.ttype == self.TOK_TYPE_OPERAND) and (len(token.tsubtype) == 0)):
                try:
                    float(token.tvalue)
                except ValueError, e:
                    if ((token.tvalue == 'TRUE') or (token.tvalue == 'FALSE')):
                        token.tsubtype = self.TOK_SUBTYPE_LOGICAL
                    else:
                        token.tsubtype = self.TOK_SUBTYPE_RANGE
                else:
                    token.tsubtype = self.TOK_SUBTYPE_NUMBER
                continue
    
            if (token.ttype == self.TOK_TYPE_FUNCTION):
                if (token.tvalue[0:1] == "@"):
                    token.tvalue = token.tvalue[1:]
                continue
    
        tokens2.reset();
    
        # move all tokens to a new collection, excluding all noops
        tokens = f_tokens()
        while (tokens2.moveNext()):
            if (tokens2.current().ttype != self.TOK_TYPE_NOOP):
                tokens.addRef(tokens2.current())
    
        tokens.reset()
        return tokens    

    def parse(self, formula):
        self.tokens = self.getTokens(formula)
        
    def render(self):
        output = ""
        if self.tokens:
            for t in self.tokens.items:
                if   t.ttype == self.TOK_TYPE_FUNCTION and t.tsubtype == self.TOK_SUBTYPE_START:     output += t.tvalue + "("
                elif t.ttype == self.TOK_TYPE_FUNCTION and t.tsubtype == self.TOK_SUBTYPE_STOP:      output += ")"
                elif t.ttype == self.TOK_TYPE_SUBEXPR  and t.tsubtype == self.TOK_SUBTYPE_START:     output += "("
                elif t.ttype == self.TOK_TYPE_SUBEXPR  and t.tsubtype == self.TOK_SUBTYPE_STOP:      output += ")"
                # TODO: add in RE substitution of " with "" for strings
                elif t.ttype == self.TOK_TYPE_OPERAND  and t.tsubtype == self.TOK_SUBTYPE_TEXT:      output += "\"" + t.tvalue + "\""
                elif t.ttype == self.TOK_TYPE_OP_IN    and t.tsubtype == self.TOK_SUBTYPE_INTERSECT: output += " "                    

                else: output += t.tvalue
        return output
    
    def prettyprint(self):
        indent = 0
        output = ""
        if self.tokens:
            for t in self.tokens.items:
                #print "'",t.ttype,t.tsubtype,t.tvalue,"'"
                if (t.tsubtype == self.TOK_SUBTYPE_STOP):
                    indent -= 1
    
                output += "    "*indent + t.tvalue + " <" + t.ttype +"> <" + t.tsubtype + ">" + "\n"
                
                if (t.tsubtype == self.TOK_SUBTYPE_START):
                    indent += 1;
        return output


