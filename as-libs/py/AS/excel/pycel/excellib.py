from __future__ import division
import numpy as np
from math import log
from string import *
from AS.iterable import *
import scipy.stats as st

#############################################################################################################################

# A dictionary that maps excel function names onto python equivalents. You should
# only add an entry to this map if the python name is different to the excel name
# (which it may need to be to  prevent conflicts with existing python functions 
# with that name, e.g., max).

# So if excel defines a function foobar(), all you have to do is add a function
# called foobar to this module.  You only need to add it to the function map,
# if you want to use a different name in the python code. 

# Note: some functions (if, pi, atan2, and, or, array, ...) are already taken care of
# in the FunctionNode code, so adding them here will have no effect.

FUNCTION_MAP = {
      "ln":"xlog",
      "min":"xmin",
      "min":"xmin",
      "max":"xmax",
      "sum":"xsum",
      "gammaln":"lgamma"
      }
VOLATILE_LIST  = ["today","offset"] # need to be lower-case


#############################################################################################################################
# Implementation of Excel statistical functions in Python

def rank(elem,arr,order=0):
    numArr = filter(numeric,arr.lst)
    ind = numArr.index(elem)
    if order == 0: 
        return st.rankdata(numArr,'min')[ind] # Deals with ties correctly
    elif order == 1: 
        return len(numArr)+1-rank(elem,arr,1)
    else:
        raise TypeError("Not a valid order type")

def xmax(*args):
    return np.max()


#############################################################################################################################
# Utility methods

def numeric(x):
    return isinstance(x,int) or isinstance(x,long) or isinstance(x,float) 

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

def transpose(lst):
    return lst.transpose()

def address(row,column,abs=0,sheet=""):
    presheet = ""
    if (abs==0):
        presheet = '$'+intToCol(column)+'$'+int(row)
    elif abs==1:
        presheet = '$'+intToCol(column)+int(row)
    elif abs==2:
        presheet = intToCol(column) + '$' + int(row)
    elif abs==3:
        presheet = intTOCol(column) + int(row)
    else:
        raise ValueError("Invalid abs argument")
    if sheet!="":
        return sheet+presheet
    else:
        return presheet

def xabs(a):
    if isinstance(a,int) or isinstance(a,float):
        return abs(a)
    else:
        return [abs(x) for x in lst]

def even(a):
    return math.ceil(a / 2.) * 2

def sumproduct(*args):
    product = args[0]
    for i in range(1,len(args)):
        product *= args[i]
    return sum(product)

def match(lst,key,type):
    if (type==0):
        return lst.index(key)+1
    raise ValueError("Not yet implemented: match with these arguments")


def trim(s):
    return strip(s)

def sumif(range1,elem,range2):
    s = 0
    for i in range(len(range1)):
        if (range1[i]==elem):
            s += range2[i]
    return s

def countif(range1,elem):
    s = 0
    for i in range(len(range1)):
        if (range1[i]==elem):
            s += 1
    return s

def averageif(range1,elem,range2):
    return sumif(range1,elem,range2)/float(countif(range1,elem))

# counts the number of numbers in a list
def count(lst):
    count = 0
    for elem in lst:
        try:
            float(elem)
            count += 1
        except:
            pass
    return count


def value(text):
    # make the distinction for naca numbers
    if text.find('.') > 0:
        return float(text)
    else:
        return int(text)

    
def right(text,n):
    #TODO: hack to deal with naca section numbers
    if isinstance(text, unicode) or isinstance(text,str):
        return text[-n:]
    else:
        # TODO: get rid of the decimal
        return str(int(text))[-n:]

def index(lst,i,j=None):
    if j!=None:
        return lst[i-1][j-1]
    else:
        return lst[i-1]


def lookup(value, lookup_range, result_range):
    
    # TODO
    if not isinstance(value,(int,float)):
        raise Exception("Non numeric lookups (%s) not supported" % value)
    
    # TODO: note, may return the last equal value
    
    # index of the last numeric value
    lastnum = -1
    for i,v in enumerate(lookup_range):
        if isinstance(v,(int,float)):
            if v > value:
                break
            else:
                lastnum = i
                

    if lastnum < 0:
        raise Exception("No numeric data found in the lookup range")
    else:
        if i == 0:
            raise Exception("All values in the lookup range are bigger than %s" % value)
        else:
            if i >= len(lookup_range)-1:
                # return the biggest number smaller than value
                return result_range[lastnum]
            else:
                return result_range[i-1]

def linest(*args, **kwargs):

    Y = args[0]
    X = args[1]
    
    if len(args) == 3:
        const = args[2]
        if isinstance(const,str):
            const = (const.lower() == "true")
    else:
        const = True
        
    degree = kwargs.get('degree',1)
    
    # build the vandermonde matrix
    A = np.vander(X, degree+1)
    
    if not const:
        # force the intercept to zero
        A[:,-1] = np.zeros((1,len(X)))
    
    # perform the fit
    (coefs, residuals, rank, sing_vals) = np.linalg.lstsq(A, Y)
        
    return coefs

def npv(*args):
    discount_rate = args[0]
    cashflow = args[1]
    return sum([float(x)*(1+discount_rate)**-(i+1) for (i,x) in enumerate(cashflow)])

if __name__ == '__main__':
    pass

