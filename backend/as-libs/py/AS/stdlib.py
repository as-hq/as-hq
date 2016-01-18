import numpy as np
import pandas as pd
import random
# from AS.iterable import ASIterable
from AS.hidden import Hidden
from AS.errors import *
from AS.formats import *

# def arr(lst):
#   return ASIterable(lst)

def space(lst, sp):
  lst2 = map((lambda x: prefixPush(x, ["" for _ in range(sp)])), lst)
  return flat(lst2)[:-sp]

def flat(lst):
  return [item for sublist in lst for item in sublist]

def prefixPush(elem, lst):
  lst.insert(0, elem) 
  return lst

def every(lst, k):
  return lst[0::k]

def sumAxis(lst, axis):
  return np.sum(lst, axis).tolist()

def multiply(lst1, lst2):
  return (np.multiply(lst1, lst2)).tolist()

def reshape(lst,axis1,axis2):
  return np.array(lst).reshape((axis1, axis2)).tolist() 

def transpose(lst):
  return np.array(lst).transpose().tolist()

def sumSquares(lst):
  return sum(np.array(lst)**2)

# def rand(m=1,n=1,upperbound=1):
#   if n==1 and m != 1:
#     return ASIterable(np.random.rand(m)*randint(1,upperbound))
#   elif m==1 and n!=1:
#     return ASIterable(np.random.rand(m)*randint(1,upperbound)).transpose()
#   elif m==1 and n==1:
#     return np.random_sample*random.randint(1,upperbound)
#   else: return ASIterable(np.random.rand(m,n)*random.randint(1,upperbound))

def hide(val, name='HIDDEN'):
  if isinstance(val, Hidden):
    return val
  # elif isinstance(val, ASIterable) and val.hidden:
  #   return val
  else:
    return Hidden(val, name)

def unhide(val):
  if isinstance(val, Hidden): # or isinstance(val, ASIterable):
    return val.unhide()
  else:
   return val

def oneDim(lst):
    if not isinstance(lst, list): 
      raise OneDimCalledOnNonListObject
    if len(lst) == 1: # if it's a row 
        return lst[0]
    elif all(isinstance(l, list) and len(l) == 1 for l in lst): # it's a column
        return [l[0] for l in lst]
    else: 
        raise OneDimCalledOnNonRowOrCol

def pprintDataFrame(dataframe):
  rows = repr(dataframe).split('\n')
  mat = [row.split(' ') for row in rows]
  cleaned = [[e for e in row if e is not ''] for row in mat]
  cleaned[0].insert(0,'')
  return cleaned
