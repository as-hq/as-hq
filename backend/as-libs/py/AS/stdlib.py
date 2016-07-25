import numpy as np
import pandas as pd
import math
import random
from AS.iterable import ASIterable
from AS.hidden import Hidden
from AS.errors import *
from AS.formats import *
from AS.dataframe import dataframe

def arr(lst):
  return ASIterable(lst)

def flatten(lst):
  return [item for sublist in lst for item in sublist]

def remove_nones(lst):
  return [x for x in lst if x != None]

def transpose(lst):
  if isinstance(lst, ASIterable):
    return lst.transpose()
  arr = np.array(lst)
  if arr.ndim == 1:
    return np.array([lst]).tolist() # [1,2] down a column --> [[1,2]]
  else: 
    return arr.transpose().tolist()

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

def pprintDataFrame(dataframe):
  rows = repr(dataframe).split('\n')
  mat = [row.split(' ') for row in rows]
  cleaned = [[e for e in row if e is not ''] for row in mat]
  cleaned[0].insert(0,'')
  return cleaned
