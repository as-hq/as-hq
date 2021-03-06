from __future__ import print_function

import numpy as np
import pandas as pd

from AS.hidden import Hidden
import AS.functions.openExcel as ex
from AS.iterable import ASIterable

import matplotlib._pylab_helpers as mpl

import os
import ast
import sys
import json
import cPickle
import base64
import uuid
import datetime

import bs4 # BeautifulSoup string serialization

imageSavePath = os.path.dirname(os.getcwd()) + '/server/static/images/'

#----------------------------------------------------------------------------------------------------------------------------------------------
#-- Node transformer for the "evaluable" line of an code block

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
        call = ast.Call(func=ast.Name(id='wrapValue', ctx=ast.Load()),
                        args=[node.value],
                        keywords=[])
        newnode = ast.Expr(value=call)
        newnode = ast.copy_location(newnode, node)
        ast.fix_missing_locations(newnode)
        return newnode
    else:
        # can't wrap non-Expr node
        return node

#----------------------------------------------------------------------------------------------------------------------------------------------
#-- Serialization helpers

def wrapValue(val):
  if (isImageInScope() and shouldShowImage(val)):
    figures = [manager.canvas.figure for manager in mpl.Gcf.get_all_fig_managers()]
    uid = uuid.uuid4().hex + '.png'
    savePath = imageSavePath + uid
    figures[-1].savefig(savePath)
    print("DETECTED IMAGE, SAVING TO: " + savePath, file=sys.__stdout__)
    mpl.Gcf.destroy_all() # #incomplete: is only correct for one user.
    return json.dumps({'tag': 'CellValue',
                       'cellValueType': 'Image',
                       'imagePath': uid})
  elif 'imagePath' in val and val['imagePath'] == 'plot3d':
    return json.dumps({'tag': 'CellValue',
                       'cellValueType': 'Image',
                       'imagePath': 'plot3d'})
  else:
    return serialize(val)

def shouldShowImage(val):
  # TODO: in the future, we want to also check if the value is actually a matplotlib instance
  return True

def isImageInScope():
  return len(mpl.Gcf.get_all_fig_managers()) > 0 # #incomplete: is only correct for one user.

def getDimensions(val):
  return np.array(val, dtype=object).ndim

# Note: this function converts all empty lists to None, in order to be consistent
# with cell-expanding behavior of nonempty lists (the empty list literally takes
# up 0 cells). Specifically it prevents creation of 0-length RangeKeys.
# HOWEVER, this screws with @ references to lists which may shrink to 0.
#needsrefactor deal with that case, 80/20 for now.
def serialize(val):
  if isinstance(val, list):
    if len(val) == 0:
      sval = None
    elif getDimensions(val) > 2:
      sval = generalSerialize(val, 'LIST')
    else: 
      vals = [serializeListElem(e) for e in val]
      sval = {'tag': 'Expanding', 'expandingType': 'List', 'listVals': vals}

  elif isinstance(val, dict):
    sval = generalSerialize(val, 'DICT')

  elif isinstance(val, np.matrixlib.defmatrix.matrix):
    sval = {'tag': 'Expanding', 'expandingType': 'NPMatrix', 'matrixVals': val.tolist()}

  elif isinstance(val, np.ndarray):
    if len(val) == 0:
      sval = None
    elif val.ndim > 2:
      sval = generalSerialize(val, 'NP ARRAY')
    else: 
      vals = [serializeListElem(e) for e in val.tolist()]
      sval = {'tag': 'Expanding', 'expandingType': 'NPArray', 'arrayVals': vals}

  elif isinstance(val, pd.DataFrame):
    # This is being used instead of toList so that we can choose how to 
    # convert types, instead of toList's "closest Python datatype", which
    # converts datetime64 objects to ints
    labels = [serializeListElem(x) for x in val.columns.values]
    indices = [serializeListElem(x) for x in val.index.values]
    data = [serializeListElem(x) for x in val.get_values()]
    sval = {'tag': 'Expanding', 
           'expandingType': 'PDataFrame', 
           'dfLabels': labels,
           'dfIndices': indices,
           'dfData': data}

  elif isinstance(val, pd.Series):
    indices = [serializeListElem(x) for x in val.index.values]
    data = [serializeListElem(x) for x in val.get_values()]
    sval = {'tag': 'Expanding',
             'expandingType': 'PSeries',
             'seriesIndices': indices,
             'seriesData': data}

  elif isinstance(val, ASIterable): 
    if len(val) == 0:
      sval = None
    elif val.hidden or val.dimension() > 2:
      name = 'HIDDEN LIST'
      if val.name:
        name = val.name
      sval = generalSerialize(val, name)
    else: 
      vals = [serializeListElem(e) for e in val.tolist2d()]
      sVal = {'tag': 'Expanding', 'expandingType': 'List', 'listVals': vals}
      sval = sVal

  elif isinstance(val, Hidden):
    sval = generalSerialize(val, val.name)

  elif isinstance(val, datetime.datetime):
    sval = generalSerialize(val, str(val))

  elif isinstance(val, ex.Sheet):
    sval = val.serialize()

  else: 
    sval = val

  try: 
    return json.dumps(sval, ensure_ascii=False)
  except Exception as e:
    sval = generalSerialize(sval, 'GENERIC')
    return json.dumps(sval, ensure_ascii=False)

# There are certain designated datatypes -- e.g. lists -- which expand
# on the spreadsheet. These datastructures should be _recursively_
# serialized. This function does that for lists, checking e.g. dimensionality
# of sublists is <= 1.
def serializeListElem(val):
  if isPrimitive(val):
    return val
  elif isinstance(val, list):
    if len(val) == 0:
      return None
    elif getDimensions(val) > 1:
      return generalSerialize(val, 'LIST')
    else:
      return [serializeListElem(e) for e in val]
  elif isinstance(val, datetime.datetime):
    return generalSerialize(val, str(val))
  elif isinstance(val, np.datetime64):
    return generalSerialize(val, str(val))
  elif isinstance(val, np.ndarray):
    if len(val) == 0:
      return None
    else:
      return [serializeListElem(e) for e in val.tolist()]
  elif isinstance(val, ASIterable):
    if len(val) == 0:
      return None
    else:
      return [serializeListElem(e) for e in val.tolist()]
  elif isinstance(val, dict):
    return generalSerialize(val, 'DICT')
  else:
    return generalSerialize(val, 'GENERIC')

def generalSerialize(val, name):
  sval = 'cPickle.loads(base64.b64decode(\"' + base64.b64encode(cPickle.dumps(val)) + '\"))' 
  return {'tag': 'CellValue', 
          'cellValueType': 'Serialized', 
          'serializedValue': sval, 
          'displayName': name}

def isPrimitive(val):
  return (type(val) in (int, 
                        float, 
                        np.int64, 
                        np.float64, 
                        bool, 
                        str, 
                        unicode, 
                        np.string_,
                        bs4.element.NavigableString
                        )) or (val is None)
