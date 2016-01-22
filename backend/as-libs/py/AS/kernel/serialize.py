from __future__ import print_function

import numpy as np
import pandas as pd

from AS.hidden import Hidden

import matplotlib._pylab_helpers as mpl

import os
import sys
import json
import cPickle
import base64
import uuid
import datetime

imageSavePath = os.path.dirname(os.getcwd()) + '/server/static/images/'

#----------------------------------------------------------------------------------------------------------------------------------------------
#-- Serialization

def wrapValue(val):
  if isImageInScope() and shouldShowImage(val):
    figures = [manager.canvas.figure for manager in mpl.Gcf.get_all_fig_managers()]
    uid = uuid.uuid4().hex + '.png'
    savePath = imageSavePath + uid
    figures[-1].savefig(savePath)
    print("DETECTED IMAGE, SAVING TO: " + savePath, file=sys.__stdout__)
    mpl.Gcf.destroy_all()
    return json.dumps({'tag': 'CellValue',
                       'cellValueType': 'Image',
                       'imagePath': uid})
  else:
    return serialize(val)

def shouldShowImage(val):
  # TODO: in the future, we want to also check if the value is actually a matplotlib instance
  return True

def isImageInScope():
  return len(mpl.Gcf.get_all_fig_managers()) > 0

def serialize(val):
  if isinstance(val, list):
    if np.array(val).ndim > 2:
      return json.dumps(generalSerialize(val, 'LIST'))
    else: 
      vals = [serializeListElem(e) for e in val]
      sVal = {'tag': 'Expanding', 'expandingType': 'List', 'listVals': vals}
      return json.dumps(sVal)

  elif isinstance(val, dict):
    return json.dumps(generalSerialize(val, 'DICT'))

  elif isinstance(val, np.matrixlib.defmatrix.matrix):
    return json.dumps({'tag': 'Expanding', 'expandingType': 'NPMatrix', 'matrixVals': val.tolist()})

  elif isinstance(val, np.ndarray):
    if val.ndim > 2:
      return json.dumps(generalSerialize(val, 'NP ARRAY'))
    else: 
      vals = [serializeListElem(e) for e in val.tolist()]
      sVal = {'tag': 'Expanding', 'expandingType': 'NPArray', 'arrayVals': vals}
      return json.dumps(sVal)

  elif isinstance(val, pd.DataFrame):
    labels = val.columns.values.tolist()
    indices = val.index.values.tolist()
    data = val.get_values().tolist()
    return json.dumps({'tag': 'Expanding', 
                       'expandingType': 'PDataFrame', 
                       'dfLabels': labels,
                       'dfIndices': indices,
                       'dfData': data})

  elif isinstance(val, pd.Series):
    indices = val.index.values.tolist()
    data = val.get_values().tolist()
    return json.dumps({'tag': 'Expanding',
                       'expandingType': 'PSeries',
                       'seriesIndices': indices,
                       'seriesData': data})

  # elif isinstance(val, ASIterable): 
  #   if val.hidden or val.arr.ndim > 2:
  #     name = 'HIDDEN LIST'
  #     if val.name:
  #       name = val.name
  #     return json.dumps(generalSerialize(val, name))
  #   else: 
  #     vals = [serializeListElem(e) for e in val.toList()]
  #     sVal = {'tag': 'Expanding', 'expandingType': 'List', 'listVals': vals}
  #     return json.dumps(sVal)

  elif isinstance(val, Hidden):
    return json.dumps(generalSerialize(val, val.name))

  elif isinstance(val, datetime.datetime):
    return json.dumps(generalSerialize(val, str(val)))

  else: 
    try:
      return json.dumps(val)
    except Exception as e:
      return json.dumps(generalSerialize(val, 'GENERIC'))

def serializeListElem(val):
  if isinstance(val, list):
    return [serializeListElem(e) for e in val]
  elif isinstance(val, np.ndarray):
    return [serializeListElem(e) for e in val.tolist()]
  # elif isinstance(val, ASIterable):
  #   return [serializeListElem(e) for e in val.toList()]
  elif isinstance(val, dict):
    return generalSerialize(val, 'DICT')
  elif isPrimitive(val):
    return val
  else:
    return generalSerialize(val, 'GENERIC')

def generalSerialize(val, name):
  sval = 'cPickle.loads(base64.b64decode(\"' + base64.b64encode(cPickle.dumps(val)) + '\"))' 
  return {'tag': 'CellValue', 
          'cellValueType': 'Serialized', 
          'serializedValue': sval, 
          'displayName': name}

def isPrimitive(val):
  return (type(val) in (int, float, bool, str, np.int64, np.float64, np.string_)) or (val is None)
