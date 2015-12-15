import numpy as np
import pandas as pd
import random
from AS.iterable import ASIterable
import json
import cPickle

def arr(lst):
	return ASIterable(lst)

def hide(lst): 
	return ASIterable(lst).hide()

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

def rand(m=1,n=1,upperbound=1):
	if n==1 and m != 1:
		return ASIterable(np.random.rand(m)*randint(1,upperbound))
	elif m==1 and n!=1:
		return ASIterable(np.random.rand(m)*randint(1,upperbound)).transpose()
	elif m==1 and n==1:
		return np.random_sample*random.randint(1,upperbound)
	else: return ASIterable(np.random.rand(m,n)*random.randint(1,upperbound))

#----------------------------------------------------------------------------------------------------------------------------------------------
#-- Serialization

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

	elif isinstance(val, ASIterable): 
		if val.hidden or val.arr.ndim > 2:
			name = 'HIDDEN RANGE'
			if val.name:
				name = val.name
			return json.dumps(generalSerialize(val, name))
		else: 
			vals = [serializeListElem(e) for e in val.toList()]
			sVal = {'tag': 'Expanding', 'expandingType': 'List', 'listVals': vals}
			return json.dumps(sVal)

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
	elif isinstance(val, ASIterable):
		return [serializeListElem(e) for e in val.toList()]
	elif isinstance(val, dict):
		return generalSerialize(val, 'DICT')
	elif isPrimitive(val):
		return val
	else:
		return generalSerialize(val, 'GENERIC')

def generalSerialize(val, name):
	sval = 'cPickle.loads(' + json.dumps(cPickle.dumps(val)) + ')' # because newline escaping for cPickle bullshit
	return {'tag': 'CellValue', 
				  'cellValueType': 'Serialized', 
				  'serializedValue': sval, 
				  'displayName': name}

def isPrimitive(val):
	return (type(val) in (int, float, bool, str, np.int64, np.float64, np.string_)) or (val is None)

def pprintDataFrame(dataframe):
	rows = repr(dataframe).split('\n')
	mat = [row.split(' ') for row in rows]
	cleaned = [[e for e in row if e is not ''] for row in mat]
	cleaned[0].insert(0,'')
	return cleaned
