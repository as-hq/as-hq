import numpy as np
import pandas as pd
import random
from AS.iterable import ASIterable
import json

def arr(lst):
	return ASIterable(lst)

def hide(lst): 
	return ASIterable(lst).hide()

def uniqueId():
	lstFiles = os.listdir(imagesPath)
	pythonImageFiles = filter(lambda s: s.startswith(imagePrefix),lstFiles)
	pythonNumbers = map(lambda s: int(s[len(imagePrefix):-4]),pythonImageFiles)
	newNumber = 1
	if len(pythonNumbers) > 0:
		newNumber = max(pythonNumbers) + 1
	return imagePrefix + str(newNumber) + ".png"

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

def serialize(val):
	if isinstance(val, list):
		return json.dumps({'tag': 'Expanding', 'expandingType': 'List', 'listVals': val})
	elif isinstance(val, dict):
		return json.dumps({'tag': 'CellValue', 'cellValueType': 'Serialized', 'serializedValue': json.dumps(val)})
	elif isinstance(val, np.matrixlib.defmatrix.matrix):
		return json.dumps({'tag': 'Expanding', 'expandingType': 'NPMatrix', 'matrixVals': val.tolist()})
	elif isinstance(val, np.ndarray):
		def f(e):
			if isinstance(e, np.ndarray): return e.tolist()
			else: return e
		vals = [f(e) for e in val]
		return json.dumps({'tag': 'Expanding', 'expandingType': 'NPArray', 'arrayVals': vals})
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
		return json.dumps({'tag': 'Expanding', 'expandingType': 'List', 'listVals': val.toList()})
	else: return json.dumps(val)

def pprintDataFrame(dataframe):
	rows = repr(dataframe).split('\n')
	mat = [row.split(' ') for row in rows]
	cleaned = [[e for e in row if e is not ''] for row in mat]
	cleaned[0].insert(0,'')
	return cleaned
