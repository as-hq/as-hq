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
		return json.dumps({'tag': 'List', 'listVals': val})
	elif isinstance(val, dict):
		return json.dumps({'tag': 'Object', 'objectType': 'PDict', 'dict': val})
	elif isinstance(val, np.ndarray):
		return json.dumps({'tag': 'Object', 'objectType': 'NPArray', 'arrayVals': val.tolist()})
	elif isinstance(val, np.matrixlib.defmatrix.matrix):
		return json.dumps({'tag': 'Object', 'objectType': 'NPMatrix', 'matrixVals': val.tolist()})
	elif isinstance(val, pd.DataFrame):
		labels = val.columns.values.tolist()
		indices = val.index.values.tolist()
		data = val.get_values().tolist()
		return json.dumps({'tag': 'Object', 
											 'objectType': 'PDataFrame', 
											 'dfLabels': labels,
											 'dfIndices': indices,
											 'dfData': data})
	elif isinstance(val, pd.Series):
		indices = val.index.values.tolist()
		data = val.get_values().tolist()
		return json.dumps({'tag': 'Object',
											 'objectType': 'PSeries',
											 'seriesIndices': indices,
											 'seriesData': data})
	elif isinstance(val, ASIterable): 
		return json.dumps({'tag': 'List', 'listVals': val.toList()})
	elif isinstance(val, bool):
		return repr(val)
	elif val is None:
		return 'None'
	else: return json.dumps(val)

def deserialize(dic):
	if 'tag' in dic and dic['tag'] == 'Object':
		if 'objectType' in dic:
			oType = dic['objectType']
			if oType == 'PDict':
				return dic['dict']
			elif oType == 'NPArray':
				return np.array(dic['arrayVals'])
			elif oType == 'NPMatrix':
				return np.matrix(dic['matrixVals'])
			elif oType == 'PDataFrame':
				return pd.DataFrame(data=dic['dfData'], columns=dic['dfLabels'], index=dic['dfIndices'])
			elif oType == 'PSeries':
				return pd.DataFrame(data=dic['seriesData'], index=dic['seriesIndices'])

def pprintDataFrame(dataframe):
	rows = repr(dataframe).split('\n')
	mat = [row.split(' ') for row in rows]
	cleaned = [[e for e in row if e is not ''] for row in mat]
	cleaned[0].insert(0,'')
	return cleaned
