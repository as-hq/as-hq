import numpy as np
import random
from AS.iterable import ASIterable

def arr(lst):
	return ASIterable(lst)

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





