import numpy as np

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

def sumWay(lst, axis):
	return np.sum(lst, axis).tolist()

