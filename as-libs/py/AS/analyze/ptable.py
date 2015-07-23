from pandas import DataFrame, pivot_table
from AS.errors import ColumnHeaderNotPresent
from AS.pandas.output import pprint
import numpy as np

def ptable(lst,values=None,index=None,columns=None,aggfunc = np.sum):
	fromitems = map(lambda x: toItems(x),map(list,zip(*lst)))
	df = DataFrame.from_items(fromitems)
	table = pivot_table(df,values=values,index=index,aggfunc=aggfunc,columns=columns)
	rows = repr(DataFrame.fillna(table,'-')).split('\n')
	cols = map(list,zip(*rows))
	colsClean = filter(lambda x : (len(set(x))!=1),cols)
	return map(list,zip(*colsClean))
	#return pprint(table)

def toItems(lst):
	if not isinstance(lst[0], basestring):
		raise ColumnHeaderNotPresent
	return (lst[0],[float(x) for x in lst[1:]])

		