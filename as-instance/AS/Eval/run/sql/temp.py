
#ADD COMMANDS HERE
from AS.stdlib import *
from AS.ui.styling import *
# from AS.tests.min import *
from AS.instruments.ETF import ETF
from AS.instruments.Stock import Stock
# from AS.ui.plot import *
import json
import sys, os
import traceback
from AS.pandas.input import listToDataframe
from AS.pandas.output import pprint
from pandasql import sqldf

pysqldf = lambda q: sqldf(q, globals())

try:
	queryStr = 'SELECT * FROM A1:B4 where a>2'.replace('\n',' ')
	(rangeStr, data) = ('A1:B4', [[1.0,"b"],[1.0,4.0],[2.0,5.0],[3.0,6.0]])

	objectMap = {} #  way of dealing with objects
	for i in range(len(data)):
		for j in range(len(data[i])):
			try:
				objectMap[data[i][j].displayValue()] = data[i][j]
				data[i][j]=data[i][j].displayValue()
			except Exception as e: 
				continue

	queryStr = queryStr.replace(rangeStr, "dataset")
	dataset = listToDataframe(data)
	cleaned = pprint(pysqldf(queryStr).head())

	for i in range(len(cleaned)): 
		for j in range(len(cleaned[i])):
			if objectMap.has_key(cleaned[i][j]):
				cleaned[i][j] = objectMap[cleaned[i][j]]

	print(cleaned)

except Exception as e: 
	exc_type, exc_obj, exc_tb = sys.exc_info()
	fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 15 # subtract template lines
	errJson = {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	print(errJson)