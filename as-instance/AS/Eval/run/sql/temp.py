
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
	queryStr = 'SELECT * FROM A1:D5 WHERE Price>50'.replace('\n',' ')
	(rangeStr, data) = ('A1:D5', [["Time","Trade","Price","Volume"],["14:00:00","TRADE",66.4,300.0],["14:28:00","TRADE",59.46,400.0],["14:32:00","TRADE",43.18,300.0],["14:58:00","TRADE",51.26,400.0]])

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