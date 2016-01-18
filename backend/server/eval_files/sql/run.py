from AS.stdlib import *
from AS.ui.styling import *
from AS.instruments.ETF import ETF
from AS.errors import ColumnHeaderNotPresent
from AS.instruments.Stock import Stock
import json
from sys import exc_info
import traceback
from pandasql import sqldf
import pandas as pd
import numpy as np

from sqlalchemy import create_engine
from pandas import read_sql_query

pysqldf = lambda q: sqldf(q, globals())

result = serialize("DefaultSqlValue")
def db(dbCmd,dbName=""):
	try:
		if "replaceCmd" in globals():
			dbCmd = replaceCmd(dbCmd)
		return pysqldf(dbCmd).head()
	except:
		try:
			e = create_engine('sqlite:///'+dbName) #absolute file path
			return read_sql_query(dbCmd,e)
		except Exception as e: 
			return pprintErr(e)

def modifyReplace(i,e):
	if "replaceCmd" in globals():
		return lambda s : replaceCmd(s).replace("dataset"+str(i),e)
	else:
		return lambda s : s.replace("dataset"+str(i),e)

#assumes lst is list of rows
def listToDataframe(lst): 
	if len(np.array(lst).shape) == 1:
		return listToDataframe([[a] for a in lst])
	else:
		Undefined = "NaN" 
		#first row must contain all strings
		for colHeader in lst[0]:
			if not isinstance(colHeader, basestring):
				raise ColumnHeaderNotPresent
		maxVals = len(lst) - 1
		data = [{} for _ in range(maxVals)]
		for rowIdx in range(maxVals):
			row = lst[rowIdx + 1]
			for colIdx in range(len(lst[0])):
				header = lst[0][colIdx]
				data[rowIdx][header] = row[colIdx]
		return pd.DataFrame(data)

def setGlobals(context):
	for i in range(len(context)):
		e = eval(context[i])
		if isinstance(e,list):
			globals()["dataset"+str(i)] = listToDataframe(e)
		else:
			globals()["replaceCmd"] = modifyReplace(i,e)

def pprintErr(e): 
	exc_type, exc_obj, exc_tb = exc_info()
	err = repr(e).replace("\'","").replace("'",'"')
	errJson = {'tag': 'CellValue', 'cellValueType': 'Error', 'errorType': repr(exc_type), 'errorMsg': err}
	return json.dumps(errJson)

try:
	setGlobals([])
	result = serialize(db('e'))
except Exception as e:
	result = pprintErr(e)