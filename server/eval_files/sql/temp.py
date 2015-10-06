#ADD COMMANDS HERE
#from AS.stdlib import *
from AS.ui.styling import *
# from AS.tests.min import *
from AS.instruments.ETF import ETF
from AS.instruments.Stock import Stock
# from AS.ui.plot import *
import json
from sys import exc_info
import traceback
from AS.pandas.input import listToDataframe
from AS.pandas.output import pprint
from pandasql import sqldf
import pandas as pd

from sqlalchemy import create_engine
from pandas import read_sql_query

pysqldf = lambda q: sqldf(q, globals())

result = "DefaultSqlValue"
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


def setGlobals(context):
	for i in range(len(context)):
		e = eval(context[i])
		if isinstance(e,list):
			globals()["dataset"+str(i)] = listToDataframe(e)
		else:
			globals()["replaceCmd"] = modifyReplace(i,e)

def pprintErr(e):
	exc_type, exc_obj, exc_tb = exc_info()
	fname = 'AlphaSheets Python evaluator'
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 52 # subtract template lines
	return {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}

def pprintSql(res):
	if isinstance(res, pd.DataFrame):
		return pprint(res)
	else: 
		return res

try:
	setGlobals(["[[\"a\",\"b\"],[1.0,3.0],[2.0,4.0]]"])
	result = pprintSql(db('select * from dataset0 where a>1'))
except Exception as e:
	result = pprintErr(e)