#ADD COMMANDS HERE
#from AS.stdlib import *
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
import pandas as pd

from sqlalchemy import create_engine
from pandas import read_sql_query

pysqldf = lambda q: sqldf(q, globals())

result = "DefaultSqlValue"
def db(dbCmd,dbName=""):
	try:
		return pysqldf(dbCmd).head()
	except:
		try:
			e = create_engine('sqlite:///'+dbName) #absolute file path
			return read_sql_query(dbCmd,e)
		except Exception as e: 
			return pprintErr(e)

def setGlobals(context):
	for i in range(len(context)):
		globals()["dataset"+str(i)] = listToDataframe([row for row in eval(context[i])])

def pprintErr(e):
	exc_type, exc_obj, exc_tb = exc_info()
	fname = 'AlphaSheets Python evaluator'
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 20 # subtract template lines
	return {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}

def pprintSql(res):
	if isinstance(e, pd.DataFrame):
		return pprint(res)
	else: 
		return res

