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
	return {'errType': repr(exc_type), 'file': fname, 'position': pos, 'error': err}

def pprintSql(res):
	if isinstance(res, pd.DataFrame):
		return pprint(res)
	else: 
		return res

try:
	setGlobals(["[[\"mpg\",\"cyl\",\"disp\",\"hp\",\"drat\",\"wt\",\"qsec\"],[21,6,160,110,3.9,2.62,16.46],[21,6,160,110,3.9,2.875,17.02],[22.8,4,108,93,3.85,2.32,18.61],[21.4,6,258,110,3.08,3.215,19.44],[18.7,8,360,175,3.15,3.44,17.02],[18.1,6,225,105,2.76,3.46,20.22],[14.3,8,360,245,3.21,3.57,15.84],[24.4,4,146.7,62,3.69,3.19,20],[22.8,4,140.8,95,3.92,3.15,22.9],[19.2,6,167.6,123,3.92,3.44,18.3],[17.8,6,167.6,123,3.92,3.44,18.9],[16.4,8,275.8,180,3.07,4.07,17.4],[17.3,8,275.8,180,3.07,3.73,17.6],[15.2,8,275.8,180,3.07,3.78,18],[10.4,8,472,205,2.93,5.25,17.98],[10.4,8,460,215,3,5.424,17.82],[14.7,8,440,230,3.23,5.345,17.42],[32.4,4,78.7,66,4.08,2.2,19.47],[30.4,4,75.7,52,4.93,1.615,18.52],[33.9,4,71.1,65,4.22,1.835,19.9],[21.5,4,120.1,97,3.7,2.465,20.01],[15.5,8,318,150,2.76,3.52,16.87],[15.2,8,304,150,3.15,3.435,17.3],[13.3,8,350,245,3.73,3.84,15.41],[19.2,8,400,175,3.08,3.845,17.05],[27.3,4,79,66,4.08,1.935,18.9],[26,4,120.3,91,4.43,2.14,16.7],[30.4,4,95.1,113,3.77,1.513,16.9],[15.8,8,351,264,4.22,3.17,14.5],[19.7,6,145,175,3.62,2.77,15.5],[15,8,301,335,3.54,3.57,14.6],[21.4,4,121,109,4.11,2.78,18.6]]"])
	result = pprintSql(db('select * from dataset0 where mpg > 20'))
except Exception as e:
	result = pprintErr(e)