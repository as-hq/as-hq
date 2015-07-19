
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

from sqlalchemy import create_engine
from pandas import read_sql_query

pysqldf = lambda q: sqldf(q, globals())


def db(dbCmd,dbName=""):
	try:
		return pysqldf(dbCmd).head()
	except:
		try:
			e = create_engine('sqlite:///'+dbName) #absolute file path
			return read_sql_query(dbCmd,e)
		except Exception as e: 
			return e

def setGlobals(context):
	for i in range(len(context)):
		globals()["dataset"+str(i)] = listToDataframe([row for row in eval(context[i])])

setGlobals([])

print(pprint(db("select * from Cars","/home/riteshr/testRit.db")))