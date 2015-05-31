
#ADD COMMANDS HERE
# from AS.stdlib import *
# from AS.ui.styling import *
# from AS.tests.min import *
# from AS.instruments.ETF import ETF
# from AS.instruments.Stock import Stock
# from AS.ui.plot import *
import json
import sys, os
import traceback
from AS.pandas.input import listToDataframe
from AS.pandas.output import pprint
from pandasql import sqldf

pysqldf = lambda q: sqldf(q, globals())

try:
	queryStr = 'SELECT * FROM A1:B5 LIMIT 2 '.replace('\n',' ')
	(rangeStr, data) = ('A1:B5', [["a","b"],[1.0,5.0],[2.0,6.0],[3.0,7.0],[4.0,8.0]])
	queryStr = queryStr.replace(rangeStr, "dataset")
	dataset = listToDataframe(data)
	cleaned = pprint(pysqldf(queryStr).head())
	print(cleaned)

except Exception as e: 
	exc_type, exc_obj, exc_tb = sys.exc_info()
	fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 15 # subtract template lines
	errJson = {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	print(errJson)