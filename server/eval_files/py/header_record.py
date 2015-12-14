import os
curWd = os.getcwd()
filename = os.getcwd() + "/eval_files/py/repl_record.py"
execfile(filename)

from AS.stdlib import *
from AS.ui.styling import *
# from AS.tests.min import *
from AS.instruments.ETF import ETF
from AS.instruments.Stock import Stock
#from AS.ui.plot import *
#import json
# from AS.excel.pycel.excelcompiler import *
# from AS.excel.pycel.excellib import * # mapping from excel to python
from sys import exc_info
from AS.iterable import ASIterable

def arr(lst):
	return ASIterable(lst)
result = ""
try:
	1+1
	result = 1+1
	result = serialize(result)
except Exception as e:
	os.chdir(curWd)
	print "exception"
	exc_type, exc_obj, exc_tb = exc_info()
	err = repr(e).replace("\'","").replace("'",'"')
	errJson = {'tag': 'CellValue', 'cellValueType': 'Error', 'errorType': repr(exc_type), 'errorMsg': err}
	result = json.dumps(errJson)