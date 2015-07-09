
#ADD COMMANDS HERE
from AS.excel.pycel.excelcompiler import *
from AS.excel.pycel.excellib import * # mapping from excel to python
from AS.excel.io import evalExcel
from sys import exc_info
try:
	
	
	print(repr({'err_type': "<type 'exceptions.Exception'>", 'position': 2, 'file': 'AlphaSheets Python evaluator', 'error': 'Exception(Invalid address format \\t[RANGE(5) FOR _ IN RANGE(5)],)'}))
except Exception as e: 
	exc_type, exc_obj, exc_tb = exc_info()
	fname = 'AlphaSheets Python evaluator'
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 8 # subtract template lines
	errJson = {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	print(errJson)