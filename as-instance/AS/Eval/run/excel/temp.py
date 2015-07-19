
#ADD COMMANDS HERE
from AS.excel.pycel.excelcompiler import *
from AS.excel.pycel.excellib import * # mapping from excel to python
from AS.excel.io import evalExcel
from sys import exc_info
try:
	
	
	result = sumproduct([0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0],[0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0])
except Exception as e: 
	exc_type, exc_obj, exc_tb = exc_info()
	fname = 'AlphaSheets Python evaluator'
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 8 # subtract template lines
	errJson = {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	print(errJson)