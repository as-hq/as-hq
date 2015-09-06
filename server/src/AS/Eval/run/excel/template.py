import os
filename = os.getcwd() + "/repl_record.py"
execfile(filename)

from AS.excel.pycel.excelcompiler import *
from AS.excel.pycel.excellib import * # mapping from excel to python
from AS.excel.io import evalExcel
from sys import exc_info
result = "EXERROR"
try:
	#CMD#
except Exception as e: 
	exc_type, exc_obj, exc_tb = exc_info()
	fname = 'AlphaSheets Python evaluator'
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 9 # subtract template lines
	errJson = {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	result = errJson