import os
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
<<<<<<< HEAD
	def foobar(x):
		x**2
	
	result = a = foobar(15)
=======
	sorted([3,2,4])
	result = sorted([3,2,4])
>>>>>>> 63d929ce103d0b98781c3510423d650178336828
except Exception as e: 
	exc_type, exc_obj, exc_tb = exc_info()
	fname = 'AlphaSheets Python evaluator'
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 20 # subtract template lines
	errJson = {'errType': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	result = errJson