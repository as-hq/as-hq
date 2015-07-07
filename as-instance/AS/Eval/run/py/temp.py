
#ADD COMMANDS HERE
#from AS.stdlib import *
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
	
try:
	from AS.ui.plot import *
	
	print(repr(plot(arr([19.59,62.57,46.66,43.34]),arr([19.62,62.59,46.7,43.37]),"bid vs ask")))
except Exception as e: 
	exc_type, exc_obj, exc_tb = exc_info()
	fname = 'AlphaSheets Python evaluator'
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 15 # subtract template lines
	errJson = {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	print(errJson)