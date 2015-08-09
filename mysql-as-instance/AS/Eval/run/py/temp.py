
execfile("/home/anand/Development/as/asl-demo/mysql-as-instance/AS/Eval/run/py/repl_record.py")
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
result = "error"
try:
	
	
	result = ETF.deserialize({'name': 'BP', 'bid': 43.34, 'rd': 0.1, 'weights': [0.0, 0.0, 6.0], 'ask': 43.37, 'cr': 0.1, 'ords': [{'ask': 2068.5, 'bid': 2066.0, 'name': 'RDSA LN'}, {'ask': 612.2, 'bid': 610.8, 'name': 'HSBC LN'}, {'ask': 483.15, 'bid': 478.4, 'name': 'BP LN'}]}).ask
except Exception as e: 
	exc_type, exc_obj, exc_tb = exc_info()
	fname = 'AlphaSheets Python evaluator'
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 20 # subtract template lines
	errJson = {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	result = errJson