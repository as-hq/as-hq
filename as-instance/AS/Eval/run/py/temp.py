
#ADD COMMANDS HERE
from AS.stdlib import *
from AS.ui.styling import *
# from AS.tests.min import *
from AS.instruments.ETF import ETF
from AS.instruments.Stock import Stock
from AS.ui.plot import *
#import json
# from AS.excel.pycel.excelcompiler import *
# from AS.excel.pycel.excellib import * # mapping from excel to python
from sys import exc_info
try:
	
	
	print(repr(plotObj([Stock.deserialize({'source': 'yahoo', 'symbol': 'YHOO', 'data': {'Volume': '13921000', 'Symbol': 'YHOO', 'Adj_Close': '41.66', 'High': '41.73', 'Low': '41.209999', 'Date': '2015-05-06', 'Close': '41.66', 'Open': '41.310001'}}),Stock.deserialize({'source': 'yahoo', 'symbol': 'YHOO', 'data': {'Volume': '19278500', 'Symbol': 'YHOO', 'Adj_Close': '41.299999', 'High': '42.00', 'Low': '40.810001', 'Date': '2015-05-05', 'Close': '41.299999', 'Open': '41.860001'}}),Stock.deserialize({'source': 'yahoo', 'symbol': 'YHOO', 'data': {'Volume': '14455200', 'Symbol': 'YHOO', 'Adj_Close': '42.040001', 'High': '42.549999', 'Low': '41.830002', 'Date': '2015-05-04', 'Close': '42.040001', 'Open': '42.50'}}),Stock.deserialize({'source': 'yahoo', 'symbol': 'YHOO', 'data': {'Volume': '10457300', 'Symbol': 'YHOO', 'Adj_Close': '42.509998', 'High': '42.68', 'Low': '42.09', 'Date': '2015-05-01', 'Close': '42.509998', 'Open': '42.549999'}})])))
except Exception as e: 
	exc_type, exc_obj, exc_tb = exc_info()
	fname = 'AlphaSheets Python evaluator'
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 15 # subtract template lines
	errJson = {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	print(errJson)