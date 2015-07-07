
#ADD COMMANDS HERE
#from AS.stdlib import *
#from AS.ui.styling import *
#from AS.tests.min import *
from AS.instruments.ETF import ETF
#from AS.instruments.Stock import Stock
#from AS.ui.plot import *
# AS.pycel.excelcompiler import *
#from AS.pycel.excellib import * # mapping from excel to python
from sys import exc_info
try:
    
	from AS.ui.plot import *
	print(repr( testStockChart()))
except Exception as e: 
    exc_type, exc_obj, exc_tb = exc_info()
    fname = 'AlphaSheets Python evaluator'
    # err = traceback.format_exc().replace("'","")
    err = repr(e).replace("\'","").replace("'",'"')
    pos = exc_tb.tb_lineno - 14 # subtract template lines
    errJson = {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
    print(errJson)
