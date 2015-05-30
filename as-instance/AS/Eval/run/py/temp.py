
#ADD COMMANDS HERE
from AS.stdlib import *
from AS.ui.styling import *
from AS.tests.min import *
from AS.instruments.ETF import ETF
from AS.instruments.Stock import Stock
from AS.ui.plot import *
import json
import sys, os
import traceback

try:
	
	
	print(repr(rand(3,3)))
except Exception as e: 
	exc_type, exc_obj, exc_tb = sys.exc_info()
	fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
	# err = traceback.format_exc().replace("'","")
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 15 # subtract template lines
	errJson = {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	print(errJson)