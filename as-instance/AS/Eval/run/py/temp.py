
#ADD COMMANDS HERE
from AS.stdlib import *
from AS.ui.styling import *
from AS.instruments.ETF import ETF
from AS.ui.plot import *
import json
import sys, os
import traceback

try:
	
	print(repr(green("Dy")))
except Exception as e: 
	exc_type, exc_obj, exc_tb = sys.exc_info()
	fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
	err = traceback.format_exc()
	errJson = {'err_type': repr(exc_type), 'file': fname, 'position': exc_tb.tb_lineno, 'error': err}
	print(errJson)