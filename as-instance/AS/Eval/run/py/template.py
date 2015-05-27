#ADD COMMANDS HERE
from AS.stdlib import *
from AS.ui.styling import *
from AS.tests.min import *
from AS.instruments.ETF import ETF
from AS.ui.plot import *
import json
import sys, os
import traceback

try:
	#CMD#
except Exception as e: 
	exc_type, exc_obj, exc_tb = sys.exc_info()
	fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
	# err = traceback.format_exc().replace("'","")
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 12 # subtract template lines
	errJson = {'err_type': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	print(errJson)