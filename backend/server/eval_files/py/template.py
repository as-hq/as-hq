import os
curWd = os.getcwd()
replFile =  os.getcwd() + "/eval_files/py/repl_record.py"
headerFile =  os.getcwd() + "/eval_files/py/header.py"
imagesPath = os.getcwd() + "/static/images/"
imagePrefix = "pythonImage"


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
from AS.iterable import ASIterable

import json
import matplotlib._pylab_helpers

result = "error"

def uniqueId():
	lstFiles = os.listdir(imagesPath)
	pythonImageFiles = filter(lambda s: s.startswith(imagePrefix),lstFiles)
	pythonNumbers = map(lambda s: int(s[len(imagePrefix):-4]),pythonImageFiles)
	newNumber = 1
	if len(pythonNumbers) > 0:
		newNumber = max(pythonNumbers) + 1
	return imagePrefix + str(newNumber) + ".png"

try:
	# execfile(replFile)
	# execfile(headerFile)
	os.chdir(os.getcwd()+"/static")
	#HEADER#
	#CODE#
	os.chdir(curWd)
	figures=[manager.canvas.figure for manager in matplotlib._pylab_helpers.Gcf.get_all_fig_managers()]
	if len(figures) > 0:
		uid = uniqueId()
		filePath = imagesPath+uid
		figures[-1].savefig(filePath)
		matplotlib._pylab_helpers.Gcf.destroy_all()
		result = json.dumps({'tag': 'CellValue', 'cellValueType': 'Image', 'imagePath': uid})
	else:
		result = serialize(result)
except Exception as e:
	os.chdir(curWd)
	print "exception"
	exc_type, exc_obj, exc_tb = exc_info()
	err = repr(e).replace("\'","").replace("'",'"')
	errJson = {'tag': 'CellValue', 'cellValueType': 'Error', 'errorType': repr(exc_type), 'errorMsg': err}
	result = json.dumps(errJson)
