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

import matplotlib._pylab_helpers

def arr(lst):
	return ASIterable(lst)

def hide(lst): 
	return ASIterable(lst).hide()

def uniqueId():
	print imagesPath, os.getcwd()
	lstFiles = os.listdir(imagesPath)
	print imagesPath,lstFiles
	pythonImageFiles = filter(lambda s: s.startswith(imagePrefix),lstFiles)
	pythonNumbers = map(lambda s: int(s[len(imagePrefix):-4]),pythonImageFiles)
	print pythonNumbers
	newNumber = 1
	if len(pythonNumbers) > 0:
		newNumber = max(pythonNumbers) + 1
	return imagePrefix + str(newNumber) + ".png"

result = "error"

# NOTE: if you make a plot anywhere in the expression, then do something else like '1+1',
# only the ValueImage will be returned, and not the ValueD.
# this is because the if statement below will always produce ValueImage
# if there exist any plots in the current environment.
# a workaround would be to check the type of result before setting it to ValueImage.
try:
	# execfile(replFile)
	# execfile(headerFile)
	os.chdir(os.getcwd()+"/static")
	#HEADER#
	#CODE#
	os.chdir('..')
	print result
	figures=[manager.canvas.figure for manager in matplotlib._pylab_helpers.Gcf.get_all_fig_managers()]
	if len(figures) > 0:
		print "found figure"
		uid = uniqueId()
		print uid
		filePath = imagesPath+uid
		print filePath
		figures[-1].savefig(filePath)
		print "saved"
		matplotlib._pylab_helpers.Gcf.destroy_all()
		print "destroyed figs"
		result = {"imagePath":uid}
except Exception as e:
	os.chdir(curWd)
	print "exception"
	exc_type, exc_obj, exc_tb = exc_info()
	fname = 'AlphaSheets Python evaluator'
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 20 # subtract template lines
	errJson = {'errType': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	result = errJson
