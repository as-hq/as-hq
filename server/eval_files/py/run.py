import os
replFile =  os.getcwd() + "/eval_files/py/repl_record.py"
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

def uniqueId():
	print imagesPath
	lstFiles = os.listdir(imagesPath)
	print lstFiles
	pythonImageFiles = filter(lambda s: s.startswith(imagePrefix),lstFiles)
	print pythonImageFiles
	pythonNumbers = map(lambda s: int(s[len(imagePrefix):-4]),pythonImageFiles)
	print pythonNumbers
	newNumber = 1
	if len(pythonNumbers) > 0:
		newNumber = max(pythonNumbers) + 1
	print newNumber
	return imagePrefix + str(newNumber) + ".png"

result = "error"

# NOTE: if you make a plot anywhere in the expression, then do something else like '1+1',
# only the ValueImage will be returned, and not the ValueD.
# this is because the if statement below will always produce ValueImage
# if there exist any plots in the current environment.
# a workaround would be to check the type of result before setting it to ValueImage.
try:
	print replFile
	execfile(replFile)
	result = arr([3,1,None]).reversed()
	figures=[manager.canvas.figure for manager in matplotlib._pylab_helpers.Gcf.get_all_fig_managers()]
	print figures
	if len(figures) > 0:
		uid = uniqueId()
		print uid
		filePath = imagesPath+uid
		print filePath
		figures[-1].savefig(filePath)
		print "saved"
		matplotlib._pylab_helpers.Gcf.destroy_all()
		print "finish line"
		result = {"imagePath":uid}
	print result
except Exception as e:
	exc_type, exc_obj, exc_tb = exc_info()
	fname = 'AlphaSheets Python evaluator'
	err = repr(e).replace("\'","").replace("'",'"')
	pos = exc_tb.tb_lineno - 20 # subtract template lines
	errJson = {'errType': repr(exc_type), 'file': fname, 'position': pos, 'error': err}
	result = errJson
