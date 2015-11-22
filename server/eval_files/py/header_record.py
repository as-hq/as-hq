import os
curWd = os.getcwd()
filename = os.getcwd() + "/eval_files/py/repl_record.py"
execfile(filename)

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
result = ""
try:
	def buildApiUrl(lat, lon):
		return "http://maps.googleapis.com/maps/api/geocode/json?latlng=" + str(lat) + "," + str(lon) + "&sensor=false"
	
	def getAddressAtCoords(lat, lon):
		url = buildApiUrl(lat, lon)
		filename = wget.download(url, out="blahblahblah")
		f = open(filename, 'r')
		l = f.read()
		f.close()
		os.remove(filename)
		addressesJson = json.loads(l)
		results = addressesJson['results']
		if len(results) == 0:
			return results
		else: 
			return results
	
	result = buildApiUrl(10,10)
  result = serialize(result)
except Exception as e:
  os.chdir(curWd)
  print "exception"
  exc_type, exc_obj, exc_tb = exc_info()
  err = repr(e).replace("\'","").replace("'",'"')
  errJson = {'tag': 'CellValue', 'cellValueType': 'Error', 'errorType': repr(exc_type), 'errorMsg': err}
  result = json.dumps(errJson)