from AS.errors import ColumnHeaderNotPresent
import pandas as pd
# assumes lst is a list of columns -- not rows
def listToDataframeColumnwise(lst):
	Undefined = "NaN" #handling nonsquare yesod output
	for col in lst: 
		if not isinstance(col[0], basestring):
			raise ColumnHeaderNotPresent
		else:
			continue

	# handling nonsquare data
	maxVals = max([len(col) for col in lst]) - 1
	data = [{} for _ in range(maxVals)]
	for col in lst:
		for valIdx in range(len(col)-1):
			data[valIdx][col[0]] = col[valIdx + 1]

	return pd.DataFrame(data)

#assumes lst is list of rows
def listToDataframe(lst): 
	Undefined = "NaN" 
	#first row must contain all strings
	for colHeader in lst[0]:
		if not isinstance(colHeader, basestring):
			raise ColumnHeaderNotPresent
		else:
			continue

	maxVals = len(lst) - 1
	data = [{} for _ in range(maxVals)]
	for rowIdx in range(maxVals):
		row = lst[rowIdx + 1]
		for colIdx in range(len(lst[0])):
			header = lst[0][colIdx]
			data[rowIdx][header] = row[colIdx]

	return pd.DataFrame(data)