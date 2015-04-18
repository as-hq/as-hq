import re
from pymongo import MongoClient

client = MongoClient('localhost', 27017)
db = client.asinstance
collection = db.ASCell # ensure db is migrated to include ASCell collection

def cells(a,b):
    collection


def py_eval(cells,expression):
        
        def cell_to_python(cell):
                alphNumSplit = (re.match(r"([A-Z]+)([0-9]+)", cell, re.I)).groups()
                numForLetters = sum((ord(char) - 64)*(26**i) for (i, char) in enumerate(alphNumSplit[0][::-1]))
                return (numForLetters, int(alphNumSplit[1])) 

        def excel_range_to_python_list(excelRange):
                # doesn't deal with items out of range
                indices = [cell_to_python(excelRange.split(":")[i]) for i in [0, 1]]
                # next like takes submatrix based on the range
                unFlatList=cells([indices[0][0]-1, indices[1][0]], [indices[0][1]-1,indices[1][1]])
                # check if row or column vector, else return 2d list
                if (indices[0][0] == indices [1][0]) or (indices[0][1] == indices [1][1]): 
                        return [item for sublist in unFlatList for item in sublist]
                else: return unFlatList

        def excel_cell_to_python_str (cell):
                return repr(cells(cell_to_python(cell)[0]-1, cell_to_python(cell)[1]-1))

        def parse(expression):
                # doesn't deal with circular references, which will cause an infinite loop
                # logic: update ranges as much as possible, then update all cells, and recurse
                expression = expression.replace(" ", "")
                rangeRegex = r"([A-Z]+[0-9]+:[A-Z]+[0-9]+)"
                cellRegex = r"([A-Z]+[0-9]+)"
                change=True
                while change:
                        changeRange=True
                        changeCells=True
                        orig=expression
                        while changeRange:
                                sub_ranges=re.sub(rangeRegex,lambda x: str(excel_range_to_python_list(x.group())),expression)
                                changeRange=(sub_ranges!=expression)
                                expression=sub_ranges
                        # look for as many ranges as possible
                        expression=re.sub(cellRegex,lambda x: str(excel_cell_to_python_str(x.group())),expression)
                        # only run cell lookup once, because this might introduce ranges
                        change=(orig!=expression)
                return eval(expression.replace("'","")) #no string expressions, or else eval won't work

        return parse(expression)


def scrubExcel(content):
	lines = [l.split(':') for l in content]
	inc = 0
	for i in range(len(lines)):
		for j in range(len(lines[i]-1)):
			expr = lines[i,j][-2:]+':'+lines[i,j+1][:2]
			result = None
			try:
				result = py_eval(sheet, expr)
			except Exception as e:
				print "handle this error: "+e.args
				continue
			varlist.append(result)
			lines[i,j] = lines[i,j][:-2] + 'varList['+repr(inc)+']'
			lines[i,j+1] = lines[i,j+1][2:]
	contentP = [[item for sublist in line for item in sublist] for line in lines]
	return contentP


def exe(scriptPath):
	with open(scriptPath, 'r') as f:
		content = scrubExcel(f)
		content[-1] = 'return ' + content[-1].split('=')[-1]
	with open(scriptPath, 'w') as f: f.writelines(content)
	execfile(scriptPath)
	return ValueError('No result produced!')


