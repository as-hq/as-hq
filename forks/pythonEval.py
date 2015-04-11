import re

def py_eval(cells,expression):
        
        def cell_to_python(cell):
                alphNumSplit = (re.match(r"([A-Z]+)([0-9]+)", cell, re.I)).groups()
                numForLetters = sum((ord(char) - 64)*(26**i) for (i, char) in enumerate(alphNumSplit[0][::-1]))
                return (numForLetters, int(alphNumSplit[1])) 

        def excel_range_to_python_list(excelRange):
                # doesn't deal with items out of range
                indices = [cell_to_python(excelRange.split(":")[i]) for i in [0, 1]]
                # next like takes submatrix based on the range
                unFlatList=[cells[row][indices[0][1]-1:indices[1][1]] for row in range(indices[0][0]-1, indices[1][0])]
                return [item for sublist in unFlatList for item in sublist]

        def excel_cell_to_python_str (cell):
                return str(cells[cell_to_python(cell)[0]-1][cell_to_python(cell)[1]-1])

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

#cells = [["sum(B1:B4)","2","A1","4"],["C1","6","7","8"],["sum(B2:C4)","10","11","12"]]

#print py_eval(cells,"A1")                  #75
#print py_eval(cells,"sum(A1:B2)")          #137
#print py_eval(cells,"sum(A1:A2)")          #77
#print py_eval(cells,"A1+B2+sum(B2:B3)")    #94
