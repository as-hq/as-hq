import openpyxl as pyx

import json
import os

# os.getcwd on localhost should be alpha-sheets/backend
sheetSavePath = os.path.dirname(os.getcwd()) + '/server/static/'

class Sheet(object):
  def __init__(self, cells, otherData=None):
    self.cells = cells
    self.otherData = otherData

  def serialize(self):
    print(self.cells)
    return json.dumps({'tag': 'ExcelSheet', 'cells': [cell.toJSON() for cell in self.cells]})

class Cell(object):
    """A cell object used only for storing data from Excel files.

    Attributes:
          location: (Col, Row)
          formula: The excel formula in the cell. E.g. "=A1+1"
          value: value in the cell. E.g. "=1"
    """

    def __init__(self, location="", formula="", value=""):
      self.location = location
      self.formula = formula
      self.value = value

    def toJSON(self):
      return {'formula': self.getFormula(), \
              'location': self.getLocation(), \
              'value': self.getValue()}

    def getLocation(self):
      return self.location

    def getFormula(self):
      return self.formula

    def getValue(self):
      return self.value

    def setLocationFormulaValue (self, loc, formula, value):
      self.setLocation(loc)
      self.setFormula(formula)
      self.setValue(value)

    def setLocation(self, loc):
      self.location = loc

    def setFormula(self, formula):
      self.formula = formula

    def setValue(self, val):
      self.value = val

# generic way to extract cells from sheet.
# TODO: add an extra sheetName.
def consumeSheet(wbName, dataOnly=False):
  wbName = sheetSavePath + wbName 
  cellValues = []
  wb = pyx.load_workbook(filename=wbName, read_only=True, data_only=dataOnly)
  ws = wb.active
  for row in ws.rows:
    for cell in row:
      if cell.value is not None:
        cellValues.append(cell)
  return cellValues

def getFormulaCells(wbName):
    return consumeSheet(wbName, False)

def getValueCells(wbName):
    return consumeSheet(wbName, True)

def readSheet(wbName):
  cells = []
  locs = getLocations(wbName)
  formulas = getFormulas(wbName)
  vals = getValues(wbName)

  """ locs, formulas, and vals should all be the same length"""
  if (len(locs) != len(formulas) and len (locs) != len(vals)):
      print("Error: locs, vals, and formulas are not synced up.")

  for i in range(len(locs)):
    c = Cell()
    c.setLocationFormulaValue(locs[i], formulas[i], vals[i])
    cells.append(c)
  return Sheet(cells)

def getLocations(wbName):
    return [str((cell.column, cell.row)) for cell in getValueCells(wbName)]
def getFormulas (wbName):
    return [str(cell.value) for cell in getFormulaCells(wbName)]
def getValues(wbName):
    return [str(cell.value) for cell in getValueCells(wbName)]
