class InsufficientArgumentsException(Exception):
    pass

class ColumnHeaderNotPresent(Exception):
	pass

class MultiDimensionalDataException(Exception):
	pass

class OneDimCalledOnNonListObject(Exception):
  pass

class OneDimCalledOnNonRowOrCol(Exception):
  pass