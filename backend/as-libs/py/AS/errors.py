class InsufficientArgumentsException(Exception):
    pass

class ColumnHeaderNotPresent(Exception):
	pass

class CannotEmbedListsOfDimensionGreaterThanTwo(Exception):
  pass

class CannotCoerceTo1DList(Exception):
  pass

class KilledException(Exception):
  pass