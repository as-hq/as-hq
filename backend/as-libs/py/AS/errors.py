class InsufficientArgumentsException(Exception):
    pass

class ColumnHeaderNotPresent(Exception):
	pass

class CannotEmbedListsOfDimensionGreaterThanTwo(Exception):
  pass

class CannotCoerceTo1DList(Exception):
  pass

class TimeoutException(Exception):
  def __repr__(self):
    return "Killed by a timeout exception."
