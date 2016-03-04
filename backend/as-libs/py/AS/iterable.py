import numpy as np
import pandas as pd
from AS.errors import CannotEmbedListsOfDimensionGreaterThanTwo, CannotCoerceTo1DList

class ASIterator:
    def __init__(self, data):
        self.data=data
        self.idx=0
    def __iter__(self):
        return self
    def next(self):
        try:
            result=self.data.get(self.idx)
            self.idx+=1
            return result
        except IndexError:
            raise StopIteration

# All AlphaSheets ranges have this type. This is a wrapper around numpy arrays that acts like a 
# 1D list if it is just a single row or a single column. Also provides extra methods that allow
# you to add, sum, and multiply ranges, and chain operations together. 
class ASIterable(object):
    @classmethod
    # an array might have an ASIterable constructor around it, or its elements might be
    # lists of ASIterable's. _unwrap basically converts such a thing into a vanilla list
    def _unwrap(cls, arr):
        # True if an ASIterable was passed in. 
        if issubclass(type(arr), ASIterable): 
            arr = arr.toList2D()
        if issubclass(type(arr), str):
            return arr
        try: 
            # Unwrap all levels deeper too if you're not a string
            # Might still not be robust, if doing [x for x in arr] doesn't get you something
            # roughly equivalent to the original array (like it does for lists or arrays).
            return [cls._unwrap(x) for x in arr]
        except TypeError:
            return arr 

    def __init__(self, arr):
        arr = ASIterable._unwrap(arr)
        dim = len(np.array(arr).shape)
        if (dim == 0): # if literal. TODO: do we actually want to support this, or throw an error?
            arr = [[arr]]
        elif (dim == 1): # if plain list
            arr = [[x] for x in arr]
        elif (dim > 3):
            raise CannotEmbedListsOfDimensionGreaterThanTwo

        self.name = None
        self.hidden = False
        self.arr = np.array(arr)
    
    ###########################################################################
    ### List typecasting

    def _isColumn(self): 
        return all(len(l) == 1 for l in self.arr)

    def _isRow(self):
        return len(self.arr) == 1

    def toList1D(self):
        if self._isRow():
            return self.arr.tolist()[0]
        elif self._isColumn(): 
            return [l[0] for l in self.arr]
        else: 
            raise CannotCoerceTo1DList

    def toList2D(self):
        return self.arr.tolist()

    def _setList(self, lst, rowCol=None): 
        if self._isColumn():
            self.arr = np.array([[x] for x in lst])
            return
        self.arr = np.array(lst)

    # If you're vertical, present yourself as a 1D list. Otherwise present yourself
    # as a 2D list. 
    def _to1DListIf1D(self):
        try: 
            return self.toList1D()
        except:
            return self.toList2D()

    ###########################################################################
    ### List overloading 

    def _apply1DListFunction(self, f):
        temp = self._to1DListIf1D()
        ret = f(temp)
        if self._isColumn():
            self.arr = np.array([[x] for x in temp])
        elif self._isRow():
            self.arr = np.array([temp])
        else:
            self.arr = np.array(temp)
        return ret

    def append(self, elem):
        self.insert(len(self), elem)

    def extend(self, L):
        return self._apply1DListFunction(lambda x: x.extend(L))

    def insert(self, idx, elem):
        return self._apply1DListFunction(lambda x: x.insert(idx, elem))

    def pop(self, i):
        return self._apply1DListFunction(lambda x: x.pop(i))

    def remove(self, x):
        return self._apply1DListFunction(lambda x: x.remove(x))

    def reverse(self):
        return self._apply1DListFunction(lambda x: x.reverse())

    def sort(self, comp=None, key=None, reverse=False):
        return self._apply1DListFunction(lambda x: x.sort(comp, key, reverse))

    def count(self, x):
        return self._apply1DListFunction(lambda y: y.count(x))

    def index(self, x):
        return self._apply1DListFunction(lambda y: y.index(x))

    ###########################################################################
    ### iteration

    # If it's a vertical or horizontal list, iterate through it like a 1D list. Otherwise
    # iterate through it as a list of lists. 
    def __getitem__(self, idx):
        return self._to1DListIf1D()[idx]

    def get(self, idx):
        return self.__getitem__(idx)

    def __iter__(self):
        return ASIterator(self)

    def __len__(self):
        return len(self._to1DListIf1D())

    def __eq__(self, elem):
        dim = len(np.array(self.arr.tolist()).shape)
        if dim==1:
            return [elem==x for x in self.arr]
        else:
            return [[elem==x for x in xlist] for xlist in self.arr]

    ###########################################################################
    ### Custom functions

    def toArray(self):
        return self.arr

    def len(self):
        return len(self)

    def sumWay(self, axis):
        return np.sum(self.arr, axis)

    def sum(self):
        return np.sum(self.arr)

    def reshape(self,axis1,axis2):
        return ASIterable(self.arr.reshape((axis1, axis2)))

    def transpose(self):
        return ASIterable(self.arr.transpose())

    def reversed(self):
        temp = ASIterable(self)
        temp.reverse()
        return temp

    def sorted(self, comp=None, key=None, reverse=False):
        temp = ASIterable(self)
        temp.sort(comp, key, reverse)
        return temp

    def t(self):
        return self.transpose()
    
    def dot(self, other):
        if isinstance(other, ASIterable):
            return ASIterable(np.dot(self.arr, other.arr))
        else: 
            return ASIterable(np.dot(self.arr, np.array(other)))

    def map(self, func):
        return ASIterable([func(x) for x in self])

    # pandas
    def toDataFrame(self):
        return pd.DataFrame(self.arr)

    ###########################################################################
    ### representations and conversions

    @classmethod
    def deserialize(cls, js):
        e = cls(js["lst"])
        if "name" in js:
            e.hide(js["name"])
        return e

    def serialize(self):
        if self.name is not None:
            return str({ "name": self.name, "lst": self.toList2D()})
        else: return str({"lst": self.toList2D()})

    def hide(self, name="HIDDEN LIST"):
        self.name = name
        self.hidden = True
        return self

    def unhide(self):
        self.name = None
        self.hidden = False
        return self

    def __repr__(self):
        if not self.hidden:
            return repr(self.toList2D())
        else:
            return str({ "displayValue": self.name, "objectType": "ASIterable", "jsonRepresentation": self.serialize() })

    def __str__(self):
        return self.__repr__()

## other functions

# def listToDataframeColumnwise(lst):
#     Undefined = "NaN" #handling nonsquare yesod output
#     for col in lst: 
#         if not isinstance(col[0], basestring):
#             raise ColumnHeaderNotPresent
#         else:
#             continue

#     # handling nonsquare data
#     maxVals = max([len(col) for col in lst]) - 1
#     data = [{} for _ in range(maxVals)]
#     for col in lst:
#         for valIdx in range(len(col)-1):
#             data[valIdx][col[0]] = col[valIdx + 1]

#     return pd.DataFrame(data)

# #assumes lst is list of rows
# def listToDataframe(lst): 
#     if len(np.array(lst).shape) == 1:
#         return listToDataframe([[a] for a in lst])
#     else :
#         Undefined = "NaN" 
#         #first row must contain all strings
#         for colHeader in lst[0]:
#             if not isinstance(colHeader, basestring):
#                 raise ColumnHeaderNotPresent
#             else:
#                 continue

#         maxVals = len(lst) - 1
#         data = [{} for _ in range(maxVals)]
#         for rowIdx in range(maxVals):
#             row = lst[rowIdx + 1]
#             for colIdx in range(len(lst[0])):
#                 header = lst[0][colIdx]
#                 data[rowIdx][header] = row[colIdx]

#         return pd.DataFrame(data)