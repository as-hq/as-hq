import numpy as np
import pandas as pd

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
            arr = arr.toList()
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
            raise "Cannot embed lists of dimension > 2"

        self.name = None
        self.hidden = False
        self.arr = np.array(arr)
    
    ###########################################################################
    ### List typecasting
    def _isColumn(self): 
        return all(len(l) == 1 for l in self.arr)

    # Get the underlying list if it's a column matrix
    def toList(self):
        if self._isColumn():
            return [l[0] for l in self.arr]
        else: 
            return self.arr.tolist()

    def _setList(self, lst): 
        if self._isColumn():
            self.arr = np.array([[x] for x in lst])
            return
        self.arr = np.array(lst)

    # If you're vertical, present yourself as a 1D list. Otherwise present yourself
    # as a 2D list. 
    # def _to1dListIf1d(self):
    #     if self._is1D():
    #         return self._getList()
    #     else:
    #         return self.arr.tolist()

    ###########################################################################
    ### List overloading 

    # #needsrefactor: so much repetetion of code here. Could definitely be better. 
    def append(self, elem):
        self.insert(len(self), elem)

    def extend(self, L):
        temp = self.toList()
        temp.extend(L)
        self._setList(temp)

    def insert(self, idx, elem):
        temp = self.toList()
        temp.insert(idx, elem)
        self._setList(temp)

    def pop(self, i):
        temp = self.toList()
        item = temp.pop(i)
        self._setList(temp)
        return item

    def remove(self, x):
        temp = self.toList()
        item = temp.remove(x)
        self._setList(temp)
        return item 

    def reverse(self):
        temp = self.toList()
        temp.reverse()
        self._setList(temp)

    def sort(self, comp=None, key=None, reverse=False):
        temp = self.toList()
        temp.sort(comp, key, reverse)
        self._setList(temp)

    def count(self, x):
        temp = self.toList()
        c = temp.count(x)
        return c

    def index(self, x):
        temp = self.toList()
        ind = temp.index(x)
        return ind

    ###########################################################################
    ### iteration

    # Does NOT iterate through the elements of a horizontal list; only iterates
    # through the list itself. 

    def __getitem__(self, idx):
        return self.toList()[idx]

    def get(self, idx):
        return self.__getitem__(idx)

    def __iter__(self):
        return ASIterator(self)

    def __len__(self):
        return len(self.toList())

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
    ### custom operator functions

    def __add__(self, other):
        if isinstance(other, ASIterable):
            try:
                return ASIterable(self.arr + other.arr)
            except: return "undefined"
        elif isinstance(other, list):
            try:
                return ASIterable(self.arr + np.array(other))
            except: return "undefined"
        else: return "undefined"

    def __div__(self, k):
        if isinstance(k, (int, long, float, complex)) and k != 0:
            return ASIterable(self.arr / k)
        elif isinstance(k, ASIterable) and not np.any(k.arr==0):
            return ASIterable(np.divide(self.arr, k.arr))
        elif isinstance(k, list) and not 0 in k:
            return ASIterable(np.divide(self.arr, np.array(k)))
        else: return "undefined"

    def __sub__(self, other):
        if isinstance(other, ASIterable):
            return ASIterable(np.subtract(self.arr,other.arr))
        else:
            try:
                return ASIterable(np.subtract(self.arr, other))
            except: return "undefined"

    def __neg__(self):
        return ASIterable(-self.arr)

    def __pos__(self):
        return self;

    def __abs__(self):
        return ASIterable(np.abs(self.arr))

    def __mul__(self, other):
        if isinstance(other, ASIterable):
            try:
                return ASIterable(np.dot(self.arr, other.arr))
            except: return "undefined"
        else: 
            try:
                return ASIterable(np.dot(self.arr, np.array(other)))
            except: return "undefined"

    def __pow__(self, other):
        if isinstance(other, ASIterable):
            try:
                return ASIterable(np.power(self.arr,other.arr))
            except: return "undefined"
        else:
            try:
                return ASIterable(np.power(self.arr,other))
            except: return "undefined"

    def __rmul__(self, other):
        return self.__mul__(other)


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
            return str({ "name": self.name, "lst": self.toList()})
        else: return str({"lst": self.toList()})

    def hide(self, name="[HIDDEN LIST]"):
        self.name = name
        self.hidden = True
        return self

    def unhide(self):
        self.name = None
        self.hidden = False
        return self

    def __repr__(self):
        if not self.hidden:
            return repr(self.toList())
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