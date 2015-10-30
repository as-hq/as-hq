import numpy as np
import pandas as pd
from AS.pandas.input import listToDataframe

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
    def _unwrap(cls, arr):
        # True if an ASIterable was passed in
        if type(arr).__name__ == "ASIterable":
            arr = arr.toArray()
        if type(arr).__name__ == 'str':
            return arr
        try: 
            # Unwrap all levels deeper too if you're not a string
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

        self.name = None
        self.hidden = False
        self.arr = np.array(arr)
    
    ###########################################################################
    ### List typecasting
    def _isRow(self): 
        return len(self.arr) == 1

    def _isColumn(self): 
        return all(len(l) == 1 for l in self.arr)

    def _is1D(self): 
        return self._isRow() or self._isColumn()

    # Get the underlying list if it's a row or column matrix
    def _getList(self): 
        if self._isColumn():
            return [l[0] for l in self.arr]
        if self._isRow():
            return self.arr[0].tolist()
        raise # ::ALEX::

    def _setList(self, lst): 
        if self._isColumn():
            self.arr = np.array([[x] for x in lst])
            return
        if self._isRow():
            self.arr = np.array([lst])
            return
        raise # ::ALEX::

    def _flattenedArrIfCol(self):
        if self._isColumn():
            return np.array([l[0] for l in self.arr])
        else:
            return self.arr

    ###########################################################################
    ### List overloading 

    # #needsrefactor: so much repetetion of code here. Could definitely be better. 
    def append(self, elem):
        self.insert(len(self), elem)

    def extend(self, L):
        temp = self._getList()
        temp.extend(L)
        self._setList(temp)

    def insert(self, idx, elem):
        temp = self._getList()
        temp.insert(idx, elem)
        self._setList(temp)

    def pop(self, i):
        temp = self._getList()
        item = temp.pop(i)
        self._setList(temp)
        return item

    def remove(self, x):
        temp = self._getList()
        item = temp.remove(x)
        self._setList(temp)
        return item 

    def reverse(self):
        temp = self._getList()
        temp.reverse()
        self._setList(temp)

    def sort(self, comp=None, key=None, reverse=False):
        temp = self._getList()
        temp.sort(comp, key, reverse)
        self._setList(temp)

    def count(self, x):
        temp = self._getList()
        c = temp.count(x)
        return c

    def index(self, x):
        temp = self._getList()
        ind = temp.index(x)
        return ind

    ###########################################################################
    ### iteration

    def __getitem__(self, idx):
        if self._is1D():
            return self._getList()[idx]
        else: 
            return self.toList()[idx]

    def get(self, idx):
        return self.__getitem__(idx)

    def __iter__(self):
        return ASIterator(self)

    def __len__(self):
        if self._is1D():
            return len(self._getList())
        else:
            return len(self.arr)

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
    
    def toList(self):
        return self.arr.tolist()

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
    def toDataframe(self):
        return listToDataframe(self.arr)

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
                return ASIterable(np.multiply(self.arr, other.arr))
            except: return "undefined"
        else: 
            try:
                return ASIterable(np.multiply(self.arr, np.array(other)))
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
            return repr(self._flattenedArrIfCol().tolist())
        else:
            return str({ "displayValue": self.name, "objectType": "ASIterable", "jsonRepresentation": self.serialize() })

    def __str__(self):
        return self.__repr__()