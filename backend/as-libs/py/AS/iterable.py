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
            arr = arr.tolist2d()
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
        self.arr = arr # the underlying array -- all ranges are internally represented as a 2D list. 
    
    ###########################################################################
    ### List typecasting

    def _isColumn(self): 
        return all(len(l) == 1 for l in self.arr)

    def _isRow(self):
        return len(self.arr) == 1

    def tolist1d(self):
        if self._isRow():
            return self.arr[0]
        elif self._isColumn(): 
            return [l[0] for l in self.arr]
        else: 
            raise CannotCoerceTo1DList

    def tolist2d(self):
        return self.arr

    # If you're vertical, present yourself as a 1D list. Otherwise present yourself
    # as a 2D list. 
    def _to1DListIf1D(self):
        try: 
            return self.tolist1d()
        except:
            return self.tolist2d()

    ###########################################################################
    ### List overloading 

    def _apply1DListFunction(self, f):
        temp = self._to1DListIf1D()
        ret = f(temp)
        if self._isColumn():
            self.arr = [[x] for x in temp]
        elif self._isRow():
            self.arr = [temp]
        else:
            self.arr = temp
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

    # Does not yet handle setting equalities like "x = A1:B2; x[0][0] = 15; x"
    def __setitem__(self, idx, val):
        self._apply1DListFunction(lambda x: x.__setitem__(idx, val))

    def get(self, idx):
        return self.__getitem__(idx)
        
    def __iter__(self):
        return ASIterator(self)

    def __len__(self):
        return len(self._to1DListIf1D())

    def __eq__(self, elem):
        dim = self.dimension()
        if dim==1:
            return [elem==x for x in self.arr]
        else:
            return [[elem==x for x in xlist] for xlist in self.arr]

    ###########################################################################
    ### Custom functions

    def transpose(self):
        return ASIterable(np.array(self.arr).transpose())

    def reversed(self):
        temp = ASIterable(self)
        temp.reverse()
        return temp

    def t(self):
        return self.transpose()
    
    def map(self, func):
        return ASIterable([func(x) for x in self])

    # pandas
    def toDataFrame(self):
        return pd.DataFrame(self.arr)

    def dimension(self):
        return np.array(self.arr).ndim

    ###########################################################################
    ### representations and conversions

    def __repr__(self):
        if not self.hidden:
            return repr(self.tolist2d())
        else:
            return str({ "displayValue": self.name, "objectType": "ASIterable", "jsonRepresentation": self.serialize() })

    def __str__(self):
        return self.__repr__()

