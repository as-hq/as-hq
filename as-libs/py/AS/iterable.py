import numpy as np

# purpose of this class is to be able to chain operations nicely
class ASIterable(object):
    def __init__(self, lst):
        self.lst = np.array(lst)
    def get(self, idx):
        return self.lst[idx]
    def head(self):
        return self.lst[0]
    def tail(self):
        return self.lst[1:]
    def init(self):
        return self.lst[:-1]
    def last(self):
        return self.lst[-1]
    def push(self, elem):
        temp = self.lst.tolist()
        temp.insert(0,elem)
        self.lst = np.array(temp)
    def append(self, elem):
        temp = self.lst.tolist()
        temp.append(elem)
        self.lst = np.array(temp)
    def insert(self, elem, idx):
        temp = self.lst.tolist()
        temp.insert(idx, elem)
        self.lst = np.array(temp)
    def take(self,n):
        return self.lst[:n]
    def arr(self):
        return self.lst
    def load(self):
        return self.lst.tolist()

    # list funcs
    def len(self):
        return len(self.lst)
    def sumWay(self, axis):
        return ASIterable(np.sum(self.lst, axis))
    def sum(self):
        return ASIterable(np.sum(self.lst))
    def reshape(self,axis1,axis2):
        return ASIterable(self.lst.reshape((axis1, axis2)))
    def transpose(self):
        return ASIterable(self.lst.transpose())
    def t(self):
        return transpose(self)
    def dot(self, other):
        if isinstance(other, ASIterable):
            return ASIterable(np.dot(self.lst, other.lst))
        else: 
            return ASIterable(np.dot(self.lst, np.array(other)))

    # operator funcs
    def __add__(self, other):
        if isinstance(other, ASIterable):
            try:
                return ASIterable(self.lst + other.lst)
            except: return "undefined"
        elif isinstance(other, list):
            try:
                return ASIterable(self.lst + np.array(other))
            except: return "undefined"
        else: return "undefined"

    def __div__(self, k):
        if isinstance(k, (int, long, float, complex)) and k != 0:
            return ASIterable(self.lst / k)
        elif isinstance(k, ASIterable) and not np.any(k.lst==0):
            return ASIterable(np.divide(self.lst, k.lst))
        elif isinstance(k, list) and not 0 in k:
            return ASIterable(np.divide(self.lst, np.array(k)))
        else: return "undefined"

    def __sub__(self, other):
        if isinstance(other, ASIterable):
            return ASIterable(np.subtract(self.lst,other.lst))
        else:
            try:
                return ASIterable(np.subtract(self.lst, other))
            except: return "undefined"

    def __neg__(self):
        return ASIterable(-self.lst)

    def __mul__(self, other):
        if isinstance(other, ASIterable):
            try:
                return ASIterable(np.multiply(self.lst, other.lst))
            except: return "undefined"
        else: 
            try:
                return ASIterable(np.multiply(self.lst, np.array(other)))
            except: return "undefined"

    def __pow__(self, other):
        if isinstance(other, ASIterable):
            try:
                return ASIterable(np.power(self.lst,other.lst))
            except: return "undefined"
        else:
            try:
                return ASIterable(np.power(self.lst,other))
            except: return "undefined"

    def __rmul__(self, other):
        return self.__mul__(other)

    def __repr__(self):
        return repr(self.load())