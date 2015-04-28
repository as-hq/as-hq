class ASIterable(object):
    def __init__(self, lst):
        self.lst = lst
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
        self.lst.insert(0,elem)
    def append(self, elem):
        self.lst.append(elem)
    def insert(self, elem, idx):
        self.lst.insert(idx, elem)
    def take(self,n):
        return self.lst[:n]
    def load(self):
        return self.lst
    def len(self):
        return len(self.lst)
    def __repr__(self):
        return repr(self.lst)