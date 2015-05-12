from  ORD import ORD
import numpy as np
import math
class ETF(object):
    def __init__(self,etfName):
        self.name=etfName
    def setPrices(self,(bid,ask)):
        self.bid=bid
        self.ask=ask
    def setORDs(self,underlyings,w): #must be in correct order
        self.ords=underlyings
        self.weights=w
    def setFees(self,(cr,rd)):
        self.cr=cr
        self.rd=rd
    def ordImpliedETFPrices(self):
        bidComps=np.array(self.weights)*np.array([x.ask for x in self.ords])
        offComps=np.array(self.weights)*np.array([x.bid for x in self.ords])
        return (np.sum(bidComps),np.sum(offComps))
    def __repr__(self):
        return "ETF: "+self.name+" ("+str(self.bid)+", "+str(self.ask)+")"
