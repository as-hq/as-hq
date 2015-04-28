from ORD import ORD
import numpy as np
import math
from AS.iterable import ASIterable
from AS.stdlib import flat
from AS import samples

class ETF(object):
    allETFs = None
    convRate = None

    def __init__(self,etfName,(bid, ask)):
        self.name=etfName
        self.bid = bid
        self.ask = ask
        if ETF.allETFs:
            ETF.allETFs.append(self)
        else:
            ETF.allETFs = ASIterable([self])
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

    #getters
    def getPrice(self):
        return self.ask
    def getBid(self):
        return self.bid
    def components(self):
        comps = []
        for i in range(len(self.ords)):
            compSet = [self.ords[i].name, self.weights[i], self.cr, self.rd]
            comps.append(compSet)
        return comps


    @staticmethod
    def all():
        return ETF.allETFs

    # list of etfs, get matrix of all components
    @staticmethod
    def getComponents(etfs):
        components = [etf.components() for etf in etfs]
        return flat(components)

    #sample data
    #n=num ETFs
    #k=num ORDs (all ETFs assumed to have same order)
    @staticmethod
    def loadSamples(n,k=3):
        data = samples('etf_samples').replace('ETF Prices', '|',).replace('ADR Prices', '|').replace('weights', '|').replace('fees', '|').replace('CRate', '|')
        data = data.split('|')[1:]
        data = [section.split('\n')[1:-1] for section in data]
        maxRngEtfs = min(len(data[0]), n)

        #first load ETFs
        for idx in range(maxRngEtfs):
            priceSet=data[0][idx].split(',')
            (bid,ask)=(float(priceSet[1]),float(priceSet[2]))
            e = ETF(priceSet[0], (bid, ask))

        maxRngOrds = min(len(data[1]), k)
        ords = []
        #then load ORDs
        for idx in range(maxRngOrds):
            priceSet=data[1][idx].split(',')
            name = priceSet[0]
            (bid,ask)=(float(priceSet[1]),float(priceSet[2]))
            ords.append(ORD(name,(bid,ask)))   

        #now weights and fees
        for idx in range(maxRngEtfs):
            weightSet =  data[2][idx].split(',')
            feeSet = data[3][idx].split(',')
            name=weightSet[0]
            w=[float(i) for i in weightSet[1:]]
            for e in ETF.allETFs.load():
                if e.name==name:
                    e.setORDs(ords,w)  
                    e.setFees((float(feeSet[1]),float(feeSet[2])))

        ETF.convRate = float(data[4][0])
        return ETF.allETFs.load()

    #reps
    @classmethod
    def deserialize(cls, js):
        dOrds = [ORD.deserialize(x) for x in js["ords"]]
        return cls(js["name"], (js["bid"], js["ask"])).setORDs(dOrds, js["weights"]).setFees((js["cr"], js["rd"]))

    def serialize(self):
        sOrds = [ORD.serialize(x) for x in self.ords]
        return str({ "name": self.name, "bid": self.bid, "ask": self.ask, "ords": sOrds, "weights": self.weights, "cr": self.cr, "rd": self.rd})

    def displayValue(self):
        if hasattr(self, 'bid') and hasattr(self, 'ask'):
            return "ETF: "+self.name+" ("+str(self.bid)+", "+str(self.ask)+")"
        else: return "ETF: "+self.name

    def __str__(self):
        return str({ "displayValue": self.displayValue(), "actualValue": { "objectType": "ETF", "jsonRepresentation": self.serialize() } })

    def __repr__(self):
        return str(self)