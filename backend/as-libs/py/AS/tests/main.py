'''
can be ignored, just some testing on my end
'''
import etfArbitrage as e3
import pivotTable as p
import betas as b
import markovitz as m
import etfArbitrage2 as e2
from numpy import corrcoef, std, array, shape


x=e2.readData("etfData.txt",4,3)
print x
y=e2.etfArb(x["ETFs"],x["Conversion Rate"])
print y




def readData(name):
    with open(name) as f:
        data=f.readlines()
    data= [x[:-1] for x in data]
    return [float(x) for x in data]
print readData("crazyData.txt")
print "here"
def dataInp(name,nDays):
    with open(name) as f:
        data = f.readlines()
        x=[[data[i][:-1]]+[float(x) for x in data[i:i+nDays][1:]] for i in range(0,len(data),nDays)]
    return x
print dataInp("stockData.txt",14)
print "hi here"
y=[x[1:] for x in dataInp("stockData.txt",14)]
print m.markovitz(y,0.02)







data = [x[1:] for x in dataInp("stockData.txt",14)]
print "OMG HI"
print dataInp("stockData.txt",14)
print m.markovitz(data,.02)
print sum(m.markovitz(data,.02))

etf=[[62.57,62.59],[46.66,46.70],[43.34,43.37]]
adr=[[2066.0,2068.50],[610.80,612.20],[478.80,483.15]]
w=[[2,0,0],[0,5,0],[0,0,6]]
fees=[[.1,.1],[.1,.1],[.1,.1]]
cr=65.84145
print e3.etfArb(etf,adr,w,fees,cr)


print [x[1:] for x in dataInp("stockData.txt",14)]
def read(name,n,k):
    etfPrices=[]
    adrPrices=[]
    weights=[]
    fees=[]
    cRate=0
    with open(name) as f:
        next(f)
        for x in xrange(n):
            line=next(f).strip().split(',')
            etfPrices.append([line[0]]+[float(i) for i in line[1:]])
        next(f)
        for x in xrange(k):
            line=next(f).strip().split(',')
            adrPrices.append([line[0]]+[float(i) for i in line[1:]])     
        next(f)
        for x in xrange(n):
            line=next(f).strip().split(',')
            weights.append([line[0]]+[float(i) for i in line[1:]])
        next(f)
        for x in xrange(n):
            line=next(f).strip().split(',')
            fees.append([line[0]]+[float(i) for i in line[1:]])
        next(f)
        cRate=float(next(f).strip())
    return {"ETFs":etfPrices,"ADRs": adrPrices,"Weights":weights,"Fees":fees,"Conversion Rate":cRate}

print read("etfData.txt",4,3)

print "pivot table"

x=[["A",1,1,1,1,2,2],["B",2,2,3,3,3,3],["C",5,6,7,8,9,10],["D","hi","bye","bye","bye","hi","hi"]]
print p.pivotTable(x,["A","D"],"B","C",lambda x: sum(x))


