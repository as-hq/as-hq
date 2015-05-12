# object oriented version of original ETF arbitrage code
# A simple trading strategy based on ETF arbitrage
# Highlights: naming variables, transparancy of formatting/logic, using numpy; making code modular/clear
import numpy as np
import math
import ETF
import ORD

'''
IN SPREADSHEET
A1=e2.readData("etfData.txt",4,3)["ETFs"] (this is a list of ETF objects)
B1=e2.readData("etfData.txt",4,3)["ORDS"] (list of ORD objects)
C1=e2.readData("etfData.txt",4,3)["Conversion Rate"]

The attributes of ETF are name, bid, ask, underlyings (list of ORDS),
        weights (for the ords), cr (creation fee) and rd (redemption fee)
The attributes of ORD are name, bid, ask
You could do:
D1 = [x.name for x in A]
E1 = [x.bid for x in A]
F1 = [x.ask for x in A]
H1,I1,J1 = same as above, but for B (to get ORDs)


L1=e2.etfArb(A1:A4,C1) (should be a 4x2 matrix)
'''

def computeETFArb(etfs, conversionRate):
    profitAndStrategy=[]
    print "hi"
    for etf in etfs: #buy at the ask price, sell at the bid price
        buyETF = -etf.ask + etf.ordImpliedETFPrices()[1]/conversionRate - etf.rd #redeem
        sellETF = etf.bid - etf.ordImpliedETFPrices()[0]/conversionRate - etf.cr #create
        profitAndStrategy.append(profStrat(buyETF,sellETF,etf.name))
    return profitAndStrategy

# takes in the profits from buying and selling the ETF, inteprets them correctly
# conditioning is transparant; errors are easy to detect
def profStrat(buy, sell,name):
    strat=""
    profit = 0 
    if buy>sell and buy>0:
        strat = "Buy "+name+", Sell ADRs"
        profit = buy
    elif sell>buy and sell>0:
        strat = "Sell "+name+", Buy ADRs"
        profit = sell
    else: #neither strategy is profitable
        return [0,"Nothing"]
    if profit<0.01:#margin is too small
        strat = "Maybe: "+strat
    if profit>0.10: #margin is suspiciously high
        profit = 0
        strat = "Possible Data Error: "+name
    return [profit, strat] 


# takes in the output/strats from computeETFArb and formats/processes it
def etfArb(etfs, conversionRate):
    strats = computeETFArb(etfs, conversionRate)
    #sort strategies in decreasing order of profit
    #this isn't transparent in excel
    strats=[[math.ceil(x*1000.0)/1000.0,y] for [x,y] in strats]
    return map(list,zip(*sorted(strats,key=lambda x:-x[0]))) #for display
    #TODO: color max profit green, color <0.01 orange, color>.10 red

#n=num ETFs
#k=num ORDs (all ETFs assumed to have same ord)
def readData(name,n,k):
    etfs=[]
    ords=[]
    cRate=0
    with open(name) as f:
        next(f)
        for x in xrange(n):
            line=next(f).strip().split(',')
            e=ETF.ETF(line[0])
            (bid,ask)=(float(line[1]),float(line[2]))
            e.setPrices((bid,ask))
            etfs.append(e)
        next(f)
        for x in xrange(k):
            line=next(f).strip().split(',')
            name=line[0]
            (bid,ask)=(float(line[1]),float(line[2]))
            ords.append(ORD.ORD(name,(bid,ask)))   
        next(f)
        for x in xrange(n):
            line=next(f).strip().split(',')
            name=line[0]
            w=[float(i) for i in line[1:]]
            for e in etfs:
                if e.name==name:
                    e.setORDs(ords,w)           
        next(f)
        for x in xrange(n):
            line=next(f).strip().split(',')
            name=line[0]
            for e in etfs:
                if e.name==name:
                    e.setFees((float(line[1]),float(line[2])))
        next(f)
        cRate=float(next(f).strip())
    return {"ETFs":etfs,"ORDs": ords,"Conversion Rate":cRate}
