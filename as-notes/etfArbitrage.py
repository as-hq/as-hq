# A simple trading strategy based on ETF arbitrage
# Highlights: naming variables, transparancy of formatting/logic, using numpy; making code modular/clear
import numpy as np
import math
'''
Input (assume that all n ETFs have the same k underlyings)
etfPrices is an nx2 matrix with best bid and offer prices of all n ETFs
adrPrices is a kx2 matrix of best bid/offers per ORD
weights is an nxk matrix; row i has the weights of ORDs for ETF i
conversionRate: 1 dollar = conversionRate GBP
fees: nx2 matrix of creation and redemption fees for each ETF
'''

'''
IN SPREADSHEET:
B2 = readData("etfData.txt",4,3)["ETFs"]
B6 = readData("etfData.txt",4,3)["Weights"]
B11 = readData("etfData.txt",4,3)["Fees"]

G2 = readData("etfData.txt",4,3)["ORDs"]
G6 = readData("etfData.txt",4,3)["Conversion Rate"]
G11 = etfArb(B2:E4,G2:I4,B6:E9,B11:E13,G6) should be a 4x2 matrix

Manually fill:
A2="ETF"
A3="Bid"
A4="Ask"
A6="RDSA LN"
A7="HSBC LN
A8="BP LN"
A11="Fees"
A12="Create"
A13="Redeem"
F2="ORDs"
F3="Bid"
F4="Ask"
'''



'''
old (ignore)
A2 = readData("etfData.txt",4,3)["ETFs"]
A7 = readData("etfData.txt",4,3)["ORDs"]
A11 = readData("etfData.txt",4,3)["Conversion Rate"]
E2 = readData("etfData.txt",4,3)["Weights"]
E7 = readData("etfData.txt",4,3)["Fees"]
J2 = etfArb(B2:C5,B7:C9,F2:H5,F7:G10,A11) should be a 4x2 matrix
this is assuming that B2:C5 will be a list with 4 elements, each of size 2 etc. ; a horizontal-based matrix

Manually fill: (old, ignore)
B1="Bid"
C1="Ask"
E1="Weights"
E6="Fees",E7="Create", E8="Redeem"
I1="Profit"
J1="Strategy"
'''

def computeETFArb(etfPrices, ordPrices, weights, fees, conversionRate):
    profitAndStrategy=[]
    weights=np.delete(np.array(weights),0,1).astype(np.float)
    ordPrices=np.delete(np.array(ordPrices),0,1).astype(np.float)
    print "weights"
    print weights,np.shape(weights)
    print "ords"
    print ordPrices
    for etf in range(len(etfPrices)): #buy at the ask price, sell at the bid price
        buyETF = -etfPrices[etf][2] + (ordImpliedETFPrice(ordPrices,weights)[etf][1])/conversionRate - fees[etf][2]
        sellETF = etfPrices[etf][1] - (ordImpliedETFPrice(ordPrices,weights)[etf][0])/conversionRate - fees[etf][1]
        profitAndStrategy.append(profStrat(buyETF,sellETF, etfPrices[etf][0]))
    return profitAndStrategy

# takes in the profits from buying and selling the ETF, inteprets them correctly
# conditioning is transparant; errors are easy to detect
def profStrat(buy, sell, name):
    strat=""
    profit = 0 
    if buy>sell and buy>0:
        strat = "Buy "+name+", Sell ORDs"
        profit = buy
    elif sell>buy and sell>0:
        strat = "Sell "+name+", Buy ORDs"
        profit = sell
    else: #neither strategy is profitable
        return [0,"Nothing"]
    if profit<0.01:#margin is too small
        strat = "Maybe: "+strat
    if profit>0.10: #margin is suspiciously high
        profit = 0
        strat = "Possible Data Error: " + name
    return [profit, strat] 

# takes in the best bids/offers and weights, returns the implied prices
def ordImpliedETFPrice(ordPrices, weights):
    impliedBids = np.dot(weights, ordPrices[:,1]) #weights*offers for adrs
    impliedOffers = np.dot(weights, ordPrices[:,0]) #weights*bid for adrs 
    return np.column_stack((impliedBids,impliedOffers))

# takes in the output/strats from computeETFArb and formats/processes it
def etfArb(etfPrices, ordPrices, weights, fees, conversionRate):
    strats = computeETFArb(etfPrices, ordPrices, weights, fees , conversionRate)
    strats=[[math.ceil(x*1000.0)/1000.0,y] for [x,y] in strats]
    return map(list,zip(*sorted(strats,key=lambda x:-x[0]))) #for display
    #TODO: color max profit green, color <0.01 orange, color>.10 red

def readData(name,n,k):
    etfPrices=[]
    ordPrices=[]
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
            ordPrices.append([line[0]]+[float(i) for i in line[1:]])     
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
    return {"ETFs":etfPrices,"ORDs": ordPrices,"Weights":weights,"Fees":fees,"Conversion Rate":cRate}
