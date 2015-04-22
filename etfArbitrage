# A simple trading strategy based on ETF arbitrage
# Highlights: naming variables, transparancy of formatting/logic, using numpy; making code modular/clear
import numpy as np
from operator import itemgetter
'''
Input (assume that all n ETFs have the same k underlyings)
etfPrices is an nx2 matrix with best bid and offer prices of all n ETFs
adrPrices is a kx2 matrix of best bid/offers per ADR
weights is an nxk matrix; row i has the weights of ADRs for ETF i
conversionRate: 1 dollar = conversionRate GBP
fees: nx2 matrix of creation and redemption fees for each ETF
'''

def computeETFArb(etfPrices, adrPrices, weights, conversionRate, fees):
    profitAndStrategy=[]
    for etf in range(len(etfPrices)): #buy at the ask price, sell at the bid price
        buyETF = -etfPrices[etf][1] + (adrImpliedETFPrice[i][1])/conversionRate - fees[i][1]
        sellETF = etfPrices[etf][0] - (adrImpliedETFPrice[i][0])/conversionRate - fees[i][0]
        profitAndStrategy.append(profStrat(buyETF,sellETF))
    return profitAndStrategy

# takes in the profits from buying and selling the ETF, inteprets them correctly
# conditioning is transparant; errors are easy to detect
def profStrat(buy, sell):
    strat=""
    profit = 0
    if buy>sell and buy>0:
        strat = "Buy ETF, Sell ADRs"
        profit = buy
    elif sell>buy and sell>0:
        strat = "Sell ETF, Buy ADRs"
        profit = sell
    else: #neither strategy is profitable
        return [0,"Nothing"]
    if profit<0.01:#margin is too small
        strat = "Maybe: "+strat
    if profit>0.10: #margin is suspiciously high
        profit = 0
        strat = "Possible Data Error"
    return [profit, strat] 

# takes in the best bids/offers and weights, returns the implied prices
def adrImpliedETFPrice(adrPrices, weights):
    impliedBids = np.dot(np.array(weights), np.array(adrPrices)[:1]) #weights*offers for adrs
    impliedOffers = np.dot(np.array(weights), np.array(adrPrices)[:0]) #weights*bid for adrs
    return np.column_stack((impliedBids,impliedOffers))

# takes in the output/strats from computeETFArb and formats/processes it
def etfArb(etfPrices, adrPrices, weights, conversionRate, fees):
    strats = computeETFArb(etfPrices, adrPrices, weights, conversionRate, fees)
    #sort strategies in decreasing order of profit
    #this isn't transparent in excel
    sortedStrats = sorted(profStrats, key=-itemgetter(0))
    return sortedStrats
    #TODO: color max profit green, color <0.01 orange, color>.10 red
