import numpy as np #can't do this in VBA

# example of one of our provided libraries for finance
# more applicable to hedge fund rather than trading
# takes in stockReturns; an nxd matrix for stocks(rows) and historical price data(cols)
# returnRate is the customers' desired return rate
# returns: how to weight the stocks in the portfolio to min variance
# the list of weights returned sums to 1 

# assumes that all data is present, allows short-selling
def markovitz(stockReturns, returnRate):
    stockReturns = np.array(stockReturns)
    nStocks=np.shape(stockReturns)[0]
    returns = np.array([(np.log(stockReturns[stock][-1])-np.log(stockReturns[stock][0]))/len(stockReturns[stock]) for stock in range(0,len(stockReturns))])
    cov = np.cov(stockReturns)
    ones = np.ones(nStocks)
    covinv=np.linalg.inv(cov)
    a = np.dot(ones,np.dot(covinv,ones))
    b = np.dot(ones, np.dot(covinv,returns))
    c = np.dot(returns,np.dot(covinv,returns))
    lambda1 = (c-b*returnRate)/float(a*c-b**2)
    lambda2 = (a*returnRate-b)/float(a*c-b**2)
    return list(np.dot(covinv,lambda1*ones+lambda2*returns))

def dataInp(name,nDays):
    with open(name) as f:
        data = f.readlines()
        x=[[data[i][:-1]]+[float(x) for x in data[i:i+nDays][1:]] for i in range(0,len(data),nDays)]
    return x 

'''
IN SPREADSHEET
A1=dataInp("stockData.txt",14) (should fill A1:E14)
F1=0.02
G1=markovitz(A2:E14,F1) (5 elem list)
	assumes that A2:E14 will be a list of 5 elems, each elem is a 13-elem list
'''
'''
Example (not real data)
x=markovitz([[1,43,24,6],[3,2,1,6],[246,13,45,2]],1)
x
array([ 1.92820321, -0.71232064, -0.21588256])
sum(x)
0.99999999999997469
'''
