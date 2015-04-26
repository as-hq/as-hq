from numpy import corrcoef, std, array, shape

# given historical price data for stocks (one stock per column)
# compute the betas for each pair of stocks
# useful for hedging in the market-making business; reduce variance/risk

#takes in a list of lists
def betas(stockData):
    numStocks = shape(array(stockData))[1]
    betaMatrix = corrcoef(stockData) #numStocks x numStocks matrix
    stdevs = std(stockData,axis=0,ddof=1) #sample std for each stock
    for i in range(numStocks):
        for j in range(numStocks): 
            betaMatrix[i][j]*= stdevs[i]/stdevs[j]
    return betaMatrix.tolist() 

# 14 days of data for this example
def dataInp(name,nDays):
    with open(name) as f:
        data = f.readlines()
        x=[[data[i][:-1]]+[float(x) for x in data[i:i+nDays][1:]] for i in range(0,len(data),nDays)]
    return x

# IN SPREADSHEET:
    # A1=dataInp("stockData.txt",14) (should fill in A1 to E14)
    # G1=betas([x[1:] for x in dataInp("stockData.txt",14)]) (should output 5x5 matrix)
    # I would like to do betas(A2:E14), but I need a list of 5 elems, each with 13 prices. I think A2:E14 will give 13 elem list, each with 5 elements. 
    
# in excel, need to do:
# =CORREL(OFFSET($B$3:$B$50,,ROWS($1:2)-1),OFFSET($B$3:$B$50,,COLUMNS($A:C)-1))
# and copy/drag this formula over the correct range of cells
# unreadable, annoying, and unnecessary
