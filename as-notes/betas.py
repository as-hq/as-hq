from numpy import corrcoef, std, array, shape
# given historical price data for stocks (one stock per column)
# compute the betas for each pair of stocks
# useful for hedging in the market-making business; reduce variance/risk

#takes in a list of lists
def beta(stockData):
    numStocks = shape(array(stockData))[1]
    betaMatrix = corrcoef(stockData) #numStocks x numStocks matrix
    stdevs = std(stockData,axis=0,ddof=1) #sample std for each stock
    for i in range(numStocks):
        for j in range(numStocks): 
            betaMatrix[i][j]*= stdevs[i]/stdevs[j]
    return betaMatrix.tolist() 

# in excel, need to do:
# =CORREL(OFFSET($B$3:$B$50,,ROWS($1:2)-1),OFFSET($B$3:$B$50,,COLUMNS($A:C)-1))
# and copy/drag this formula over the correct range of cells
# unreadable, annoying, and unnecessary