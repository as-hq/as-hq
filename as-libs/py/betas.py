#takes in a list of lists
def beta(stockData):
    numStocks = shape(array(stockData))[1]
    betaMatrix = corrcoef(stockData) #numStocks x numStocks matrix
    stdevs = std(stockData,axis=0,ddof=1) #sample std for each stock
    for i in range(numStocks):
        for j in range(numStocks): 
            betaMatrix[i][j]*= stdevs[i]/stdevs[j]
    return betaMatrix.tolist() 
