# an annoying problem in finance: how to deal with seemingly bad ticks
# one possible solution: just delete any stock with outliers

# rows of dataMatrix are stocks, columns are times in the day
# given a matrix, delete any row (stock) that has a "low" or "high" outlier
def filterData (dataMatrix, low, high):
    return filter(lambda x: min(x)>low and max(x)<high, dataMatrix)

# see http://www.extendoffice.com/documents/excel/1747-excel-delete-row-if-zero.html#a1


# another possible solution: replace an outlier with the last "good" price
# rows of dataMatrix are stocks, columns are times in the day
def filterTickData (tickData, low, high):
    good = lambda x: x>low and x<high
    return [modifiedTickData(good,tickRow) for tickRow in tickData]

# helper function for filterTickData
# good is a function that is true if a tick price is reasonable
# tickData is a bunch of ticks over time for a particular stock
def modifiedTickData (good, tickData):
    reasonablePrices = [tickData[0]] #first price assumed OK
    for i in range(1,len(tickData)):
        if good(tickData[i]): # the last good price is the current price
            reasonablePrices.append(tickData[i])
        else: #the last good price is the last good price in time 1...i-1
            reasonablePrices.append(reasonablePrices[i-1])
    return reasonablePrices

'''
Example: 
>>> good = lambda x: x>2 and x<10
>>> m=modifiedTickData(good,[3,1,1,4,5,11,12,13,6,7,8,-3])
>>> m
[3, 3, 3, 4, 5, 5, 5, 5, 6, 7, 8, 8]
'''
    
