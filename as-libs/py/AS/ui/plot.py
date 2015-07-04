import matplotlib.pyplot as plt
import os.path 
from AS.iterable import ASIterable
from AS.instruments.Stock import Stock
from AS.errors import *

folder = os.path.dirname(__file__) 
directory = os.path.abspath(os.path.join(folder, '..','..','..','..','frontend','client','app','images')) + '/'

retPath = "images/"

def getList(x,y):
    xlist = []; ylist = []
    if isinstance(x, ASIterable):
        xlist = x.load()
    else: xlist = x
    if isinstance(y, ASIterable):
        ylist = y.load()
    else: ylist = y
    return xlist, ylist

def plot(x,y=None,name=None):
    if y is None:
        return plotObj(x)
    xlist, ylist = getList(x,y)
    plt.plot(xlist,ylist)
    plt.grid(True)
    plt.title(name)
    return savePlot(plt, name)

def bar(x,y,name):
    xlist, ylist = getList(x,y)
    plt.bar(xlist,ylist,1/1.5, color='blue')
    plt.title(name)
    return savePlot(plt, name)

def savePlot(plt, name):
    fig=plt.gcf()
    fig.set_size_inches(5,5)
    path=directory+name+".png"
    fig.savefig(path)
    return {'imagePath':retPath + name + ".png"}

def plotObj(x, name=None):
    if isinstance(x[0], Stock):
        prices = []
        for stock in x:
            date = stock.data['Date'] + 'T23:28:56.782Z'
            opn = float(stock.data['Open'])
            high = float(stock.data['High'])
            low = float(stock.data['Low'])
            close = float(stock.data['Adj_Close'])
            prices.append([date,opn,high,low,close])
        rev = [e for e in reversed(prices)]
        return {'stockPrices':rev, 'stockName': x[0].symbol}

def plotGeneric(x, name="Generic"):
    lst = x.transpose().load()
    if (len(lst) > 2):
        raise MultiDimensionalDataException
    return plot(x[0], x[1], name)

def testStockChart():
    return {'stockPrices':[["2014-01-01T23:28:56.782Z", 1.00, 1.50, 0.90, 1.20], ["2014-01-02T23:28:56.782Z", 1.30, 1.50, 1.00, 1.15]], 'stockName': 'TEST'}

def testRickshaw():
    return {'rickshawData': [[0,23],[1,15]]}