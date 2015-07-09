import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('Qt4Agg')
matplotlib.rcParams['backend.qt4']='PySide'
import os.path 
from AS.iterable import ASIterable
from AS.instruments.Stock import Stock
from AS.errors import *
import numpy as np

folder = os.path.dirname(__file__) 
directory = os.path.abspath(os.path.join(folder, '..','..','..','..','frontend','client','app','images')) + '/'

retPath = "images/"

def getList(x):
    if isinstance(x, ASIterable):
        return x.load()
    else: return x

def plot(x,y=None,name=None):
    if y is None:
        return plotObj(x)
    xlist = getList(x)
    ylist = getList(y)
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
    xlist = getList(x)
    if isinstance(xlist[0], Stock):
        prices = []
        for stock in xlist:
            date = stock.data['Date'] + 'T23:28:56.782Z'
            opn = float(stock.data['Open'])
            high = float(stock.data['High'])
            low = float(stock.data['Low'])
            close = float(stock.data['Adj_Close'])
            prices.append([date,opn,high,low,close])
        rev = [e for e in reversed(prices)]
        return {'stockPrices':rev, 'stockName': xlist[0].symbol}

def plotGeneric(x, name="Selection Plot"):
    lst = ASIterable([e.load() for e in x])
    lst = lst.transpose()
    lst = lst.load()
    if (len(lst) > 2):
        raise MultiDimensionalDataException
    return plot(lst[0], lst[1], name)

def testStockChart():
    return {'stockPrices':[["2014-01-01T23:28:56.782Z", 1.00, 1.50, 0.90, 1.20], ["2014-01-02T23:28:56.782Z", 1.30, 1.50, 1.00, 1.15]], 'stockName': 'TEST'}

def testRickshaw():
    return {'rickshawData': [[0,23],[1,15]]}