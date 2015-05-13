import matplotlib.pyplot as plt
import os.path 
from AS.iterable import ASIterable

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

def plot(x,y,name):
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
    fig.set_size_inches(5,4)
    path=directory+name+".png"
    fig.savefig(path)
    return {'imagePath':retPath + name + ".png"}


def testStockChart():
    return {'stockPrices':[["2014-01-01T23:28:56.782Z", 1.00, 1.50, 0.90, 1.20], ["2014-01-02T23:28:56.782Z", 1.30, 1.50, 1.00, 1.15]], 'stockName': 'TEST'}
