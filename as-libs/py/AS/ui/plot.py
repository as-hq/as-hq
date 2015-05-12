import matplotlib.pyplot as plt
import os.path 

folder = os.path.dirname(__file__) 
directory = os.path.abspath(os.path.join(folder, '..','..','..','..','frontend','client','app','images')) + '/'

retPath = "images/"
def plot(x,y,name):
    plt.plot(x,y)
    fig=plt.gcf()
    path=directory+name+".png"
    fig.savefig(path)
    return {'imagePath':retPath + name + ".png"}

def bar(x,y,name):
    plt.bar(x,y,1/1.5, color='blue')
    fig=plt.gcf()
    path=directory+name+".png"
    fig.savefig(path)
    return {'imagePath':retPath + name + ".png"}

def testStockChart():
    return {'stockPrices':[["2014-01-01T23:28:56.782Z", 1.00, 1.50, 0.90, 1.20], ["2014-01-02T23:28:56.782Z", 1.30, 1.50, 1.00, 1.15]], 'stockName': 'TEST'}
