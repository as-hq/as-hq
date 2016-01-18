import datetime
import numpy as np
from matplotlib import finance as f
from sklearn import cluster, covariance, manifold
from matplotlib.collections import LineCollection
import matplotlib.pyplot as plt
from AS.ui.plot import *


class ClusterStocks:
    def __init__(self):
        self.dict = {
            'TOT': 'Total',
            'XOM': 'Exxon',
            'CVX': 'Chevron',
            'MDLZ': 'Kraft Foods',
            'K': 'Kellogg',
            'PFE': 'Pfizer',
            'SNY': 'Sanofi-Aventis',
            'NVS': 'Novartis',
            'KMB': 'Kimberly-Clark',
            'R': 'Ryder',
            'GD': 'General Dynamics',
            'RTN': 'Raytheon',
            'CVS': 'CVS',
            'CAT': 'Caterpillar',
            'DD': 'DuPont de Nemours',
            'MSFT': 'Microsoft',
            'IBM': 'IBM',
            'TWX': 'Time Warner',
            'CMCSA': 'Comcast',
            'CVC': 'Cablevision',
            'YHOO': 'Yahoo',
            'DELL': 'Dell',
            'HPQ': 'HP',
            'AMZN': 'Amazon'}
        self.symbols, self.names = np.array(list(self.dict.items())).T
        self.d1=datetime.datetime(2003,1,1)
        self.d2=datetime.datetime(2003,11,1)

    def getDefaultStocks(self):
        stocks = []
        for tick in self.dict.keys():
            stocks.append([tick,self.dict[tick]])
        return stocks

    def setDates(self,(y1,m1,d1),(y2,m2,d2)):
        self.d1 = datetime.datetime(y1,m1,d1)
        self.d2 = datetime.datetime(y2,m2,d2)

    def getVariations(self,symbols=None,names=None):
        if symbols==None:
            symbols=self.symbols
        if names==None:
            names = self.names
        quotes = [f.quotes_historical_yahoo_ochl(symbol, self.d1, self.d2, asobject=True, adjusted=True)
          for symbol in symbols]
        opens = np.array([q.open for q in quotes]).astype(np.float)
        closes = np.array([q.close for q in quotes]).astype(np.float)
        variation = closes - opens
        return variation

    def cluster(self,variation):
        variation = np.asarray(variation)
        edge_model = covariance.GraphLassoCV()
        X = variation.copy().T
        X /= X.std(axis=0)
        edge_model.fit(X)
        _, labels = cluster.affinity_propagation(edge_model.covariance_)
        n_labels = labels.max()
        self.edgeModel = edge_model
        self.X = X
        self.labels = labels
        self.n_labels = n_labels
        clusters = []
        for i in range(n_labels+1):
            clusters.append(self.names[labels == i].tolist())
        return clusters

    def visualize(self):
        node_position_model = manifold.LocallyLinearEmbedding(
         n_components=2, eigen_solver='dense', n_neighbors=6)
        embedding = node_position_model.fit_transform(self.X.T).T

        plt.figure(1, facecolor='w', figsize=(10, 8))
        plt.clf()
        ax = plt.axes([0., 0., 1., 1.])
        plt.axis('off')

        # Display a graph of the partial correlations
        partial_correlations = self.edgeModel.precision_.copy()
        d = 1 / np.sqrt(np.diag(partial_correlations))
        partial_correlations *= d
        partial_correlations *= d[:, np.newaxis]
        non_zero = (np.abs(np.triu(partial_correlations, k=1)) > 0.02)

        # Plot the nodes using the coordinates of our embedding
        plt.scatter(embedding[0], embedding[1], s=100 * d ** 2, c=self.labels,
                    cmap=plt.cm.spectral)

        # Plot the edges
        start_idx, end_idx = np.where(non_zero)
        #a sequence of (*line0*, *line1*, *line2*), where::
        #            linen = (x0, y0), (x1, y1), ... (xm, ym)
        segments = [[embedding[:, start], embedding[:, stop]]
                    for start, stop in zip(start_idx, end_idx)]
        values = np.abs(partial_correlations[non_zero])
        lc = LineCollection(segments,
                            zorder=0, cmap=plt.cm.hot_r,
                            norm=plt.Normalize(0, .7 * values.max()))
        lc.set_array(values)
        lc.set_linewidths(15 * values)
        ax.add_collection(lc)

        # Add a label to each node. The challenge here is that we want to
        # position the labels to avoid overlap with other labels
        for index, (name, label, (x, y)) in enumerate(
                zip(self.names, self.labels, embedding.T)):

            dx = x - embedding[0]
            dx[index] = 1
            dy = y - embedding[1]
            dy[index] = 1
            this_dx = dx[np.argmin(np.abs(dy))]
            this_dy = dy[np.argmin(np.abs(dx))]
            if this_dx > 0:
                horizontalalignment = 'left'
                x = x + .002
            else:
                horizontalalignment = 'right'
                x = x - .002
            if this_dy > 0:
                verticalalignment = 'bottom'
                y = y + .002
            else:
                verticalalignment = 'top'
                y = y - .002
            plt.text(x, y, name, size=10,
                     horizontalalignment=horizontalalignment,
                     verticalalignment=verticalalignment,
                     bbox=dict(facecolor='w',
                               edgecolor=plt.cm.spectral(label / float(self.n_labels)),
                               alpha=.6))

        plt.xlim(embedding[0].min() - .15 * embedding[0].ptp(),
                 embedding[0].max() + .10 * embedding[0].ptp(),)
        plt.ylim(embedding[1].min() - .03 * embedding[1].ptp(),
                 embedding[1].max() + .03 * embedding[1].ptp())

        return savePlot(plt,"embedding")

