from AS.datasources.yahoo import stock_historical
from AS.ui.styling import color

from AS.errors import InsufficientArgumentsException

class Stock(object):
	def __init__(self, symbol, source):
		self.symbol = symbol
		self.source = source

	# def setDataByArg(self, high=None, low=None, opn=None, close=None, volume=None, adj_close=None):
	# 	self.data = {}
	# 	args = [high,low,opn,close,volume,adj_close]
	# 	for arg in args:
	# 		if arg is not None:

	def setData(self, data):
		self.data = data

	@staticmethod
	def load(symbol, source='yahoo', start=None, interval=None):
		if start is None and interval is not None:
			return str({"style": "blue","value":{"display":"SPECIFY","curried":1,"manipulate":["start"]}})
		elif start is None and interval is None:
			raise InsufficientArgumentsException("must specify interval")
		else:
			if source == 'yahoo':
				endDay = int(start.split('-')[-1]) + interval
				if endDay < 10:
					endDay = repr(0) + repr(endDay)
				else:
					endDay = repr(endDay)
				endMonth = '-'.join(start.split('-')[:-1])
				end = '-'.join([endMonth, endDay])
				results = stock_historical(symbol, start, end)
				data = []
				for result in results:
					stock = Stock(symbol, source)
					stock.setData(result)
					data.append(stock)
				return data

	@classmethod
	def deserialize(cls, js):
		stock = cls(js["symbol"], js["source"])
		if "data" in js:
			stock.setData(js["data"])
		return stock

	def serialize(self):
		obj = {"symbol": self.symbol, "source": self.source}
		if hasattr(self, 'data'):
			obj["data"] = self.data
		return str(obj)

	def displayValue(self):
		if hasattr(self, 'data'):
			return self.symbol + '_' + self.data['Date']
		else:
			return self.symbol + '_UNSPECIFIED'

	def __str__(self):
		return str({ "displayValue": self.displayValue(), "actualValue": { "objectType": "Stock", "jsonRepresentation": self.serialize() } })

	def __repr__(self):
		return str(self)