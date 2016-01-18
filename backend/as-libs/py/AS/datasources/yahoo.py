from yahoo_finance import Share

def stock_historical(security, start, end):
	sec = Share(security)
	return sec.get_historical(start, end)
