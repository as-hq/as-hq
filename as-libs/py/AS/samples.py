import pkgutil

def samples(name):
	return pkgutil.get_data('AS', 'data/' + name + '.txt')

def stockSamples(name='stock_samples',nDays=14):
	data = samples('stock_samples')
	return [[data[i][:-1]]+[float(x) for x in data[i:i+nDays][1:]] for i in range(0,len(data),nDays)]
	#TODO link datainp from markobvitz