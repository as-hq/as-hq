import pkgutil

def samples(name):
	return pkgutil.get_data('AS', 'data/' + name + '.txt')

#def stockSamples(name=):
	#TODO link datainp from markobvitz