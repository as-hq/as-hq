import pkgutil

def samples(name):
	return pkgutil.get_data('AS', 'data/' + name + '.txt')