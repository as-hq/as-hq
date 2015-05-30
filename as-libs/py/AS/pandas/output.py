import pandas as pd

def pprint(dataframe):
	rows = repr(dataframe).split('\n')
	mat = [row.split(' ') for row in rows]
	cleaned = [[e for e in row if e is not ''] for row in mat]
	cleaned[0].insert(0,'')
	return cleaned
