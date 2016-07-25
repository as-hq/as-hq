import pandas as pd
import numpy as np
from pandas.compat import StringIO

# #incomplete doesn't yet support multi-column indexing
def dataframe(data, parse_dates=False, index_col=None, dayfirst=False):
    df = pd.DataFrame(data[1:], columns=data[0])

    if parse_dates == True:
        if index_col != None:
    	   parse_dates = [index_col]
        else:
            parse_dates = []
    elif parse_dates == False: 
    	parse_dates = [] 
    if type(parse_dates) is not list and parse_dates in df.keys(): # if it's a non-list, column name
        parse_dates = [parse_dates]
    for date_column in parse_dates:
        if np.issubdtype(df[date_column].dtype, np.number): 
            df[date_column] = [excel_to_python_datetime(x) for x in df[date_column]]
        else: 
            df[date_column] = pd.to_datetime(df[date_column], dayfirst=dayfirst)

    if index_col != None: 
        if index_col in df.keys():
            df = df.set_index(index_col)
        else:
            return df[index_col] # will return an error. probably not the best way of doing this


    return df

def excel_to_python_datetime(excelTime):
    return pd.to_datetime(np.datetime64('1900') + np.timedelta64(int(excelTime*24*60*60*1000), 'ms'))