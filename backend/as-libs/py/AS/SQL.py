from __future__ import print_function
import json
import traceback
from pysqldf import SQLDF
import pandas as pd
import numpy as np
import sys

#-------------------------------------------------------------------------------

# Evaluate a SQL command given context. Produce a namespace with variables 
# corresponding to the tables, and call sqldf. We also move the first column 
# to the row indices.
def evalSql(dbCmd, context):
  # print(dbCmd, file=sys.__stdout__)
  namespace = produceNamespace(context)
  sqldf = SQLDF(namespace)
  df_result = sqldf.execute(dbCmd)
  df_result = moveFirstColumnToRows(df_result)
  print(df_result, file=sys.__stdout__)
  return df_result

# If we get a list, convert to dataframe, keeping in mind to 
# treat the first col as row names if the top left corner is empty.
# If we get a dataframe, we're good to go. If that dataframe has
# row names, though, move it to the first column (you probably want
# to retain that data across queries)
def produceNamespace(context):
  namespace = {}
  for i in range(len(context)):
    table = eval(context[i])
    if isinstance(table, list):
      #print("LIST! " + str(table[0][0]), file=sys.__stdout__)
      topLeftEmpty = table[0][0] == None or table[0][0] == ''
      df = moveRowsToFirstColumn(listToDataframe(table))
      if topLeftEmpty:
        df.set_index('Column1', inplace=True)
        df.drop('RowNames', axis=1, inplace=True)
      namespace[varName(i)] = df
      # print(topLeftEmpty, file=sys.__stdout__)
      # print(df, file=sys.__stdout__)
    elif isinstance(table, pd.DataFrame):
      # print(table, file=sys.__stdout__)
      namespace[varName(i)] = moveRowsToFirstColumn(table)
  return namespace

#-------------------------------------------------------------------------------
# Helpers

def varName(i):
  return "dataset" + str(i)

def moveRowsToFirstColumn(df):
  df.index.name = 'RowNames'
  return df.reset_index()

def moveFirstColumnToRows(df):
  try:
    df = df.set_index('RowNames')
    df.index = [possiblyStringify(ind) for ind in df.index]
    return df
  except Exception as e:
    # print(e, file=sys.__stdout__)
    return df

# If row names are lists due to joins, for example, stringify them.
def possiblyStringify(ind):
  #print(ind, file=sys.__stdout__)
  if isinstance(ind, list) or isinstance(ind, tuple):
    return str(ind)
  else:
    return ind

# Given a row-major list, convert it to a pandas dataframe
def listToDataframe(lst): 
  # Transform a 1D list to a 2D list
  if len(np.array(lst).shape) == 1:
    return listToDataframe([[a] for a in lst])
  else:
    # The first row should be all strings, default to Column_i. Do not throw
    # an error just because a column header wasn't given.
    areStrings = map(lambda x: isinstance(x, basestring), lst[0])
    if not any(areStrings):
      colHeaders = map(lambda x: "Column" + str(x + 1), range(len(lst[0])))
      lst.insert(0, colHeaders)
    for idx, colHeader in enumerate(lst[0]):
      if not isinstance(colHeader, basestring):
        lst[0][idx] = "Column" + str(idx + 1)
    maxVals = len(lst) - 1
    data = [{} for _ in range(maxVals)]
    for rowIdx in range(maxVals):
      row = lst[rowIdx + 1]
      for colIdx in range(len(lst[0])):
        header = lst[0][colIdx]
        data[rowIdx][header] = row[colIdx]
    return pd.DataFrame(data)

#-------------------------------------------------------------------------------