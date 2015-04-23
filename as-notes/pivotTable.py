
# takes in data as a list of lists, with column headers as strings
# should eventually integrate with frontend so that you select rows, select cols etc and does it for you

def pivotTable(data, rows, cols, value, aggregator):
    # value is a column name
    rowList=True
    colList=True
    if not isinstance(rows,list):
        rows=[rows]
        rowList=False
    if not isinstance(cols,list):
        cols=[cols]
        colList=False
    rows = [data[0].index(r) for r in rows]
    cols = [data[0].index(c) for c in cols]
    value = data[0].index(value) #convert column header names to column indices
    data=data[1:] #eliminate column headers

    allRowList = list(set([tuple([data[i][r] for r in rows]) for i in range(len(data))]))
    allColList = list(set([tuple([data[i][c] for c in cols]) for i in range(len(data))]))
    
    summary = {}
    for i in range(len(data)):
        rowData = tuple([data[i][r] for r in rows])
        colData = tuple([data[i][c] for c in cols])
        if (rowData,colData) in summary:
            summary[(rowData,colData)].append(data[i][value])
        else:
            summary[(rowData,colData)]=[data[i][value]]
            
    answer=[["NoData"]*(len(allColList)+1) for _ in range(len(allRowList)+1)]
    for (r,c) in summary:
        rowIndex = allRowList.index(r)
        colIndex = allColList.index(c)
        answer[rowIndex+1][colIndex+1] = aggregator(summary[(r,c)])
        if rowList:
            answer[rowIndex+1][0]=str(list(r))
        else:
            answer[rowIndex+1][0]=r[0]
        if colList:
            answer[0][colIndex+1]=str(list(c))
        else:
            answer[0][colIndex+1]=c[0]
    answer[0][0]="PTable"
    return answer


'''
Example:
data=[["A","B","C","D"],[1,2,5,"hi"],[1,2,6,"bye"],[1,3,7,"bye"],[1,3,8,"bye"],[2,3,9,"hi"],[2,3,10,"hi"]]
pivotTable(data,["A","D"],"B","C",lambda x: sum(x))
[['PTable', '[2]', '[3]'], ["[1, 'bye']", 6, 15], ["[1, 'hi']", 5, 'NoData'], ["[2, 'hi']", 'NoData', 19]]

pivotTable(data,"A","B","C",lambda x:sum(x))
[['PTable', 2, 3], [2, 'NoData', 19], [1, 11, 15]]
'''
