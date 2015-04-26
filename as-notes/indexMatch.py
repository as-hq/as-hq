# finds the first match, takes in column headers
# assumes that data matrix is fully filled
def indexMatch(data, matchValues, matchCols, lookupCols,matchFuncs=None):
    # data entered in col form, we want row form
    data=map(list,zip(*data)) #transpose
    # deal with user entering one value vs. list
    if not isinstance(matchCols,list):
        matchCols=[matchCols]
        matchValues=[matchValues]
        if matchFuncs!=None:
            matchFuncs=[matchFuncs]
    if not isinstance(lookupCols,list):
        lookupCols=[lookupCols]
    # convert matchCols and lookupCols to indices
    matchCols=[data[0].index(m) for m in matchCols]
    lookupCols=[data[0].index(m) for m in lookupCols]
    foundMatch=False
    row=0     # skip row 0; column headers   
    while (not foundMatch) and row<len(data)-1:
        row+=1
        matchValuesForRow = [data[row][m] for m in matchCols]
        if matchFuncs!=None: 
            foundMatch = reduce(lambda x,y:(x and y),[matchFuncs[i](matchValues[i],matchValuesForRow[i]) for i in range(len(matchValues))])
        else:
            foundMatch = reduce(lambda x,y:(x and y),[matchValues[i]==matchValuesForRow[i] for i in range(len(matchValues))])
    if row==len(data):
        return [""]*len(lookupCols)
    else:
        return [data[row][i] for i in lookupCols]

'''
IN SPREADSHEET (input data from text file is not really needed here, can use the below example)
A1=[["A",1,1,1,1,2,2],["B",2,2,3,3,3,3],["C",5,6,7,8,9,10],["D","hi","bye","bye","bye","hi","hi"]]
F1=indexMatch(A1:D7,[1,3],["A","B"],"C") (what value in col C corresponds to first instance of (col A, col B)=(1,3))
(should be 7)
G1=indexMatch(A1:D7,2,"A","C")
(should be 9)
'''

'''
Example: row oriented
x=[["A","B","C","D"],[1,2,5,"hi"],[1,2,6,"bye"],[1,3,7,"bye"],[1,3,8,"bye"],[2,3,9,"hi"],[2,3,10,"hi"]]
'''
