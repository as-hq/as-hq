module AS.Dispatch.Expanding where

decomposeCompositeValue :: ASCell -> CompositeValue -> Maybe FatCell
decomposeCompositeValue _ (CellValue _) = Nothing

decomposeCompositeValue (Cell idx xp _ ts) (Expanding (VList coll)) = Just $ FatCell cells desc
  where
    dims      = getDimensions coll
    listkey   = DU.getListKey idx dims
    locs      = decomposeLocations idx dims

-- turns a composite value into possible a list, plus a range descriptor
createListCells :: ASCell -> ASValue -> Maybe ASLists
createListCells _ (ValueL []) = Nothing
createListCells _ (RDataFrame []) = Nothing
createListCells (Cell (Index sheet (a,b)) xp _ ts) cv = if (shouldCreateListCells cv)
  then Just (listKey, cells)s
  else Nothing
  where
    values    = getValuesFromCV cv
    origLoc   = Index sheet (a,b)
    rows      = map toList values
    zipVals   = zip [0..] values
    locs      = map (Index sheet) (concat $ map (\(row, val) -> shift val row (a,b)) zipVals)
    height    = length values
    width     = maximum $ map length rows
    listKey   = DU.getListKey origLoc (height, width)
    tags      = getExtraTagsFromCV cv
    cells     = map (\(loc, val) -> Cell loc xp val ((ListMember listKey):tags)) $ zip locs (concat rows)

    getValuesFromCV (ValueL l)        = l
    getValuesFromCV (RDataFrame rows) = U.transposeList rows

    getExtraTagsFromCV (ValueL _)     = []
    getExtraTagsFromCV (RDataFrame _) = [DFMember]

    shift (ValueL v) r (a,b)  = [(a+c,b+r) | c<-[0..length(v)-1] ]
    shift other r (a,b)       = [(a,b+r)]

    shouldCreateListCells (ValueL _) = True
    shouldCreateListCells (RDataFrame _) = True
    shouldCreateListCells _ = False

getDimensions :: Collection -> Dimensions
getDimensions coll = case coll of 
  A arr -> (length arr, 0)
  M mat -> (length mat, max $ map length mat) 