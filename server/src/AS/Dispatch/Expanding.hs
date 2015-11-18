module AS.Dispatch.Expanding where

import Prelude

import AS.Types.Core
import AS.DB.Util as DU
import AS.Util as U

import qualified Data.Map as M

----------------------------------------------------------------------------------------------------------------------------------------------
-- value decomposition (expansion)

decomposeCompositeValue :: ASCell -> CompositeValue -> Maybe FatCell
decomposeCompositeValue _ (CellValue _) = Nothing

decomposeCompositeValue c@(Cell idx _ _ _) (Expanding (VList coll)) = Just $ FatCell cells idx desc
  where
    dims      = getDimensions coll
    rangeKey  = DU.getRangeKey idx dims
    cells     = decomposeCells List rangeKey c coll
    desc      = ListDescriptor rangeKey

--decomposeCompositeValue c@(Cell idx _ _ _) (Expanding (VRList pairs)) = Just $ FatCell cells desc
--  where
--    cells     = decomposeCells Object c (M $ map snd pairs)
--    listkey   = DU.getListKey idx dims
--    listNames = JSONLeaf . ListValue . A $ map fst pairs
--    attrs     = JSON $ M.fromList [("listKeys", listNames)]
--    desc      = ObjectDescriptor listkey RList attrs

--decomposeCompositeValue c@(Cell idx _ _ _) (Expanding (VRDataFrame names values)) = Just $ FatCell cells desc
--  where

decomposeCompositeValue c@(Cell idx _ _ _) (Expanding (VNPArray coll)) = Just $ FatCell cells idx desc
  where
    dims      = getDimensions coll
    rangeKey  = DU.getRangeKey idx dims
    cells     = decomposeCells Object rangeKey c coll
    desc      = ObjectDescriptor rangeKey NPArray $ M.fromList []

decomposeCompositeValue c@(Cell idx _ _ _) (Expanding (VNPMatrix mat)) = Just $ FatCell cells idx desc
  where
    coll      = M mat
    dims      = getDimensions coll
    rangeKey  = DU.getRangeKey idx dims
    cells     = decomposeCells Object rangeKey c coll
    desc      = ObjectDescriptor rangeKey NPMatrix $ M.fromList []

decomposeCells :: ComplexType -> RangeKey -> ASCell -> Collection -> [ASCell]
decomposeCells cType rangeKey (Cell (Index sheet (c,r)) (Expression str lang) _ ts) coll = 
  let xp' = Coupled str lang cType rangeKey 
  in case coll of 
    A arr -> unpack $ zip [r..] arr
        where unpack = map (\(r', val) -> Cell (Index sheet (c,r')) xp' val ts)
    M mat -> concat . unpack $ zip [r..] mat
        where
          unpack = map (\(r', row) -> unpackRow r' $ zip [c..] row)
          unpackRow r' = map (\(c', val) -> Cell (Index sheet (c',r')) xp' val ts)

---- turns a composite value into possible a list, plus a range descriptor
--createListCells :: ASCell -> ASValue -> Maybe ASLists
--createListCells _ (ValueL []) = Nothing
--createListCells _ (RDataFrame []) = Nothing
--createListCells (Cell (Index sheet (a,b)) xp _ ts) cv = if (shouldCreateListCells cv)
--  then Just (listKey, cells)s
--  else Nothing
--  where
--    values    = getValuesFromCV cv
--    origLoc   = Index sheet (a,b)
--    rows      = map toList values
--    zipVals   = zip [0..] values
--    locs      = map (Index sheet) (concat $ map (\(row, val) -> shift val row (a,b)) zipVals)
--    height    = length values
--    width     = maximum $ map length rows
--    listKey   = DU.getListKey origLoc (height, width)
--    tags      = getExtraTagsFromCV cv
--    cells     = map (\(loc, val) -> Cell loc xp val ((ListMember listKey):tags)) $ zip locs (concat rows)

--    getValuesFromCV (ValueL l)        = l
--    getValuesFromCV (RDataFrame rows) = U.transposeList rows

--    getExtraTagsFromCV (ValueL _)     = []
--    getExtraTagsFromCV (RDataFrame _) = [DFMember]

--    shift (ValueL v) r (a,b)  = [(a+c,b+r) | c<-[0..length(v)-1] ]
--    shift other r (a,b)       = [(a,b+r)]

--    shouldCreateListCells (ValueL _) = True
--    shouldCreateListCells (RDataFrame _) = True
--    shouldCreateListCells _ = False

getDimensions :: Collection -> Dimensions
getDimensions coll = case coll of 
  A arr -> (length arr, 1)
  M mat -> (length mat, maximum $ map length mat) 

----------------------------------------------------------------------------------------------------------------------------------------------
-- value recomposition

recomposeCompositeValue :: FatCell -> CompositeValue
recomposeCompositeValue (FatCell cells _ (ListDescriptor key)) = Expanding val
  where
    val = VList coll
    coll = recomposeCells dims cells
    (_, dims) = rangeKeyToDimensions key

recomposeCompositeValue (FatCell cells _ (ObjectDescriptor key NPArray _)) = Expanding val
  where
    val = VNPArray coll
    coll = recomposeCells dims cells
    (_, dims) = rangeKeyToDimensions key

recomposeCompositeValue (FatCell cells _ (ObjectDescriptor key NPMatrix _)) = Expanding val
  where
    val = VNPMatrix mat
    (M mat) = recomposeCells dims cells
    (_, dims) = rangeKeyToDimensions key

recomposeCells :: Dimensions -> [ASCell] -> Collection
recomposeCells dims cells = case (snd dims) of 
  1 -> A $ map cellValue cells
  _ -> M $ map (\row -> map cellValue row) $ U.reshapeList cells dims


--------------------------------------------------------------------------------------------------------------
-- Lists

---- assumes all rows have same length, and every input ASValue is a row (i.e. column-major)
---- TODO deal with case of RDataFrame
--transposeList :: [ASValue] -> [ASValue]
--transposeList l = case (head l) of
--  (ValueL _) -> map ValueL $ L.transpose $ map toList l
--  _ -> [ValueL l]

--isHighDimensional :: Int -> ASValue -> Bool
--isHighDimensional depth (ValueL l) = if (depth + 1 > 2)
--  then True
--  else isHighDimensional (depth + 1) (head l)
--isHighDimensional depth (RDataFrame l) = if (depth + 1 > 2)
--  then True
--  else isHighDimensional (depth + 1) (head l)
--isHighDimensional depth _ = False

--sanitizeList :: ASValue -> ASValue
--sanitizeList v = if (isHighDimensional 0 v)
--  then ValueError "Cannot embed lists of dimension > 2." "StdErr" "" 0
--  else v