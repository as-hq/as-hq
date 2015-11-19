module AS.Dispatch.Expanding where

import Prelude

import AS.Types.Core
import AS.DB.Util as DU
import AS.Util as U

import qualified Data.Map as M
import qualified Data.List as L

----------------------------------------------------------------------------------------------------------------------------------------------
-- value decomposition (expansion)

-- TODO this function has a lot of repetition in its cases,
-- but I think it's clearer to separate each case 
-- instead of making a shitton of nested cases
decomposeCompositeValue :: ASCell -> CompositeValue -> Maybe FatCell
decomposeCompositeValue _ (CellValue _) = Nothing

decomposeCompositeValue c@(Cell idx _ _ _) (Expanding (VList coll)) = Just $ FatCell cells idx desc
  where
    dims      = getDimensions coll
    rangeKey  = DU.getRangeKey idx dims
    cells     = decomposeCells List rangeKey c coll
    desc      = ListDescriptor rangeKey

decomposeCompositeValue c@(Cell idx _ _ _) (Expanding (VRList pairs)) = Just $ FatCell cells idx desc
  where
    names     = map (ValueS . fst) pairs
    vals      = transpose' $ map snd pairs
    coll      = M $ names:vals
    dims      = getDimensions coll
    rangeKey  = DU.getRangeKey idx dims
    cells     = decomposeCells Object rangeKey c coll
    desc      = ObjectDescriptor rangeKey RList $ M.fromList []

decomposeCompositeValue c@(Cell idx _ _ _) (Expanding (VRDataFrame labels indices vals)) = Just $ FatCell cells idx desc
  where
    vals'     = M $ prependColumn (NoValue:indices) (labels:vals)
    dims      = getDimensions vals'
    rangeKey  = DU.getRangeKey idx dims
    cells     = decomposeCells Object rangeKey c vals'
    desc      = ObjectDescriptor rangeKey RDataFrame $ M.fromList []

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

decomposeCompositeValue c@(Cell idx _ _ _) (Expanding (VPDataFrame labels indices vals)) = Just $ FatCell cells idx desc
  where
    vals'     = M $ prependColumn (NoValue:indices) (labels:vals)
    dims      = getDimensions vals'
    rangeKey  = DU.getRangeKey idx dims
    cells     = decomposeCells Object rangeKey c vals'
    desc      = ObjectDescriptor rangeKey PDataFrame $ M.fromList []

decomposeCompositeValue c@(Cell idx _ _ _) (Expanding (VPSeries indices vals)) = Just $ FatCell cells idx desc
  where
    dims      = getDimensions (A vals)
    rangeKey  = DU.getRangeKey idx dims
    cells     = decomposeCells Object rangeKey c (A vals)
    desc      = ObjectDescriptor rangeKey PSeries $ M.fromList [("dfIndices", JSONLeaf . ListValue . A $ indices)]

decomposeCells :: DisplayType -> RangeKey -> ASCell -> Collection -> [ASCell]
decomposeCells dtype rangeKey (Cell (Index sheet (c,r)) xp _ ts) coll = 
  let str = xpString xp
      lang = xpLanguage xp
      xp' = Coupled str lang dtype rangeKey 
  in case coll of 
    A arr -> unpack $ zip [r..] arr
        where unpack = map (\(r', val) -> Cell (Index sheet (c,r')) xp' val ts)
    M mat -> concat . unpack $ zip [r..] mat
        where
          unpack = map (\(r', row) -> unpackRow r' $ zip [c..] row)
          unpackRow r' = map (\(c', val) -> Cell (Index sheet (c',r')) xp' val ts)

getDimensions :: Collection -> Dimensions
getDimensions coll = case coll of 
  A arr -> (length arr, 1)
  M mat -> (length mat, maximum $ map length mat) 

prependColumn :: Array -> Matrix -> Matrix
prependColumn arr mat = map (\(x,xs) -> x:xs) $ zip arr mat

----------------------------------------------------------------------------------------------------------------------------------------------
-- value recomposition

recomposeCompositeValue :: FatCell -> CompositeValue
recomposeCompositeValue (FatCell cells _ (ListDescriptor key)) = Expanding val
  where
    val = VList coll
    coll = recomposeCells dims cells
    (_, dims) = rangeKeyToDimensions key

recomposeCompositeValue (FatCell cells _ (ObjectDescriptor key RList _)) = Expanding val
  where
    val = VRList $ zip names' fields'
    names' = map (\(ValueS s) -> s) names
    fields' = L.transpose fields
    (M (names:fields)) = recomposeCells dims cells
    (_, dims) = rangeKeyToDimensions key

recomposeCompositeValue (FatCell cells _ (ObjectDescriptor key RDataFrame _)) = Expanding val
  where 
    val       = VRDataFrame labels indices (L.transpose vals)
    ((_:labels):indexedVals) = mat
    indices   = map (\(index:_) -> index) indexedVals 
    vals      = map (\(_:row) -> row) indexedVals
    (M mat)   = recomposeCells dims cells
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

recomposeCompositeValue (FatCell cells _ (ObjectDescriptor key PDataFrame _)) = Expanding val
  where
    val       = VPDataFrame labels indices vals
    ((_:labels):indexedVals) = mat
    indices   = map (\(index:_) -> index) indexedVals 
    vals      = map (\(_:row) -> row) indexedVals
    (M mat)   = recomposeCells dims cells
    (_, dims) = rangeKeyToDimensions key

recomposeCompositeValue (FatCell cells _ (ObjectDescriptor key PSeries attrs)) = Expanding val
  where
    val       = VPSeries indices vals
    (JSONLeaf (ListValue (A indices))) = attrs M.! "seriesIndices"
    (A vals)   = recomposeCells dims cells
    dims = (length cells, 1)

recomposeCells :: Dimensions -> [ASCell] -> Collection
recomposeCells dims cells = case (snd dims) of 
  1 -> A $ map cellValue cells
  _ -> M . L.transpose $ map (\row -> map cellValue row) $ U.reshapeList cells (snd dims, fst dims)

transpose' :: [[ASValue]] -> [[ASValue]]
transpose' vals = L.transpose squarified
  where
    width      = maximum $ map length vals
    squarified = map (\row -> take width $ row ++ (repeat NoValue)) vals

--------------------------------------------------------------------------------------------------------------
-- Lists

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