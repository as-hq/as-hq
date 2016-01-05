module AS.Dispatch.Expanding
  ( decomposeCompositeValue
  , recomposeCells
  , decomposeCells
  , getDimensions
  , recomposeCompositeValue
  ) where

import Prelude

import AS.Types.Cell
import AS.Types.Eval

import AS.DB.Internal
import AS.Util as U

import Data.List.Split (chunksOf)
import Control.Lens

import qualified Data.Map as M
import qualified Data.List as L

----------------------------------------------------------------------------------------------------------------------------------------------
-- value decomposition (expansion)

-- this function has a lot of repetition in its cases,
-- but I think it's clearer to separate each case 
-- instead of making a shitton of nested cases
decomposeCompositeValue :: ASCell -> CompositeValue -> Maybe FatCell
decomposeCompositeValue _ (CellValue _) = Nothing

decomposeCompositeValue c (Expanding (VList coll)) = Just $ FatCell cells desc
  where
    idx       = c^.cellLocation
    dims      = getDimensions coll
    rangeKey  = RangeKey idx dims
    desc      = RangeDescriptor rangeKey List $ M.fromList []
    cells     = decomposeCells desc c coll

decomposeCompositeValue c (Expanding (VRList pairs)) = Just $ FatCell cells desc
  where
    idx       = c^.cellLocation
    names     = map (ValueS . fst) pairs
    vals      = transpose' $ map snd pairs
    coll      = M $ names:vals
    dims      = getDimensions coll
    rangeKey  = RangeKey idx dims
    desc      = RangeDescriptor rangeKey RList $ M.fromList []
    cells     = decomposeCells desc c coll

decomposeCompositeValue c (Expanding (VRDataFrame labels indices vals)) = Just $ FatCell cells desc
  where
    idx       = c^.cellLocation
    vals'     = M $ prependColumn (NoValue:indices) (labels:vals)
    dims      = getDimensions vals'
    rangeKey  = RangeKey idx dims
    desc      = RangeDescriptor rangeKey RDataFrame $ M.fromList []
    cells     = decomposeCells desc c vals'

decomposeCompositeValue c (Expanding (VNPArray coll)) = Just $ FatCell cells desc
  where
    idx       = c^.cellLocation
    dims      = getDimensions coll
    rangeKey  = RangeKey idx dims
    desc      = RangeDescriptor rangeKey NPArray $ M.fromList []
    cells     = decomposeCells desc c coll

decomposeCompositeValue c (Expanding (VNPMatrix mat)) = Just $ FatCell cells desc
  where
    idx       = c^.cellLocation
    coll      = M mat
    dims      = getDimensions coll
    rangeKey  = RangeKey idx dims
    desc      = RangeDescriptor rangeKey NPMatrix $ M.fromList []
    cells     = decomposeCells desc c coll

decomposeCompositeValue c (Expanding (VPDataFrame labels indices vals)) = Just $ FatCell cells desc
  where
    idx       = c^.cellLocation
    vals'     = M $ prependColumn (NoValue:indices) (labels:vals)
    dims      = getDimensions vals'
    rangeKey  = RangeKey idx dims
    desc      = RangeDescriptor rangeKey PDataFrame $ M.fromList []
    cells     = decomposeCells desc c vals'

decomposeCompositeValue c (Expanding (VPSeries indices vals)) = Just $ FatCell cells desc
  where
    idx       = c^.cellLocation
    dims      = getDimensions (A vals)
    rangeKey  = RangeKey idx dims
    desc      = RangeDescriptor rangeKey PSeries $ M.fromList [("dfIndices", JSONLeaf . ListValue . A $ indices)]
    cells     = decomposeCells desc c (A vals)

decomposeCells :: RangeDescriptor -> ASCell -> Collection -> [ASCell]
decomposeCells (RangeDescriptor key etype _) (Cell (Index sheet coord) xp _ ps _) coll = case coll of -- #lens
  A arr -> unpack $ zip [r..] arr
      where
        r = coord^.row
        unpack = map (\(r', val) -> Cell (Index sheet (coord & row .~ r')) xp val ps (Just key))
  M mat -> concat . unpack $ zip [r..] mat
      where
        r = coord^.row
        c = coord^.col
        unpack = map (\(r', row) -> unpackRow r' $ zip [c..] row)
        unpackRow r' = map (\(c', val) -> Cell (Index sheet (coord & row .~ r' & col .~ c')) xp val ps (Just key))

getDimensions :: Collection -> Dimensions
getDimensions coll = case coll of 
  A arr -> Dimensions { width = 1, height = length arr }
  M mat -> Dimensions { width = maximum $ map length mat, height = length mat }

----------------------------------------------------------------------------------------------------------------------------------------------
-- value recomposition

recomposeCompositeValue :: FatCell -> CompositeValue
recomposeCompositeValue (FatCell cells (RangeDescriptor key List _)) = Expanding val
  where
    val  = VList coll
    coll = recomposeCells dims cells
    dims = keyDimensions key

recomposeCompositeValue (FatCell cells (RangeDescriptor key RList _)) = Expanding val
  where
    val     = VRList $ zip names' fields'
    names'  = map (\(ValueS s) -> s) names
    fields' = L.transpose fields
    (M (names:fields)) = recomposeCells dims cells
    dims    = keyDimensions key

recomposeCompositeValue (FatCell cells (RangeDescriptor key RDataFrame _)) = Expanding val
  where 
    val       = VRDataFrame labels indices (L.transpose vals)
    ((_:labels):indexedVals) = mat
    indices   = map (\(index:_) -> index) indexedVals 
    vals      = map (\(_:row) -> row) indexedVals
    (M mat)   = recomposeCells dims cells
    dims      = keyDimensions key

recomposeCompositeValue (FatCell cells (RangeDescriptor key NPArray _)) = Expanding val
  where
    val  = VNPArray coll
    coll = recomposeCells dims cells
    dims = keyDimensions key

recomposeCompositeValue (FatCell cells (RangeDescriptor key NPMatrix _)) = Expanding val
  where
    val     = VNPMatrix mat
    (M mat) = recomposeCells dims cells
    dims    = keyDimensions key

recomposeCompositeValue (FatCell cells (RangeDescriptor key PDataFrame _)) = Expanding val
  where
    val       = VPDataFrame labels indices vals
    ((_:labels):indexedVals) = mat
    indices   = map (\(index:_) -> index) indexedVals 
    vals      = map (\(_:row) -> row) indexedVals
    (M mat)   = recomposeCells dims cells
    dims      = keyDimensions key

recomposeCompositeValue (FatCell cells (RangeDescriptor key PSeries attrs)) = Expanding val
  where
    val       = VPSeries indices vals
    (JSONLeaf (ListValue (A indices))) = attrs M.! "seriesIndices"
    (A vals)  = recomposeCells dims cells
    dims      = Dimensions { width = 1, height = length cells }

recomposeCells :: Dimensions -> [ASCell] -> Collection
recomposeCells dims cells = case (width dims) of 
  1 -> A $ map (view cellValue) cells
  _ -> M . map (map (view cellValue)) $ reshapeList cells dims

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers

reshapeList :: [a] -> Dimensions -> [[a]]
reshapeList xs dims = chunksOf (width dims) xs

-- transposes non-rectangular matrices by filling in gaps with NoValue
transpose' :: [[ASValue]] -> [[ASValue]]
transpose' vals = L.transpose matrixified
  where
    width       = maximum $ map length vals
    matrixified = map (\row -> take width $ row ++ (repeat NoValue)) vals

prependColumn :: Array -> Matrix -> Matrix
prependColumn arr mat = map (\(x,xs) -> x:xs) $ zip arr mat
