module AS.Types.Eval
  ( module AS.Types.Eval
  , module AS.Types.Locations
  , module AS.Types.Errors
  , module AS.Types.RangeDescriptor
  , module AS.Types.Values
  ) where

import AS.Types.CellProps
import AS.Types.Values
import AS.Types.Locations
import AS.Types.RangeDescriptor
import AS.Types.Errors
import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Updates

import Safe (headMay)

import Control.Lens hiding (index)
import Control.Applicative ((<$>), (<*>))

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (isJust)


-- For internal use only. Represents a "cell" that takes up numerous cells (e.g., range(10)).
data FatCell = FatCell { expandedCells :: [ASCell], descriptor :: RangeDescriptor } deriving (Show, Read)
data CompositeCell = Single ASCell | Fat FatCell

type CellMap = M.Map ASIndex ASCell

-- This should be thought of as a mini spreadsheet used by eval as a cache (which can be updated)
data EvalContext = EvalContext { virtualCellsMap :: CellMap
                               , rangeDescriptorsInSheet :: [RangeDescriptor] -- so we only have to get it once (pulling from DB is relatively expensive)
                               , updateAfterEval :: SheetUpdate }
                               deriving (Show, Read, Eq)

emptyContext :: EvalContext
emptyContext = EvalContext M.empty [] (SheetUpdate emptyUpdate emptyUpdate emptyUpdate emptyUpdate)

data DescendantsSetting = ProperDescendants | DescendantsWithParent deriving (Show, Read, Eq)
data AncestrySetting = SetAncestry | DontSetAncestry deriving (Show, Read, Eq)

type EvalCode = String

----------------------------------------------------------------------------------------------------------------------
-- Fat cells

--getListType :: ListKey -> String
--getListType key = last parts
--  where parts = splitBy keyPartDelimiter key

virtualRangeDescriptors :: EvalContext -> [RangeDescriptor]
virtualRangeDescriptors = applyUpdate <$> (descriptorUpdates . updateAfterEval) <*> rangeDescriptorsInSheet

virtualRangeDescriptorAt :: EvalContext -> RangeKey -> Maybe RangeDescriptor
virtualRangeDescriptorAt ctx rk = headMay $ filter ((== rk) . descriptorKey) $ virtualRangeDescriptors ctx

newCellsInContext :: EvalContext -> [ASCell]
newCellsInContext = newVals . cellUpdates . updateAfterEval

newRangeDescriptorsInContext :: EvalContext -> [RangeDescriptor]
newRangeDescriptorsInContext = newVals . descriptorUpdates . updateAfterEval

oldRangeKeysInContext :: EvalContext -> [RangeKey]
oldRangeKeysInContext = oldKeys . descriptorUpdates . updateAfterEval

indexIsHead :: ASIndex -> RangeKey -> Bool
indexIsHead idx (RangeKey idx' _) = idx == idx'

rangeKeyToIndices :: RangeKey -> [ASIndex]
rangeKeyToIndices k = rangeToIndices range
  where range = Range (locSheetId . keyIndex $ k) (rangeRect k)

rangeRect :: RangeKey -> Rect
rangeRect (RangeKey idx dims) = (tl, br)
  where 
    tl = index $ idx
    o = Offset ((width dims) -1) ((height dims) -1)
    br = shiftCoordIgnoreOutOfBounds o tl

rangeKeyToSheetId :: RangeKey -> ASSheetId
rangeKeyToSheetId = locSheetId . keyIndex

isFatCellHead :: ASCell -> Bool 
isFatCellHead c = maybe False ((== c^.cellLocation) . keyIndex) (c^.cellRangeKey)

isCoupled :: ASCell -> Bool
isCoupled = isJust . view cellRangeKey 

isEvaluable :: ASCell -> Bool
isEvaluable c = isFatCellHead c || (not $ isCoupled c)

-- assumes all the indices are in the same sheet
getFatCellIntersections :: EvalContext -> Either [ASIndex] [RangeKey] -> [RangeDescriptor]
getFatCellIntersections ctx (Left locs) = filter descriptorIntersects $ virtualRangeDescriptors ctx
  where
    indexInRect :: Rect -> ASIndex -> Bool
    indexInRect (rectCoord1, rectCoord2) (Index _ indexCoord) = a >= a' && b >= b' &&  a <= a2' && b <= b2'
      where a   = view col indexCoord
            b   = view row indexCoord
            a'  = view col rectCoord1
            b'  = view row rectCoord1
            a2' = view col rectCoord2
            b2' = view row rectCoord2
    anyLocsContainedInRect :: [ASIndex] -> Rect -> Bool
    anyLocsContainedInRect ls r = any id $ map (indexInRect r) ls
    descriptorIntersects :: RangeDescriptor -> Bool
    descriptorIntersects r = anyLocsContainedInRect locs (rangeRect . descriptorKey $ r)

-- given a list of keys and a descriptor, return True iff the descriptor intersects any of the keys
getFatCellIntersections ctx (Right keys) = descriptorsIntersectingKeys descriptors keys
  where 
    descriptors = virtualRangeDescriptors ctx
    descriptorIntersectsAnyKeyInList ks d = length (filter (\key -> keysIntersect (descriptorKey d) key) ks) > 0
    descriptorsIntersectingKeys ds ks = filter (descriptorIntersectsAnyKeyInList ks) ds
    keysIntersect k1 k2    = rectsIntersect (rangeRect k1) (rangeRect k2)
    rectsIntersect (rect1Coord1, rect1Coord2) (rect2Coord1, rect2Coord2)
      | y2 < y' = False 
      | y > y2' = False
      | x2 < x' = False 
      | x > x2' = False
      | otherwise = True 
        where
          y   = view col rect1Coord1
          y'  = view col rect2Coord1
          y2  = view col rect1Coord2
          y2' = view col rect2Coord2
          x   = view row rect1Coord1
          x'  = view row rect2Coord1
          x2  = view row rect1Coord2
          x2' = view row rect2Coord2
