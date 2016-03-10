module AS.Types.Eval
  ( module AS.Types.Eval
  , module AS.Types.Locations
  , module AS.Types.Errors
  , module AS.Types.RangeDescriptor
  , module AS.Types.Values
  ) where

import GHC.Generics
import Control.Lens hiding (index)
import Control.Applicative ((<$>), (<*>))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import Safe (headMay)

import AS.Types.CellProps
import AS.Types.Values
import AS.Types.Locations
import AS.Types.RangeDescriptor
import AS.Types.Errors
import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Updates
import AS.ASJSON


-- For internal use only. Represents a "cell" that takes up numerous cells (e.g., range(10)).
data FatCell = FatCell { expandedCells :: [ASCell], descriptor :: RangeDescriptor } deriving (Show, Read)
data CompositeCell = Single ASCell | Fat FatCell

type CellMap = M.Map ASIndex ASCell

-- This should be thought of as a mini spreadsheet used by eval as a cache (which can be updated)
data EvalContext = EvalContext { _virtualCellsMap :: CellMap
                               , _rangeDescriptorsInSheet :: [RangeDescriptor] -- so we only have to get it once (pulling from DB is relatively expensive)
                               , _updateAfterEval :: SheetUpdate }
                               deriving (Show, Read, Eq)
makeLenses ''EvalContext

emptyContext :: EvalContext
emptyContext = EvalContext M.empty [] (SheetUpdate emptyUpdate emptyUpdate emptyUpdate emptyUpdate)

data DescendantsSetting = ProperDescendants | DescendantsWithParent deriving (Show, Read, Eq)
data AncestrySetting = SetAncestry | DontSetAncestry deriving (Show, Read, Eq)

type EvalCode = String

data EvalResult = EvalResult { _resultValue :: CompositeValue, _resultDisplay :: Maybe String } deriving (Show, Read, Eq, Generic)
data HeaderResult = HeaderResult { _headerValue :: String, _headerDisplay :: Maybe String} deriving (Show, Read, Eq, Generic)

emptyResult :: EvalResult
emptyResult = EvalResult (CellValue NoValue) Nothing

makeLenses ''EvalResult
makeLenses ''HeaderResult
asLensedToJSON ''HeaderResult -- only used for sending header
----------------------------------------------------------------------------------------------------------------------
-- Fat cells

--getListType :: ListKey -> String
--getListType key = last parts
--  where parts = splitBy keyPartDelimiter key

-- #RoomForImprovement. Could use apply2way, from Data.Function.Tools
virtualRangeDescriptors :: EvalContext -> [RangeDescriptor]
virtualRangeDescriptors ctx = applyUpdate (ctx^.updateAfterEval.descriptorUpdates) (ctx^.rangeDescriptorsInSheet)

virtualRangeDescriptorAt :: EvalContext -> RangeKey -> Maybe RangeDescriptor
virtualRangeDescriptorAt ctx rk = headMay $ filter ((== rk) . descriptorKey) $ virtualRangeDescriptors ctx

newCellsInContext :: EvalContext -> [ASCell]
newCellsInContext = view (updateAfterEval.cellUpdates.newVals)

newCellsInContextSet :: EvalContext -> S.Set ASCell
newCellsInContextSet = view (updateAfterEval.cellUpdates.newValsSet)

newRangeDescriptorsInContext :: EvalContext -> [RangeDescriptor]
newRangeDescriptorsInContext = view (updateAfterEval.descriptorUpdates.newVals)

oldRangeKeysInContext :: EvalContext -> [RangeKey]
oldRangeKeysInContext = view (updateAfterEval.descriptorUpdates.oldKeys)

indexIsHead :: ASIndex -> RangeKey -> Bool
indexIsHead idx (RangeKey idx' _) = idx == idx'

rangeKeyToIndices :: RangeKey -> [ASIndex]
rangeKeyToIndices k = rangeToIndices range
  where range = Range (view locSheetId.keyIndex $ k) (rangeRect k)

rangeRect :: RangeKey -> Rect
rangeRect (RangeKey idx dims) = (tl, br)
  where 
    tl = idx^.index
    o = Offset ((width dims) -1) ((height dims) -1)
    br = shiftCoordIgnoreOutOfBounds o tl

rangeKeyToSheetId :: RangeKey -> ASSheetId
rangeKeyToSheetId = view locSheetId . keyIndex

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
    keysIntersect k1 k2 = rectsIntersect (rangeRect k1) (rangeRect k2)
    rectsIntersect (rect1Coord1, rect1Coord2) (rect2Coord1, rect2Coord2)
      | y2 < y' = False 
      | y > y2' = False
      | x2 < x' = False 
      | x > x2' = False
      | otherwise = True 
        where
          (y, x)     = (view col rect1Coord1, view row rect1Coord1) -- top left in rect 1
          (y', x')   = (view col rect2Coord1, view row rect2Coord1) -- bottom right in rect 1
          (y2, x2)   = (view col rect1Coord2, view row rect1Coord2) -- top left in rect 2
          (y2', x2') = (view col rect2Coord2, view row rect2Coord2) -- bottom right in rect 2


-- #needsrefactor -- should add blank cells locations to oldKeys, rather than to newly added cells. 
-- Helper function that adds cells to a context, by merging them to addedCells and the map (with priority). #lens
addCellsToContext :: [ASCell] -> EvalContext -> EvalContext
addCellsToContext cells ctx =
  ctx & virtualCellsMap .~ newMap & (updateAfterEval . cellUpdates) %~ (insertCellsIntoUpdate cells)
    where
      newMap        = insertMultiple (ctx^.virtualCellsMap) (mapCellLocation cells) cells

-- Inserts multiple elements into a map
insertMultiple :: (Ord k) => M.Map k v -> [k] -> [v] -> M.Map k v
insertMultiple mp keys values = L.foldl' (\curMap (key,value) -> M.insert key value curMap) mp assoc
  where assoc = zip keys values
