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

import Control.Lens

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (isJust)


-- For internal use only. Represents a "cell" that takes up numerous cells (e.g., range(10)).
data FatCell = FatCell { expandedCells :: [ASCell], descriptor :: RangeDescriptor } deriving (Show, Read)
data CompositeCell = Single ASCell | Fat FatCell

type CellMap = M.Map ASIndex ASCell

-- This should be thought of as a mini spreadsheet used by eval as a cache (which can be updated)
data EvalContext = EvalContext { virtualCellsMap :: CellMap
                               , updateAfterEval :: SheetUpdate }
                               deriving (Show, Read, Eq)

emptyContext :: EvalContext
emptyContext = EvalContext M.empty (SheetUpdate emptyUpdate emptyUpdate emptyUpdate emptyUpdate)

data DescendantsSetting = ProperDescendants | DescendantsWithParent deriving (Show, Read, Eq)
data AncestrySetting = SetAncestry | DontSetAncestry deriving (Show, Read, Eq)

----------------------------------------------------------------------------------------------------------------------
-- Fat cells

--getListType :: ListKey -> String
--getListType key = last parts
--  where parts = splitBy keyPartDelimiter key

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
    Index _ (col, row) = idx
    tl = (col, row)
    br = (col + (width dims) - 1, row + (height dims) - 1)

rangeKeyToSheetId :: RangeKey -> ASSheetId
rangeKeyToSheetId = locSheetId . keyIndex

isFatCellHead :: ASCell -> Bool 
isFatCellHead c = maybe False ((== c^.cellLocation) . keyIndex) (c^.cellRangeKey)

isCoupled :: ASCell -> Bool
isCoupled = isJust . view cellRangeKey 

isEvaluable :: ASCell -> Bool
isEvaluable c = isFatCellHead c || (not $ isCoupled c)
