module AS.Eval.ColRangeHelpers (rangeWithContextToIndicesRowMajor2D, rangeWithDBAndContextToIndices, rangeWithCellMapToFiniteRange) where

import AS.Prelude

import AS.Types.Cell
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.DB
import AS.Util

import qualified AS.DB.API as DB
import AS.Logging

import qualified Data.Map as M
import qualified Data.List as L

import Database.Redis hiding (decode)
import Data.List
import Control.Monad
import Control.Lens hiding (set, index)
import Control.Monad.Trans.Class

-- | Helper methods for colRangeWithContextToIndices. Used in colRange list interpolation.
-- gets all the indices in the DB corresponding to a particular column number.
-- The implementation of this is REALLY STUPID. Gets all cells
-- and then filters and then turns them into indices -- in later
-- methods,, these are turned back into cells! timchu, 1/4/15.

-- COLRANGE LIST INTERPOLATION NOTE: GUARANTEES ON THE EVALCONTEXT.
-- colRange list interpolation doesn't guarantee that the evalContext in evalchain' 
-- has all the cells in the column, but it does guarantee that
-- it has all the non-empty cells in that column.

-- #lenses.
getCellCol :: ASCell -> Col
getCellCol c = c^.cellLocation.index.col

getCellRow :: ASCell -> Row
getCellRow c = c^.cellLocation.index.row

numTrailingEmptyCells :: [ASCell] -> Int
numTrailingEmptyCells ls = length $ takeWhile (\c -> (view cellValue c) == NoValue) $ reverse ls

removeEmptyCellsFromEndOfList :: [ASCell] -> [ASCell]
removeEmptyCellsFromEndOfList ls = reverse $ drop (numTrailingEmptyCells ls) $ reverse ls

-- TODO: timchu, need the DB lookup at beginning of dispatch to get the context,
-- This is inefficient. Filters all cells in the sheet and checks if it's in a column.
lookUpDBCellsByCol :: Connection -> ASSheetId -> Col -> IO [ASCell]
lookUpDBCellsByCol conn sid column =  do
  allCells <- DB.getCellsInSheet conn sid
  return $ filter (\ind -> (getCellCol ind == column)) allCells

-- filters for the indices in the EvalContext corresponding to a particular column number.
evalContextCellsByCol :: EvalContext -> ASSheetId -> Col -> [ASCell]
evalContextCellsByCol ctx sid column = cellMapCellsByCol (ctx^.virtualCellsMap) sid column

cellMapCellsByCol :: CellMap -> ASSheetId -> Col -> [ASCell]
cellMapCellsByCol virtualCellsMap sid column = filter isInMyColumn cellsInCtx 
  where
    isInMyColumn c = getCellCol c == column && (c^.cellLocation.locSheetId == sid)
    cellsInCtx = M.elems virtualCellsMap

compareCellByRow:: ASCell -> ASCell -> Ordering
compareCellByRow c1 c2  = compare (c1^.cellLocation.index.row) (c2^.cellLocation.index.row)

-- gives the maximum non-blank row of a list of cells. Used in colRange interpolation.
-- if the list contains only NoValues, return 0.
maxNonBlankRow :: [ASCell] -> Row
maxNonBlankRow cells =
  -- must sort cells by row in order for removeEmptyCellsAtEndOfList to work
  let cellsSortedByRows =  L.sortBy (compareCellByRow) cells
      rowListWithEmptiesRemovedFromEnd = map getCellRow $ removeEmptyCellsFromEndOfList $ cellsSortedByRows
   in
   case rowListWithEmptiesRemovedFromEnd of
        [] -> 0
        otherwise  -> maximum rowListWithEmptiesRemovedFromEnd

-- gives the maximum non-blank row in a list of list of cells. Used in colRange interpolation.
maxNonBlankRowInListOfLists :: [[ASCell]] -> Row
maxNonBlankRowInListOfLists ll = maximum (map maxNonBlankRow ll)

--This is only used in minimalBoundingFiniteRange as a way to build finite ranges
--from the top left coord, the bottom right col, and the bottom left row of the
--range you want to construct.
makeFiniteRange' :: ASSheetId -> Coord -> Col -> Row -> ASRange
makeFiniteRange' sid coord1 col row = makeFiniteRange sid coord1 (makeCoord col row)

-- helper function in colRangeWith(DBAnd)ContextToFiniteRange
-- Creates the minimal finite range containing [[ASCell]], Coord, and Col
minimalBoundingFiniteRange :: ASSheetId -> Coord -> Col -> [[ASCell]] -> ASRange
minimalBoundingFiniteRange sid tl rCol cellsByCol = 
  let maxRowInCols = maxNonBlankRowInListOfLists cellsByCol
      maxRowNum = max maxRowInCols $ tl^.row
  in
  makeFiniteRange' sid tl rCol maxRowNum

-- Uses the evalcontext, DB, and column range to extract the range equivalent to the ColumnRange
-- If there is nothing in the ColRange ((l, t), r), result of this function is equivalent to the range
-- Range((l,t),(r,t))
-- Note: this is very inefficient: this converts cells to indices, where they're later converted back to cells.
-- Only used in getModifiedContext
-- Guarantees: ASRange passed in is guaranteed to be a colrange.
-- #RoomForImprovement. Should just pass in the upper left, the column, and the sheet Id.
-- Or have some wrapper to do that. Currently there's a really deep pattern match.
-- Only used when ASRange is a colrange.
colRangeWithDBAndContextToFiniteRange :: Connection -> EvalContext -> ASRange -> IO ASRange
colRangeWithDBAndContextToFiniteRange conn ctx cr@(Range sid (coord1, (Finite col2, Infinite))) = do
  let cellsInCol :: Col -> IO [ASCell]
      cellsInCol column = do
        let ctxCells = evalContextCellsByCol ctx sid column
        dbCells <- lookUpDBCellsByCol conn sid column
        return $ mergeCells ctxCells dbCells
  cellsByCol <- mapM cellsInCol [(coord1 ^. col).. col2] -- :: [[ASIndex]]
  return $ minimalBoundingFiniteRange sid coord1 col2 cellsByCol

-- If there is nothing in the ColRange ((l, t), r), result of this function is equivalent to the range
-- Range((l,t),(r,t))
-- This is used in list interpolation. In this case, the evalContext has all the cells needed to
-- get the range corresponding to the column range, due to invariants within evalChain' since all the
-- ancestor cells were loaded in the beginning of dispatch.
-- Note: this is very inefficient: this converts cells to indices, where they're later converted back to cells.
-- Note: I actually don't know the best way to do this directly.

-- Caller guarantees: ASRange passed in is guaranteed to be a colrange.
-- #RoomForImprovement. Pass in col range description (sid, top left coord, 
-- rightmost col).

colRangeWithCellMapToFiniteRange :: CellMap -> ASRange -> ASRange
colRangeWithCellMapToFiniteRange cellMap (Range sid (coord1,  (Finite col2, Infinite))) =
  let cellsInCol :: Col -> [ASCell]
      cellsInCol column = cellMapCellsByCol cellMap sid column
      cellsByCol = map cellsInCol [(coord1 ^. col).. col2] -- :: [[ASIndex]]
  in
  minimalBoundingFiniteRange sid coord1 col2 cellsByCol

-- Uses the evalcontext and column range to extract the indices used in a column range.
-- Used in evaluateLanguage.
-- Range passed in should be a ColRange.
colRangeWithContextToIndicesRowMajor2D :: EvalContext -> ASRange -> [[ASIndex]]
colRangeWithContextToIndicesRowMajor2D ctx rng =
  finiteRangeToIndicesRowMajor2D $ colRangeWithCellMapToFiniteRange (ctx^.virtualCellsMap) rng

-- For use in conditional formatting and shortCircuit.
-- Caller Requirement: Range passed in must be a ColRange.
colRangeWithDBAndContextToIndices :: Connection -> EvalContext -> ASRange -> IO [ASIndex]
colRangeWithDBAndContextToIndices conn ctx rng = do
  r <- colRangeWithDBAndContextToFiniteRange conn ctx rng
  return $ finiteRangeToIndices r

-- Applies the first function to a given range if the range is  finite, and the
-- second function to the range if the range has infinite rows and finite cols.
caseOnRangeType :: (ASRange -> a) -> (ASRange -> a) -> ASRange -> a
caseOnRangeType transformOnFiniteRange transformOnColRange rng
  | isFiniteRange rng = transformOnFiniteRange rng
  | isColRange rng = transformOnColRange rng

rangeWithCellMapToFiniteRange :: CellMap -> ASRange -> ASRange
rangeWithCellMapToFiniteRange cellMap =
  caseOnRangeType id (colRangeWithCellMapToFiniteRange cellMap)

rangeWithContextToIndicesRowMajor2D :: EvalContext -> ASRange -> [[ASIndex]]
rangeWithContextToIndicesRowMajor2D ctx =
  caseOnRangeType finiteRangeToIndicesRowMajor2D (colRangeWithContextToIndicesRowMajor2D ctx)

rangeWithDBAndContextToIndices :: Connection -> EvalContext -> ASRange -> IO [ASIndex]
rangeWithDBAndContextToIndices conn ctx =
  caseOnRangeType (return . finiteRangeToIndices) (colRangeWithDBAndContextToIndices conn ctx)
