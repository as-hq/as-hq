module AS.Eval.ColRangeHelpers (colRangeWithContextToIndicesRowMajor2D, colRangeWithDBAndContextToIndices, colRangeWithCellMapToRange) where

import Prelude()
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
-- and then filter! timchu, 1/4/15.

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

-- helper function in colRangeWith(DBAnd)ContextToRange
-- need to pass in sid, startCol, t, endCol, which we do by passing in a colRange.
listOfCellsByColToRange :: ASColRange -> [[ASCell]] -> ASRange
listOfCellsByColToRange c@(ColRange sid (Coord startCol t, InfiniteRowCoord endCol)) cellsByCol = 
  case startCol <= endCol of
    True ->
      let maxRowInCols = maxNonBlankRowInListOfLists cellsByCol
      in
      if (maxRowInCols < t)
         then Range sid (Coord startCol t, Coord endCol t)
         else Range sid (Coord startCol t, Coord endCol maxRowInCols)
    False -> $error "colRange startCol > endCol in listOfCellsByColToRange "

orientColRange :: ASColRange -> ASColRange
orientColRange cr@(ColRange sid (coord1, col2)) = 
  let startCol = min (view col coord1) (view col col2)
      endCol   = max (view col coord1) (view col col2)
      tl       = coord1 & col .~ startCol
      r        = InfiniteRowCoord endCol
  in
  ColRange sid (tl, r)

-- Uses the evalcontext, DB, and column range to extract the range equivalent to the ColumnRange
-- If there is nothing in the ColRange ((l, t), r), result of this function is equivalent to the range
-- Range((l,t),(r,t))
-- Note: this is very inefficient: this converts cells to indices, where they're later converted back to cells.
-- Only used in getModifiedContext
colRangeWithDBAndContextToRange :: Connection -> EvalContext -> ASColRange -> IO ASRange
colRangeWithDBAndContextToRange conn ctx cr = do
  let cellsInCol :: Col -> IO [ASCell]
      cellsInCol column = do
        let ctxCells = evalContextCellsByCol ctx sid column
        dbCells <- lookUpDBCellsByCol conn sid column
        return $ mergeCells ctxCells dbCells
      orientedCr@(ColRange sid (coord1, col2)) = orientColRange cr
  cellsByCol <- mapM cellsInCol [(coord1 ^. col).. (col2 ^. col)] -- :: [[ASIndex]]
  return $ listOfCellsByColToRange orientedCr cellsByCol

-- If there is nothing in the ColRange ((l, t), r), result of this function is equivalent to the range
-- Range((l,t),(r,t))
-- This is used in list interpolation. In this case, the evalContext has all the cells needed to
-- get the range corresponding to the column range, due to invariants within evalChain' since all the
-- ancestor cells were loaded in the beginning of dispatch.
-- Note: this is very inefficient: this converts cells to indices, where they're later converted back to cells.
-- Note: I actually don't know the best way to do this directly.

colRangeWithCellMapToRange :: CellMap -> ASColRange -> ASRange
colRangeWithCellMapToRange cellMap cr =
  let cellsInCol :: Col -> [ASCell]
      cellsInCol column = cellMapCellsByCol cellMap sid column
      orientedCr@(ColRange sid (coord1, col2)) = orientColRange cr
      cellsByCol = map cellsInCol [(coord1 ^. col).. (col2 ^. col)] -- :: [[ASIndex]]
  in
  listOfCellsByColToRange orientedCr cellsByCol

-- Uses the evalcontext and column range to extract the indices used in a column range.
-- Used in evaluateLanguage.
colRangeWithContextToIndicesRowMajor2D :: EvalContext -> ASColRange -> [[ASIndex]]
colRangeWithContextToIndicesRowMajor2D ctx c =
  rangeToIndicesRowMajor2D $ colRangeWithCellMapToRange (ctx^.virtualCellsMap) c

-- For use in conditional formatting and shortCircuit.
-- TODO: timchu, 12/29/15. Haven't checked that anything works with cond format.
colRangeWithDBAndContextToIndices :: Connection -> EvalContext -> ASColRange -> IO [ASIndex]
colRangeWithDBAndContextToIndices conn ctx cr = rangeToIndices <$> colRangeWithDBAndContextToRange conn ctx cr
