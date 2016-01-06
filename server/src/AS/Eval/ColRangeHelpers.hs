module AS.Eval.ColRangeHelpers (colRangeWithContextToIndicesRowMajor2D, colRangeWithDBAndContextToIndices) where

import Prelude()
import AS.Prelude

import AS.Types.Cell
import AS.Types.Errors
import AS.Types.Eval as E
import AS.Types.DB
import AS.Util

import qualified AS.DB.API as DB
import AS.Logging

import qualified Data.Map as M
import qualified Data.List as L

import Database.Redis hiding (decode)
import Data.List
import Control.Monad
import Control.Lens hiding (set)
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
getCellCol (Cell (Index _ (column, _)) _ _ _ _) = column

getCellRow :: ASCell -> Row
getCellRow (Cell (Index _ (_, row)) _ _ _ _) = row

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
evalContextCellsByCol (EvalContext virtualCellsMap _ _) sid column =
  filter (\c -> (getCellCol c == column && locSheetId (c^.cellLocation) == sid)) $ cellsInCtx where
    cellsInCtx = M.elems virtualCellsMap

compareCellByRow:: ASCell -> ASCell -> Ordering
compareCellByRow c1 c2  = compare (row $ E.index $ view cellLocation c1) (row $ E.index $ view cellLocation c2)

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
listOfCellsByColToRange c@(ColRange sid ((startCol, t), endCol)) cellsByCol = 
  case startCol <= endCol of
    True ->
      let maxRowInCols = maxNonBlankRowInListOfLists cellsByCol
      in
      if (maxRowInCols < t)
         then Range sid ((startCol, t), (endCol, t))
         else Range sid ((startCol, t), (endCol, maxRowInCols))
    False -> $error "colRange startCol > endCol in listOfCellsByColToRange "

-- Uses the evalcontext, DB, and column range to extract the range equivalent to the ColumnRange
-- If there is nothing in the ColRange ((l, t), r), result of this function is equivalent to the range
-- Range((l,t),(r,t))
-- Note: this is very inefficient: this converts cells to indices, where they're later converted back to cells.
-- Only used in getModifiedContext
colRangeWithDBAndContextToRange :: Connection -> EvalContext -> ASColRange -> IO ASRange
colRangeWithDBAndContextToRange conn ctx c@(ColRange sid ((l, t), r)) = do
  let cellsInCol :: Col -> IO [ASCell]
      cellsInCol column = do
        let ctxCells = evalContextCellsByCol ctx sid column
        dbCells <- lookUpDBCellsByCol conn sid column
        return $ mergeCells ctxCells dbCells
      startCol = min l r
      endCol = max l r
  cellsByCol <- mapM cellsInCol [startCol..endCol] -- :: [[ASIndex]]
  return $ listOfCellsByColToRange (ColRange sid ((startCol, t), endCol)) cellsByCol

-- If there is nothing in the ColRange ((l, t), r), result of this function is equivalent to the range
-- Range((l,t),(r,t))
-- This is used in list interpolation. In this case, the evalContext has all the cells needed to
-- get the range corresponding to the column range, due to invariants within evalChain' since all the
-- ancestor cells were loaded in the beginning of dispatch.
-- Note: this is very inefficient: this converts cells to indices, where they're later converted back to cells.
-- Note: I actually don't know the best way to do this directly.
colRangeWithContextToRange :: Connection -> EvalContext -> ASColRange -> ASRange
colRangeWithContextToRange conn ctx c@(ColRange sid ((l, t), r)) =
  let cellsInCol :: Col -> [ASCell]
      cellsInCol column = evalContextCellsByCol ctx sid column
      startCol = min l r
      endCol = max l r
      cellsByCol = map cellsInCol [startCol..endCol] -- :: [[ASIndex]]
  in
  listOfCellsByColToRange (ColRange sid ((startCol, t), endCol)) cellsByCol

-- Uses the evalcontext and column range to extract the indices used in a column range.
-- Used in evaluateLanguage.
colRangeWithContextToIndicesRowMajor2D :: Connection -> EvalContext -> ASColRange -> [[ASIndex]]
colRangeWithContextToIndicesRowMajor2D conn ctx c = rangeToIndicesRowMajor2D $ colRangeWithContextToRange conn ctx c

-- For use in conditional formatting and shortCircuit.
-- TODO: timchu, 12/29/15. Haven't checked that anything works with cond format.
colRangeWithDBAndContextToIndices :: Connection -> EvalContext -> ASColRange -> IO [ASIndex]
colRangeWithDBAndContextToIndices conn ctx cr = rangeToIndices <$> colRangeWithDBAndContextToRange conn ctx cr
