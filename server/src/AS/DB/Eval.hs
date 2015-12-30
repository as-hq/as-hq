module AS.DB.Eval where

import Prelude

import AS.Types.Cell
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.DB
import AS.Util
import qualified AS.Dispatch.Expanding as DE

import qualified AS.DB.API as DB
import AS.Logging

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

import Database.Redis hiding (decode)
import Data.List
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------
-- Cell lookups

-- looks up cells in the given context, then in the database, in that precedence order
-- this function is order-preserving
getCellsWithContext :: Connection -> EvalContext -> [ASIndex] -> IO [Maybe ASCell]
getCellsWithContext conn EvalContext { virtualCellsMap = mp } locs = map replaceWithContext <$> zip locs <$> DB.getCells conn locs
  where
    replaceWithContext (l, c) = case (M.lookup l mp) of 
      Just foundCell -> Just foundCell
      Nothing -> c

----------------------------------------------------------------------------------------------------------------------
-- Reference conversions/lookups

-- used by lookUpRef
referenceToCompositeValue :: Connection -> EvalContext -> ASReference -> IO CompositeValue
referenceToCompositeValue _ (EvalContext { virtualCellsMap = mp }) (IndexRef i) = return $ CellValue . cellValue $ mp M.! i 
referenceToCompositeValue conn ctx (PointerRef p) = do 
  let idx = pointerToIndex p
  let mp = virtualCellsMap ctx
  let cell = mp M.! idx
  case (cellExpression cell) of
    Expression _ _ -> error "Pointer to normal expression!" 
    Coupled _ _ expType rKey -> do 
      mDescriptor <- DB.getRangeDescriptorUsingContext conn ctx rKey
      case mDescriptor of
        Nothing -> error "Couldn't find range descriptor of coupled expression!"
        Just descriptor -> do 
          let indices = rangeKeyToIndices rKey
              cells  = map ((virtualCellsMap ctx) M.!) indices
              fatCell = FatCell cells descriptor
          printObj "REF TO COMPOSITE DESCRIPTOR: " descriptor
          return $ DE.recomposeCompositeValue fatCell
-- TODO: This is not the best way to do it: takes column cells, converts to indices, then converts back to values.....
referenceToCompositeValue conn ctx (ColRangeRef cr) = do
  return . Expanding . VList . M $ vals
  where
    indices = colRangeWithContextToIndicesRowMajor2D conn ctx cr
    -- The only case where the index is not in the virtualCellsMap is when the
    -- current dispatch created new cells in the bottom of a column whose
    -- colRange is being evaluated.
    indToVal ind = case (M.member ind $ virtualCellsMap ctx) of
                        True -> cellValue $ (virtualCellsMap ctx) M.! ind
                        False -> NoValue
    vals    = map (map indToVal) indices
referenceToCompositeValue conn ctx (RangeRef r) = return . Expanding . VList . M $ vals
  where
    indices = rangeToIndicesRowMajor2D r
    indToVal ind = cellValue $ (virtualCellsMap ctx) M.! ind
    vals    = map (map indToVal) indices


-- | Helper methods for colRangeWithContextToIndices
-- gets all the indices in the DB corresponding to a particular column number.
-- TODO: timchu, the implementation of this is REALLY STUPID. Gets all cells
-- and then filter!

-- TODO: timchu, 12/23/15. Relocate this to a different file.
getCol :: ASCell -> Col
getCol (Cell (Index _ (column, _)) _ _ _) = column

getRow :: ASCell -> Row
getRow (Cell (Index _ (_, row)) _ _ _) = row

numTrailingEmptyCells :: [ASCell] -> Int
numTrailingEmptyCells ls = length $ takeWhile (\c -> (cellValue c) == NoValue) $ reverse ls

removeEmptyCellsFromEndOfList :: [ASCell] -> [ASCell]
removeEmptyCellsFromEndOfList ls = reverse $ drop (numTrailingEmptyCells ls) $ reverse ls

-- TODO: timchu, need the DB lookup at beginning of dispatch to get the context,
-- This is inefficient. Filters all cells in the sheet and checks if it's in a column.
lookUpDBCellsByCol :: Connection -> ASSheetId -> Col -> IO [ASCell]
lookUpDBCellsByCol conn sid column =  do
  allCells <- DB.getCellsInSheet conn sid
  return $ filter (\ind -> (getCol ind == column)) allCells

-- filters for the indices in the EvalContext corresponding to a particular column number.
-- TODO: timchu, doesn't filter by sheet.
evalContextCellsByCol :: EvalContext -> ASSheetId -> Col -> [ASCell]
evalContextCellsByCol (EvalContext virtualCellsMap _) sid column =
  filter (\c -> (getCol c == column)) $ cellsInCtx where
    cellsInCtx = M.elems virtualCellsMap

equalIndex :: ASCell -> ASCell -> Bool
equalIndex cell1 cell2 = (cellLocation cell1) == (cellLocation cell2)

compareCellByRow:: ASCell -> ASCell -> Ordering
compareCellByRow c1 c2  = compare (row $ index $ cellLocation c1) (row $ index $ cellLocation c2)

-- gives the maximum non-blank row of a list of cells. Used in colRange interpolation.
-- if the list contains only NoValues, return 0.
maxNonBlankRow :: [ASCell] -> Row
maxNonBlankRow cells =
  -- must sort cells by row in order for removeEmptyCellsAtEndOfList to work
  let cellsSortedByRows =  Data.List.sortBy (compareCellByRow) cells
      rowListWithEmptiesRemovedFromEnd = map getRow $ removeEmptyCellsFromEndOfList $ cellsSortedByRows
   in
   case rowListWithEmptiesRemovedFromEnd of
        [] -> 0
        otherwise  -> maximum rowListWithEmptiesRemovedFromEnd

-- gives the maximum non-blank row in a list of list of cells. Used in colRange interpolation.
maxNonBlankRowInListOfLists :: [[ASCell]] -> Row
maxNonBlankRowInListOfLists ll = maximum (map maxNonBlankRow ll)

-- Uses the evalcontext, DB, and column range to extract the range equivalent to the ColumnRange
-- Note; this is inefficient, as I convert cells to indices, and later in eval they're reconverted to cells.
-- Note: I actually don't know the best way to do this directly.
-- Only used in getModifiedContext
colRangeWithDBAndContextToRange :: Connection -> EvalContext -> ASColRange -> IO ASRange
colRangeWithDBAndContextToRange conn ctx c@(ColRange sid ((l, t), r)) = do
  let cellsByCol :: Col -> IO [ASCell]
      cellsByCol column = do
        let ctxCells = evalContextCellsByCol ctx sid column
        dbCells <- lookUpDBCellsByCol conn sid column
        return $ unionBy equalIndex ctxCells dbCells
      startCol = min l r
      endCol = max l r
  listOfCellsByCol <- mapM cellsByCol [startCol..endCol] -- :: [[ASIndex]]
  let maxRowInCols = maxNonBlankRowInListOfLists listOfCellsByCol
  -- TODO: timchu,code duplication.
  -- handles the case if maxRowInCols  is smaller than the top of the colRange.
  if (maxRowInCols < t)
     then return $ Range sid ((startCol, t), (endCol, t))
     else return $ Range sid ((startCol, t), (endCol, maxRowInCols))

-- TODO: timchu 12/26/15. blatant code duplication. Don't know how to unduplicate further.
-- If there is nothing in the ColRange ((l, t), r), result of this function is equivalent to the range
-- Range((l,t),(r,t))
colRangeWithContextToRange :: Connection -> EvalContext -> ASColRange -> ASRange
colRangeWithContextToRange conn ctx c@(ColRange sid ((l, t), r)) =
  let cellsByCol :: Col -> [ASCell]
      cellsByCol column = evalContextCellsByCol ctx sid column
      startCol = min l r
      endCol = max l r
      listOfCellsByCol = map cellsByCol [startCol..endCol] -- :: [[ASIndex]]
      maxRowInCols = maxNonBlankRowInListOfLists listOfCellsByCol
   in
   if (maxRowInCols < t)
      then Range sid ((startCol, t), (endCol, t))
      else Range sid ((startCol, t), (endCol, maxRowInCols))

-- TODO: timchu, leave very clear comments about where these are used!
-- Uses the evalcontext and column range to extract the indices used in a column range.
-- For use in evaluateLanguage.
colRangeWithContextToIndicesRowMajor2D :: Connection -> EvalContext -> ASColRange -> [[ASIndex]]
colRangeWithContextToIndicesRowMajor2D conn ctx c = rangeToIndicesRowMajor2D $ colRangeWithContextToRange conn ctx c

-- For use in conditional formatting and shortCircuit.
-- TODO: timchu, 12/29/15. Haven't checked that anything works with cond format.
colRangeWithDBAndContextToIndices :: Connection -> EvalContext -> ASColRange -> IO [ASIndex]
colRangeWithDBAndContextToIndices conn ctx cr = do
  underlyingRange <- colRangeWithDBAndContextToRange conn ctx cr
  return $ rangeToIndices underlyingRange


---- TODO: timchu, 12/25/15. Never used. Delete this if never needs to be used.
--colRangeWithContextToIndices :: Connection -> EvalContext -> ASColRange -> [ASIndex]
--colRangeWithContextToIndices conn ctx c = rangeToIndices $ colRangeWithContextToRange conn ctx c

-- Only used in conditional formatting.
-- TODO: timchu, 12/25/15. not sure if return $ colRange ... or  lift colRangeWithDB..
refToIndices :: Connection -> ASReference -> EitherTExec [ASIndex]
refToIndices conn (IndexRef i) = return [i]
refToIndices conn (ColRangeRef cr) = lift $ colRangeWithDBAndContextToIndices conn emptyContext cr
refToIndices conn (RangeRef r) = return $ rangeToIndices r
refToIndices conn (PointerRef p) = do
  let index = pointerToIndex p
  cell <- lift $ DB.getCell conn index
  case cell of
    Nothing -> left IndexOfPointerNonExistant
    Just cell' -> case (cellToRangeKey cell') of
        Nothing -> left PointerToNormalCell
        Just rKey -> return $ rangeKeyToIndices rKey

-- TODO: timchu, 12/25/15. Only used in shortcircuit? Not sure if I need a DB lookup in this case.
-- converts ref to indices using the evalContext, then the DB, in that order.

-- because our evalContext might contain information the DB doesn't (e.g. decoupling)
-- so in the pointer case, we need to check the evalContext first for changes that might have happened during eval
refToIndicesWithContextDuringEval :: Connection -> EvalContext -> ASReference -> EitherTExec [ASIndex]
refToIndicesWithContextDuringEval conn _ (IndexRef i) = return [i]
refToIndicesWithContextDuringEval conn _ (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextDuringEval conn ctx (ColRangeRef cr) = lift $ colRangeWithDBAndContextToIndices conn ctx cr
refToIndicesWithContextDuringEval conn (EvalContext { virtualCellsMap = mp }) (PointerRef p) = do -- #record
  let index = pointerToIndex p
  case (M.lookup index mp) of 
    Just (Cell _ (Coupled _ _ _ rKey) _ _) -> return $ rangeKeyToIndices rKey
    Just (Cell _ (Expression _ _) _ _) -> left $ PointerToNormalCell
    Nothing -> do
      cell <- lift $ DB.getCell conn index 
      case cell of
        Nothing -> left IndexOfPointerNonExistant
        Just cell' -> case (cellToRangeKey cell') of
            Nothing -> left PointerToNormalCell
            Just rKey -> return $ rangeKeyToIndices rKey

-- This is the function we use to convert ref to indices for updating the map PRIOR TO eval. There are some cases where we don't flip a shit. 
-- For example, if the map currently has A1 as a normal expression, and we have @A1 somewhere downstream, we won't flip a shit, and instead expect that
-- by the time the pointer is evalled, A1 will have a coupled expression due to toposort. We flip a shit if it's not the case then. 
refToIndicesWithContextBeforeEval :: Connection -> EvalContext -> ASReference -> IO [ASIndex]
refToIndicesWithContextBeforeEval conn _ (IndexRef i) = return [i]
refToIndicesWithContextBeforeEval conn _ (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextBeforeEval conn ctx (ColRangeRef r) = colRangeWithDBAndContextToIndices conn ctx r
refToIndicesWithContextBeforeEval conn (EvalContext { virtualCellsMap = mp }) (PointerRef p) = do -- #record
  let index = pointerToIndex p
  case (M.lookup index mp) of 
    Just (Cell _ (Coupled _ _ _ rKey) _ _) -> return $ rangeKeyToIndices rKey
    Just (Cell _ (Expression _ _) _ _) -> return []
    Nothing -> do
      cell <- DB.getCell conn index 
      case cell of
        Nothing -> return []
        Just cell' -> case (cellToRangeKey cell') of
            Nothing -> return []
            Just rKey -> return $ rangeKeyToIndices rKey

----------------------------------------------------------------------------------------------------------------------------------------------
-- Header expressions handlers

getEvalHeader :: Connection -> ASSheetId -> ASLanguage -> IO String
getEvalHeader conn sid lang = runRedis conn $ do 
  msg <- get . toRedisFormat $ EvalHeaderKey sid lang
  return $ case msg of 
    Right (Just msg') -> BC.unpack msg'
    Right Nothing -> ""
    Left _            -> error "Failed to retrieve eval header"

setEvalHeader :: Connection -> ASSheetId -> ASLanguage -> String -> IO ()
setEvalHeader conn sid lang xp = runRedis conn $ do
  set (toRedisFormat $ EvalHeaderKey sid lang) (BC.pack xp)
  return ()
