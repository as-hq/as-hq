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
getCellsWithContext conn (EvalContext mp _ _) locs = map replaceWithContext <$> zip locs <$> DB.getCells conn locs
  where
    replaceWithContext (l, c) = case (M.lookup l mp) of 
      Just foundCell -> Just foundCell
      Nothing -> c

----------------------------------------------------------------------------------------------------------------------
-- Reference conversions/lookups

-- used by lookUpRef
referenceToCompositeValue :: Connection -> EvalContext -> ASReference -> IO CompositeValue
referenceToCompositeValue _ (EvalContext mp _ _) (IndexRef i) = return $ CellValue . cellValue $ mp M.! i 
referenceToCompositeValue conn ctx (PointerRef p) = do 
  let idx = pointerToIndex p
  let mp = contextMap ctx
  let cell = mp M.! idx
  case (cellExpression cell) of
    Expression _ _ -> error "Pointer to normal expression!" 
    Coupled _ _ expType rKey -> do 
      mDescriptor <- DB.getRangeDescriptorUsingContext conn ctx rKey
      case mDescriptor of
        Nothing -> error "Couldn't find range descriptor of coupled expression!"
        Just descriptor -> do 
          let indices = rangeKeyToIndices rKey
              cells  = map ((contextMap ctx) M.!) indices
              fatCell = FatCell cells descriptor
          printObj "REF TO COMPOSITE DESCRIPTOR: " descriptor
          return $ DE.recomposeCompositeValue fatCell
-- TODO: This is not the best way to do it: takes column cells, converts to indices, then converts back to values.....
referenceToCompositeValue conn ctx (ColRangeRef cr) = do
  return . Expanding . VList . M $ vals
  where
    indices = colRangeWithContextToIndicesRowMajor2D conn ctx cr
    -- The only case where the index is not in the context is when the current dispatch
    -- created new cells in the bottom of a column whose colRange is being evaluated.
    indToVal ind = case (M.member ind $ contextMap ctx) of
                        True -> cellValue $ (contextMap ctx) M.! ind
                        False -> NoValue
    vals    = map (map indToVal) indices
referenceToCompositeValue conn ctx (RangeRef r) = return . Expanding . VList . M $ vals
  where
    indices = rangeToIndicesRowMajor2D r
    indToVal ind = cellValue $ (contextMap ctx) M.! ind
    vals    = map (map indToVal) indices


-- TODO: timchu, 12/23/15. Relocate this to a different file.
-- | Helper methods for colRangeWithContextToIndices
-- gets all the indices in the DB corresponding to a particular column number.
-- TODO: timchu, the implementation of this is REALLY STUPID. Gets all cells
-- and then filter!

getCol :: ASCell -> Col
getCol (Cell (Index _ (column, _)) _ _ _) = column

getRow :: ASCell -> Row
getRow (Cell (Index _ (_, row)) _ _ _) = row

numTrailingEmptyCells :: [ASCell] -> Int
numTrailingEmptyCells ls = length $ takeWhile (\c -> (cellValue c) == NoValue) $ reverse ls

removeEmptyCellsFromEndOfList :: [ASCell] -> [ASCell]
removeEmptyCellsFromEndOfList ls = reverse $ drop (numTrailingEmptyCells ls) $ reverse ls

-- TODO: timchu, need the DB lookup at beginning of dispatch to get the context,
lookUpDBCellsByCol :: Connection -> ASSheetId -> Col -> IO [ASCell]
lookUpDBCellsByCol conn sid column =  do
  allCells <- DB.getCellsInSheet conn sid
  return $ filter (\ind -> (getCol ind == column)) allCells

-- filters for the indices in the EvalContext corresponding to a particular column number.
-- TODO: timchu, doesn't filter by sheet.
evalContextCellsByCol :: EvalContext -> ASSheetId -> Col -> [ASCell]
evalContextCellsByCol (EvalContext valMap _ _) sid column =
  filter (\c -> (getCol c == column)) $ cellsInCtx where
    cellsInCtx = M.elems valMap

equalIndex :: ASCell -> ASCell -> Bool
equalIndex cell1 cell2 = (cellLocation cell1) == (cellLocation cell2)

compareCellByRow:: ASCell -> ASCell -> Ordering
compareCellByRow c1 c2  = compare (row $ index $ cellLocation c1) (row $ index $ cellLocation c2)

-- Uses the evalcontext, DB, and column range to extract the range equivalent to the ColumnRange
-- Note; this is inefficient, as I convert cells to indices, and later in eval they're reconverted to cells. 
-- Note: I actually don't know the best way to go directly to colRanges.
colRangeWithDBAndContextToRange :: Connection -> EvalContext -> ASColRange -> IO ASRange
colRangeWithDBAndContextToRange conn ctx c@(ColRange sid ((l, t), r)) = do
  let cellsByCol :: Col -> IO [ASCell]
      cellsByCol column = do
        let ctxCells = evalContextCellsByCol ctx sid column
        dbCells <- lookUpDBCellsByCol conn sid column
        return $ unionBy equalIndex ctxCells dbCells
      maxRowWithNonBlanksInCol :: [ASCell] -> Row
      maxRowWithNonBlanksInCol cells = maximum rowList where
        cellsSortedByRows =  Data.List.sortBy (compareCellByRow) cells
        rowList = map getRow $ removeEmptyCellsFromEndOfList $ cellsSortedByRows
  listOfCellsByCol <- mapM cellsByCol [l..r] -- :: [[ASIndex]]
  let maxRowInCols = maximum (map maxRowWithNonBlanksInCol listOfCellsByCol)
  return $ Range sid ((l, t), (r, maxRowInCols))

-- TODO: timchu 12/26/15. blatant code duplication.
colRangeWithContextToRange :: Connection -> EvalContext -> ASColRange -> ASRange
colRangeWithContextToRange conn ctx c@(ColRange sid ((l, t), r)) =
  let cellsByCol :: Col -> [ASCell]
      cellsByCol column = evalContextCellsByCol ctx sid column
      -- must sort cells by row in order for removeEmptyCellsAtEndOfList to work
      maxRowWithNonBlanksInCol :: [ASCell] -> Row
      maxRowWithNonBlanksInCol cells = maximum rowList where
        cellsSortedByRows =  Data.List.sortBy (compareCellByRow) cells
        rowList = map getRow $ removeEmptyCellsFromEndOfList $ cellsSortedByRows
      listOfCellsByCol = map cellsByCol [l..r] -- :: [[ASIndex]]
      maxRowInCols = maximum (map maxRowWithNonBlanksInCol listOfCellsByCol)
   in
   Range sid ((l, t), (r, maxRowInCols))

-- TODO: timchu, leave very clear comments about where these are used!
-- Uses the evalcontext and column range to extract the indices used in a column range.
-- For use in evaluateLanguage.
colRangeWithContextToIndicesRowMajor2D :: Connection -> EvalContext -> ASColRange -> [[ASIndex]]
colRangeWithContextToIndicesRowMajor2D conn ctx c = rangeToIndicesRowMajor2D $ colRangeWithContextToRange conn ctx c

-- For use in conditional formatting and shortCircuit.
-- TODO: TIMCHU, STOP POINT 12/25. THERES ANA ERROR IN HERE SOMEWHERE
-- AM I IN THE WORLD WHERE I KNOW ENOUGH TO DEBUG THIS?
-- AFTER THIS
colRangeWithDBAndContextToIndices :: Connection -> EvalContext -> ASColRange -> IO [ASIndex]
colRangeWithDBAndContextToIndices conn ctx@(EvalContext vmap _ _) c = do
  underlyingRange <- colRangeWithDBAndContextToRange conn ctx c
  return $ rangeToIndices underlyingRange

--
---- TODO: timchu, 12/25/15. Never used.
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
refToIndicesWithContextDuringEval conn ctx@(EvalContext mp _ _) (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextDuringEval conn ctx@(EvalContext mp _ _) (ColRangeRef r) = lift $ colRangeWithDBAndContextToIndices conn ctx r
refToIndicesWithContextDuringEval conn (EvalContext mp _ _) (PointerRef p) = do
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
refToIndicesWithContextBeforeEval conn (EvalContext mp _ _) (PointerRef p) = do
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
