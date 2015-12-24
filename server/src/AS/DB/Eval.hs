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
-- TODO: timchu, 12/23/15. Code duplication between colRange and range cases.
referenceToCompositeValue conn ctx (ColRangeRef cr) = do
  indices <- colRangeWithContextToIndicesRowMajor2D conn ctx cr
  let indToVal ind = cellValue $ (contextMap ctx) M.! ind
      vals'    = map (map indToVal) indices
      vals = cleanEmptysFromEndOfListOfLists vals'
  return . Expanding . VList . M $ vals
referenceToCompositeValue conn ctx (RangeRef r) = return . Expanding . VList . M $ vals
  where
    indices = rangeToIndicesRowMajor2D r
    indToVal ind = cellValue $ (contextMap ctx) M.! ind
    vals    = map (map indToVal) indices


-- TODO: timchu, 12/23/15. What's the right place to put this?
-- | Helper methods for colRangeWithContextToIndices
-- gets all the indices in the DB corresponding to a particular column number.
-- TODO: timchu, the implementation of this is REALLY STUPID. Gets all cells
-- and then filter!
getCol :: ASIndex -> Col
getCol (Index _ (column, _)) = column

trailingEmpties :: [ASValue] -> Int
trailingEmpties ls = length $ takeWhile (== NoValue) $ reverse ls
-- Must return a rectangular range.
cleanEmptysFromEndOfListOfLists :: [[ASValue]] -> [[ASValue]]
cleanEmptysFromEndOfListOfLists lls = finalLists 
  where
    minEmptiesAtEndOfLists = minimum $ map trailingEmpties lls
    dropFromEnd :: Int -> [a] -> [a]
    dropFromEnd i = reverse.((drop i).reverse)
    finalLists = map (dropFromEnd minEmptiesAtEndOfLists) lls

-- TODO: timchu, need the DB lookup at beginning of dispatch to get the context,
-- don't need DB lookup for list interpolation during Eval. RIght now, I'm
-- doing the DB lookup in all cases.
lookUpDBIndicesByCol :: Connection -> ASSheetId -> Col -> IO [ASIndex]
lookUpDBIndicesByCol conn sid column =  do
  allCells <- DB.getCellsInSheet conn sid
  let allIndices = map cellLocation allCells
  return $ filter (\ind -> (getCol ind == column)) allIndices


-- gets all the indices in the EvalContext corresponding to a particular column number.
evalContextIndicesByCol :: EvalContext -> ASSheetId -> Col -> [ASIndex]
evalContextIndicesByCol (EvalContext _ addedCells _) sid column =
  let indicesInCtx = map cellLocation addedCells in
      filter (\ind -> (getCol ind == column)) indicesInCtx

-- Uses the evalcontext and column range to extract the range equivalent to the ColumnRange
-- Note; this is agnostic as to whether the EvalContext contains blank cells.
colRangeWithContextToRange :: Connection -> EvalContext -> ASColRange -> IO ASRange
colRangeWithContextToRange conn ctx c@(ColRange sid ((l, t), r)) = do
  let indicesByCol :: Col -> IO[ASIndex]
      indicesByCol column = do
        let ctxIndices = evalContextIndicesByCol ctx sid column
        dbIndices <- lookUpDBIndicesByCol conn sid column
        return $ union dbIndices ctxIndices
      maxRowInCol :: [ASIndex] -> Row
      maxRowInCol indices = maximum rowList where
        rowList = map (row.index) indices
      s = id (trace' "ColRange expanded is : " c)
  listOfIndicesByColumn <- mapM indicesByCol [l..r] -- listOfIndicesByColumn :: [[ASIndex]]
  let maxRowInCols = maximum (map maxRowInCol ((trace' "List of indices from ColRange: " listOfIndicesByColumn)))
  return $ Range sid ((l, t), (r, maxRowInCols))

-- Uses the evalcontext and column range to extract the indices used in a column range.
-- Note; this is agnostic as to whether the EvalContext contains blank cells.
colRangeWithContextToIndicesRowMajor2D :: Connection -> EvalContext -> ASColRange -> IO [[ASIndex]]
colRangeWithContextToIndicesRowMajor2D conn ctx c = do
  underlyingRange <- colRangeWithContextToRange conn ctx c
  return $ rangeToIndicesRowMajor2D underlyingRange

colRangeWithContextToIndices :: Connection -> EvalContext -> ASColRange -> IO [ASIndex]
colRangeWithContextToIndices conn ctx c = do
  underlyingRange <- colRangeWithContextToRange conn ctx c
  return $ rangeToIndices underlyingRange

refToIndices :: Connection -> ASReference -> EitherTExec [ASIndex]
refToIndices conn (IndexRef i) = return [i]
refToIndices conn (ColRangeRef cr) = lift $ colRangeWithContextToIndices conn emptyContext cr
refToIndices conn (RangeRef r) = return $ rangeToIndices r
refToIndices conn (PointerRef p) = do
  let index = pointerToIndex p 
  cell <- lift $ DB.getCell conn index 
  case cell of
    Nothing -> left IndexOfPointerNonExistant
    Just cell' -> case (cellToRangeKey cell') of
        Nothing -> left PointerToNormalCell
        Just rKey -> return $ rangeKeyToIndices rKey

-- converts ref to indices using the evalContext, then the DB, in that order.
-- because our evalContext might contain information the DB doesn't (e.g. decoupling)
-- so in the pointer case, we need to check the evalContext first for changes that might have happened during eval
refToIndicesWithContextDuringEval :: Connection -> EvalContext -> ASReference -> EitherTExec [ASIndex]
refToIndicesWithContextDuringEval conn _ (IndexRef i) = return [i]
refToIndicesWithContextDuringEval conn _ (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextDuringEval conn ctx (ColRangeRef r) = lift $ colRangeWithContextToIndices conn ctx r
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
refToIndicesWithContextBeforeEval conn ctx (ColRangeRef r) = colRangeWithContextToIndices conn ctx r
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
