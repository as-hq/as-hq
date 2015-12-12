module AS.DB.Eval where

import Prelude

import AS.Types.Cell
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.DB

import qualified AS.DB.API as DB

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

import Database.Redis hiding (decode)
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

refToIndices :: Connection -> ASReference -> EitherTExec [ASIndex]
refToIndices conn (IndexRef i) = return [i]
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
  msg <- get $ makeEvalHeaderKey sid lang
  return $ case msg of 
    Right (Just msg') -> BC.unpack msg'
    Right Nothing -> ""
    Left _            -> error "Failed to retrieve eval header"

setEvalHeader :: Connection -> ASSheetId -> ASLanguage -> String -> IO ()
setEvalHeader conn sid lang xp = runRedis conn (set (makeEvalHeaderKey sid lang) (BC.pack xp)) >> return ()
