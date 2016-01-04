module AS.DB.Eval where

import Prelude

import AS.Types.Cell
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.DB
import qualified AS.Dispatch.Expanding as DE

import qualified AS.DB.API as DB
import AS.Logging

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

import Database.Redis hiding (decode)
import Control.Monad
import Control.Lens hiding (set)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------
-- Cell lookups

-- looks up cells in the given context, then in the database, in that precedence order
-- this function is order-preserving
getCellsWithContext :: Connection -> EvalContext -> [ASIndex] -> IO [Maybe ASCell]
getCellsWithContext conn (EvalContext { virtualCellsMap = mp }) locs = map replaceWithContext <$> zip locs <$> DB.getCells conn locs
  where
    replaceWithContext (l, c) = maybe c Just $ M.lookup l mp

----------------------------------------------------------------------------------------------------------------------
-- Reference conversions/lookups

-- used by lookUpRef
-- #mustrefactor IO CompositeValue should be EitherTExec CompositeValue
referenceToCompositeValue :: Connection -> EvalContext -> ASReference -> IO CompositeValue
referenceToCompositeValue _ (EvalContext { virtualCellsMap = mp }) (IndexRef i) = return $ CellValue . view cellValue $ mp M.! i 
referenceToCompositeValue conn ctx (PointerRef p) = do 
  let idx = pointerIndex p
  let mp = virtualCellsMap ctx
  let cell = mp M.! idx
  case cell^.cellRangeKey of 
    Nothing -> error "Pointer to normal expression!" -- #mustrefactor why isn't this left IndexOfPointerNonExistant
    Just rKey -> do 
      case virtualRangeDescriptorAt ctx rKey of
        Nothing -> error "Couldn't find range descriptor of coupled expression!"
        Just descriptor -> do 
          let indices = rangeKeyToIndices rKey
              cells  = map ((virtualCellsMap ctx) M.!) indices
              fatCell = FatCell cells descriptor
          printObj "REF TO COMPOSITE DESCRIPTOR: " descriptor
          return $ DE.recomposeCompositeValue fatCell
referenceToCompositeValue conn ctx (RangeRef r) = return . Expanding . VList . M $ vals
  where
    indices = rangeToIndicesRowMajor2D r
    indToVal ind = view cellValue $ (virtualCellsMap ctx) M.! ind
    vals    = map (map indToVal) indices

refToIndices :: Connection -> ASReference -> EitherTExec [ASIndex]
refToIndices conn (IndexRef i) = return [i]
refToIndices conn (RangeRef r) = return $ rangeToIndices r
refToIndices conn (PointerRef p) = do
  let index = pointerIndex p 
  cell <- lift $ DB.getCell conn index 
  case cell of
    Nothing -> left IndexOfPointerNonExistant
    Just cell' -> case cell'^.cellRangeKey of
        Nothing -> left PointerToNormalCell
        Just rKey -> return $ rangeKeyToIndices rKey

-- #needsrefactor DRY this up

-- converts ref to indices using the evalContext, then the DB, in that order.
-- because our evalContext might contain information the DB doesn't (e.g. decoupling)
-- so in the pointer case, we need to check the evalContext first for changes that might have happened during eval
refToIndicesWithContextDuringEval :: Connection -> EvalContext -> ASReference -> EitherTExec [ASIndex]
refToIndicesWithContextDuringEval conn _ (IndexRef i) = return [i]
refToIndicesWithContextDuringEval conn _ (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextDuringEval conn (EvalContext { virtualCellsMap = mp }) (PointerRef p) = do -- #record
  let index = pointerIndex p
  case (M.lookup index mp) of
    Just c -> maybe (left PointerToNormalCell) (return . rangeKeyToIndices) $ c^.cellRangeKey
    Nothing -> do
      cell <- lift $ DB.getCell conn index 
      case cell of
        Nothing -> left IndexOfPointerNonExistant
        Just cell' -> maybe (left PointerToNormalCell) (return . rangeKeyToIndices) $ cell'^.cellRangeKey

-- This is the function we use to convert ref to indices for updating the map PRIOR TO eval. There are some cases where we don't flip a shit. 
-- For example, if the map currently has A1 as a normal expression, and we have @A1 somewhere downstream, we won't flip a shit, and instead expect that
-- by the time the pointer is evalled, A1 will have a coupled expression due to toposort. We flip a shit if it's not the case then. 
refToIndicesWithContextBeforeEval :: Connection -> EvalContext -> ASReference -> IO [ASIndex]
refToIndicesWithContextBeforeEval conn _ (IndexRef i) = return [i]
refToIndicesWithContextBeforeEval conn _ (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextBeforeEval conn (EvalContext { virtualCellsMap = mp }) (PointerRef p) = do -- #record
  let index = pointerIndex p
  case (M.lookup index mp) of 
    Just c -> return $ maybe [] rangeKeyToIndices $ c^.cellRangeKey
    Nothing -> do
      cell <- DB.getCell conn index 
      case cell of
        Nothing -> return []
        Just cell' -> return $ maybe [] rangeKeyToIndices $ cell'^.cellRangeKey

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
