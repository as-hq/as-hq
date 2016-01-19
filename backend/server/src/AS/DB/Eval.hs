module AS.DB.Eval where

import Prelude()
import AS.Prelude

import AS.Types.Cell
import AS.Types.Errors
import AS.Types.Eval as E
import AS.Types.DB
import AS.Util
import AS.Eval.ColRangeHelpers
import qualified AS.Dispatch.Expanding as DE

import qualified AS.DB.API as DB
import AS.Logging

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.List as L

import Database.Redis hiding (decode)
import Data.List
import Control.Monad
import Control.Lens hiding (set)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------
-- Cell lookups

-- looks up cells in the given context, then in the database, in that precedence order
-- this function is order-preserving
getCellsWithContext :: Connection -> EvalContext -> [ASIndex] -> IO [Maybe ASCell]
getCellsWithContext conn EvalContext { virtualCellsMap = mp } locs = map replaceWithContext <$> zip locs <$> DB.getCells conn locs
  where
    replaceWithContext (l, c) = maybe c Just $ M.lookup l mp

----------------------------------------------------------------------------------------------------------------------
-- Reference conversions/lookups

-- used by lookUpRef
--  #mustrefactor IO CompositeValue should be EitherTExec CompositeValue
--  #mustrefactor why isn't this left IndexOfPointerNonExistant
referenceToCompositeValue :: Connection -> EvalContext -> ASReference -> IO CompositeValue
referenceToCompositeValue _ (EvalContext { virtualCellsMap = mp }) (IndexRef i) = return $ CellValue . view cellValue $ mp M.! i 
referenceToCompositeValue conn ctx (PointerRef p) = do 
  let idx = pointerIndex p
  let mp = virtualCellsMap ctx
  let cell = mp M.! idx
  case cell^.cellRangeKey of 
    Nothing -> $error "Pointer to normal expression!" 
    Just rKey -> do 
      case virtualRangeDescriptorAt ctx rKey of
        Nothing -> $error "Couldn't find range descriptor of coupled expression!"
        Just descriptor -> do 
          let indices = rangeKeyToIndices rKey
              cells  = map ((virtualCellsMap ctx) M.!) indices
              fatCell = FatCell cells descriptor
          printObj "REF TO COMPOSITE DESCRIPTOR: " descriptor
          return $ DE.recomposeCompositeValue fatCell
-- TODO: This is not the best way to do it: takes column cells, converts to indices, then converts back to values.....
referenceToCompositeValue conn ctx (ColRangeRef cr) = return $ Expanding . VList . M $ vals
  where
    indices = colRangeWithContextToIndicesRowMajor2D ctx cr
    -- The only case where the index is not in the virtualCellsMap is when the
    -- current dispatch created new cells in the bottom of a column whose
    -- colRange is being evaluated.
    indToVal ind = case M.member ind (virtualCellsMap ctx) of
                        True -> view cellValue $ (virtualCellsMap ctx) M.! ind
                        False -> NoValue
    vals    = map (map indToVal) indices
referenceToCompositeValue conn ctx (RangeRef r) = return . Expanding . VList . M $ vals
  where
    indices = rangeToIndicesRowMajor2D r
    indToVal ind = view cellValue $ (virtualCellsMap ctx) M.! ind
    vals    = map (map indToVal) indices

-- Only used in conditional formatting.
-- TODO: timchu, 12/25/15. not sure if return $ colRange ... or  lift colRangeWithDB..
refToIndices :: Connection -> ASReference -> EitherTExec [ASIndex]
refToIndices conn (IndexRef i) = return [i]
refToIndices conn (ColRangeRef cr) = lift $ colRangeWithDBAndContextToIndices conn emptyContext cr
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
-- TODO: timchu. Only used in shortCircuitDuringEval. This could be renamed to be more clear.
--  #record after PointerRef
refToIndicesWithContextDuringEval :: Connection -> EvalContext -> ASReference -> EitherTExec [ASIndex]
refToIndicesWithContextDuringEval conn _ (IndexRef i) = return [i]
refToIndicesWithContextDuringEval conn _ (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextDuringEval conn ctx (ColRangeRef cr) = lift $ colRangeWithDBAndContextToIndices conn ctx cr
refToIndicesWithContextDuringEval conn (EvalContext { virtualCellsMap = mp }) (PointerRef p) = do 
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
--  #record after PointerRef
refToIndicesWithContextBeforeEval :: Connection -> EvalContext -> ASReference -> IO [ASIndex]
refToIndicesWithContextBeforeEval conn _ (IndexRef i) = return [i]
refToIndicesWithContextBeforeEval conn _ (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextBeforeEval conn ctx (ColRangeRef r) = colRangeWithDBAndContextToIndices conn ctx r
refToIndicesWithContextBeforeEval conn (EvalContext { virtualCellsMap = mp }) (PointerRef p) = do 
  let index = pointerIndex p
  case (M.lookup index mp) of 
    Just c -> return $ maybe [] rangeKeyToIndices $ c^.cellRangeKey
    Nothing -> do
      cell <- DB.getCell conn index 
      case cell of
        Nothing -> return []
        Just cell' -> return $ maybe [] rangeKeyToIndices $ cell'^.cellRangeKey
