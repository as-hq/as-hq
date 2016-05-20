module AS.DB.Eval where

import AS.Prelude

import AS.Types.Cell
import AS.Types.Errors
import AS.Types.Eval as E
import AS.Types.DB
import AS.Types.Graph
import AS.Types.Network
import AS.Types.Messages (MessageId)
import AS.Types.Excel hiding (dbConn)
import AS.Types.User
import AS.Util

import AS.DB.Graph as G
import AS.Eval.ColRangeHelpers
import qualified AS.Dispatch.Expanding as DE

import qualified AS.DB.API as DB
import qualified AS.DB.Users as DB
import AS.Logging

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.List as L

import Database.Redis hiding (decode)
import Data.List
import Data.Maybe (catMaybes)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Control.Concurrent (myThreadId)

type EvalChainFunc = MessageContext -> [ASCell] -> EvalContext -> EitherTExec EvalContext

-- looks up cells in the given context, then in the database, in that precedence order
-- this function is order-preserving
getCellsWithContext :: Connection -> EvalContext -> [ASIndex] -> IO [Maybe ASCell]
getCellsWithContext conn ctx locs = map replaceWithContextCell <$> zip locs <$> DB.getCells conn locs
  where
    replaceWithContextCell (l, c) = maybe c Just $ M.lookup l (ctx^.virtualCellsMap)

getPossiblyBlankCellsWithContext :: Connection -> EvalContext -> [ASIndex] -> IO [ASCell]
getPossiblyBlankCellsWithContext conn ctx locs = map replaceWithContextCell <$> zip locs <$> DB.getPossiblyBlankCells conn locs
  where
    replaceWithContextCell (l, c) = maybe c id $ M.lookup l (ctx^.virtualCellsMap)

----------------------------------------------------------------------------------------------------------------------
-- Reference value lookup

-- outputs an exRange equivalent to the input of the first ExRange, with the first coord <= second coord
-- #lenses
-- TODO: Introduce PossiblyInfiniteRange as a type: is just correct, makes colRange functions  consequence of functions on ranges.
-- Note: Code duplication between this and orientRange.

-- used by lookUpRef
--  #mustrefactor IO CompositeValue should be EitherTExec CompositeValue
referenceToCompositeValue ::  MessageContext ->
                              EvalContext -> 
                              ASReference -> 
                              EvalChainFunc -> 
                              IO CompositeValue
referenceToCompositeValue _ evalctx (IndexRef i) _ = return $ CellValue . view cellValue $ $valAt i $ evalctx^.virtualCellsMap
referenceToCompositeValue msgctx evalctx (PointerRef p) _ = do 
  let idx = pointerIndex p
      mp = evalctx^.virtualCellsMap
      cell = $valAt idx mp
      conn = msgctx^.dbConnection
  case cell^.cellRangeKey of 
    Nothing -> $error "Pointer to normal expression!" 
    Just rKey ->
      case virtualRangeDescriptorAt evalctx rKey of
        Nothing -> $error "Couldn't find range descriptor of coupled expression!"
        Just descriptor -> do 
          let indices = finiteRangeKeyToIndices rKey
              cells  = map ((evalctx^.virtualCellsMap) M.!) indices
              fatCell = FatCell cells descriptor
          return $ DE.recomposeCompositeValue fatCell
-- #NeedsRefactor: This is not the best way to do it: takes column cells, converts to indices, then converts back to values.....
referenceToCompositeValue _ evalctx (RangeRef r) _
  | isFiniteRange r = do
    let finiteRangeIndToVal ind = view cellValue $ $valAt ind $ evalctx^.virtualCellsMap
        rangeVals = map (map finiteRangeIndToVal) indices
    return . Expanding . VList . M $ rangeVals
  | isColRange r = do
      -- The only case where the index is not in the virtualCellsMap is when the
      -- current dispatch created new cells in the bottom of a column whose
      -- colRange is being evaluated.
    let colRangeIndToVal ind =
          if M.member ind (evalctx^.virtualCellsMap)
             then view cellValue $ $valAt ind $ evalctx^.virtualCellsMap
             else NoValue
        colRangeVals = map (map colRangeIndToVal) indices
    return $ Expanding . VList . M $ colRangeVals
      where indices = rangeWithContextToIndicesRowMajor2D evalctx r
referenceToCompositeValue msgctx evalctx (TemplateRef (SampleExpr n idx)) f = $fromRight <$> (runEitherT $ do 
      -- Get all ancestors
      let conn = msgctx^.dbConnection
      ancRefs <- G.getAllAncestors $ indicesToAncestryRequestInput [idx]
      ancInds <- concat <$> mapM (refToIndicesWithContextDuringEval conn evalctx) ancRefs
      ancCells <- lift $ getPossiblyBlankCellsWithContext conn evalctx ancInds
      let evalctxWithAncs = addCellsToContext ancCells evalctx
      -- After adding ancestors to context, evaluate n times
      samples <- replicateM n $ evaluateNode msgctx evalctxWithAncs idx ancCells f
      return $ Expanding $ VList $ A samples)

-- Only used in conditional formatting.
refToIndicesInCondFormatting :: Connection -> ASReference -> EitherTExec [ASIndex]
refToIndicesInCondFormatting conn ref = refToIndicesWithContextDuringEval conn emptyContext ref

-- #needsrefactor DRY this up
-- converts ref to indices using the evalContext, then the DB, in that order.

-- because our evalContext might contain information the DB doesn't (e.g. decoupling)
-- so in the pointer case, we need to check the evalContext first for changes that might have happened during eval
refToIndicesWithContextDuringEval :: Connection -> EvalContext -> ASReference -> EitherTExec [ASIndex]
refToIndicesWithContextDuringEval conn _ (IndexRef i) = return [i]
refToIndicesWithContextDuringEval conn ctx (RangeRef r) =
  lift $ rangeWithDBAndContextToIndices conn ctx r
refToIndicesWithContextDuringEval conn ctx (PointerRef p) = do -- #record
  let index = pointerIndex p
  case (M.lookup index $ ctx^.virtualCellsMap) of
    Just c -> maybe (left PointerToNormalCell) (return . finiteRangeKeyToIndices) $ c^.cellRangeKey
    Nothing -> do
      cell <- lift $ DB.getCell conn index 
      case cell of
        Nothing -> left IndexOfPointerNonExistant
        Just cell' -> maybe (left PointerToNormalCell) (return . finiteRangeKeyToIndices) $ cell'^.cellRangeKey
refToIndicesWithContextDuringEval _ _ (TemplateRef _) = return []

-- This is the function we use to convert ref to indices for updating the map PRIOR TO eval. There are some cases where we don't flip a shit. 
-- For example, if the map currently has A1 as a normal expression, and we have @A1 somewhere downstream, we won't flip a shit, and instead expect that
-- by the time the pointer is evalled, A1 will have a coupled expression due to toposort. We flip a shit if it's not the case then. 
--  #record after PointerRef
-- #RoomForImprovement: Timchu. Assumed that Pointers point to finite ranges.
refToIndicesWithContextBeforeEval :: Connection -> EvalContext -> ASReference -> IO [ASIndex]
refToIndicesWithContextBeforeEval conn _ (IndexRef i) = return [i]
refToIndicesWithContextBeforeEval conn ctx (RangeRef r) = rangeWithDBAndContextToIndices conn ctx r
refToIndicesWithContextBeforeEval conn ctx (PointerRef p) = do 
  let index = pointerIndex p
  case (M.lookup index $ ctx^.virtualCellsMap) of 
    Just c -> return $ maybe [] finiteRangeKeyToIndices $ c^.cellRangeKey
    Nothing -> do
      cell <- DB.getCell conn index 
      case cell of
        Nothing -> return []
        Just cell' -> return $ maybe [] finiteRangeKeyToIndices $ cell'^.cellRangeKey


-- Evaluate a node by running an evaluation function and extracting the answer from the context at the end. 
-- Assumes all ancestors are already in the context.
evaluateNode :: MessageContext -> EvalContext -> ASIndex -> [ASCell] -> EvalChainFunc -> EitherTExec ASValue
evaluateNode msgctx evalctx idx ancestors f = do
  evalctx' <- f msgctx ancestors evalctx
  return . view cellValue . $valAt idx $ evalctx'^.virtualCellsMap
