module AS.Dispatch.Core where

-- AlphaSheets and base
import Prelude
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.List  as L
import Data.Maybe (fromJust, isNothing, catMaybes)
import Text.ParserCombinators.Parsec
import Control.Applicative
import Data.Text as T (unpack,pack)
import Control.Exception.Base

import AS.Types.Cell
import AS.Types.Locations
import AS.Types.CellProps
import AS.Types.Messages
import AS.Types.Network
import AS.Types.DB
import AS.Types.Eval

import AS.Dispatch.Expanding        as DE
import qualified AS.Eval.Core       as EC (evaluateLanguage)
import qualified AS.DB.API          as DB
import qualified AS.DB.Transaction  as DT
import qualified AS.DB.Util         as DU
import AS.Util                      as U
import AS.Eval.Middleware           as EM
import AS.Eval.Endware              as EE
import qualified AS.DB.Graph        as G
import AS.Logging
import AS.Parsing.Common
import AS.Parsing.Show hiding (first)
import AS.Parsing.Read

import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson hiding (Success)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Network.WebSockets as WS
import Database.Redis (Connection)

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------------------------------
-- Debugging

testDispatch :: MVar ServerState -> ASLanguage -> Coord -> String -> IO ASServerMessage
testDispatch state lang crd str = runDispatchCycle state [Cell (Index sid crd) (Expression str Python) NoValue emptyProps] (sid, uid)
  where 
    sid = T.pack "sheetid"
    uid = T.pack "userid"

----------------------------------------------------------------------------------------------------------------------------------------------
-- Exposed functions / regular eval route

-- assumes all evaled cells are in the same sheet
-- the only information we're really passed in from the cells is the locations and the expressions of
-- the cells getting evaluated. We pull the rest from the DB. 
runDispatchCycle :: MVar ServerState -> [ASCell] -> CommitSource -> IO ASServerMessage
runDispatchCycle state cs src = do
  printObj "cs" cs
  roots <- EM.evalMiddleware cs
  conn <- dbConn <$> readMVar state
  errOrCells <- runEitherT $ do
    printWithTimeT $ "about to start dispatch"
    ctxAfterDispatch <- dispatch conn roots emptyContext False
    printWithTimeT $ "finished dispatch"
    finalizedCells <- EE.evalEndware state (addedCells ctxAfterDispatch) src roots ctxAfterDispatch
    let ctx = ctxAfterDispatch { addedCells = finalizedCells }
    broadcastCells <- DT.updateDBFromEvalContext conn src ctx
    return broadcastCells
  case errOrCells of 
    Left _ -> G.recompute -- #needsrefactor. Overkill. But recording all cells that might have changed is a PITA. (Alex 11/20)
    _      -> return ()
  return $ makeUpdateMessage errOrCells

-- takes an old context, inserts the new values necessary for this round of eval, and evals using the new context.
-- this seems conceptually better than letting each round of dispatch produce a new context, 
-- and hoping we union them in the right order. This means that at any point in time, there is a *single*
-- EvalContext in existence, and we just continue writing to it every time dispatch is called recursively. 
dispatch :: Connection -> [ASCell] -> EvalContext -> Bool -> EitherTExec EvalContext
dispatch conn roots oldContext shouldGetProperDescs = do
  printObjT "STARTING DISPATCH CYCLE WITH CELLS" roots
  -- For all the original cells, add the edges in the graph DB; parse + setRelations
  rootsDepSets   <- DB.setCellsAncestors roots
  printObjT "Set cell ancestors" rootsDepSets
  descLocs       <- getEvalLocs conn roots shouldGetProperDescs
  printObjT "Got eval locations" descLocs
  -- Turn the descLocs into Cells, but the roots are already given as cells, so no DB actions needed there
  cellsToEval    <- getCellsToEval conn descLocs roots
  printObjT "Got cells to evaluate" cellsToEval
  ancLocs        <- G.getImmediateAncestors $ indicesToGraphReadInput descLocs
  printObjT "Got ancestor locs" ancLocs
  -- The initial lookup cache has the ancestors of all descendants
  initContext <- getInitialContext conn ancLocs oldContext
  printObjT "Created initial context" initContext
  printWithTimeT "Starting eval chain"
  evalChainException conn initContext cellsToEval -- start with current cells, then go through descendants


----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval building blocks

-- | Return the locations of cells to evaluate. (Just the descendants of all the cells passed in
-- to the dispatch cycle, plus all the volatile cells -- the ones that always re-evaluate.)
-- List of locations guaranteed to be topologically sorted by dependency. (So the cells with no
-- dependencies are evaluated first.)
-- TODO: throw exceptions for permissions/locking
-- Currently, the graph returns only indices as descendants
getEvalLocs :: Connection -> [ASCell] -> Bool -> EitherTExec [ASIndex]
getEvalLocs conn origCells shouldGetProperDescs = do
  let locs = map cellLocation origCells
  vLocs <- lift $ DB.getVolatileLocs conn -- Accounts for volatile cells being reevaluated each time
  if shouldGetProperDescs
    then G.getProperDescendantsIndices $ (locs ++ vLocs)
    else (G.getDescendantsIndices $ (locs ++ vLocs)) >>= (\idxs -> printObjT "DESC INDICES: " idxs >> return idxs)

-- | Given a set of locations to eval, return the corresponding set of cells to perform
-- the evaluations in (which includes info about tags, language, and expression string).
-- Distinguishes between new cells to evaluate (the ones passed into runDispatchCycle)
-- and old cells already in the database, which all reference the new cells. For the new 
-- cells, just evaluate them as-is (we already have the cells); for old cells, pull them from the database.
getCellsToEval :: Connection -> [ASIndex] -> [ASCell] -> EitherTExec [ASCell]
getCellsToEval conn locs origCells = do
  let locCellMap = M.fromList $ map (\c -> (cellLocation c, c)) origCells
  mCells <- lift $ DB.getCells locs
  lift $ mapM (\(loc, mCell) -> if loc `M.member` locCellMap
      then return $ locCellMap M.! loc
      else return $ fromJust mCell) (zip locs mCells) 
  -- if the mCell is Nothing and loc is not a part of the locCellMap, then something messed up. 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Maps 

---- Takes in a value map, and change it from (Index -> ASValue) to (Index -> Formatted ASValue), which
---- is necessary for Excel evals.  Since this is only relevant for Excel evaluations, I'd ideally have
---- this in Eval/Core and only call it for Excel evals, but that would require O(n) calls to the DB, 
---- which is prohibitively expensive. 
---- TODO refactor s.t. we construct a formatted map from the beginning
--formatValsMap :: ValMap -> IO FormattedValMap
--formatValsMap mp = do
--  let mp' = M.toList mp  
--      locs = map fst mp'
--      vals = map snd mp' 
--  cells <- DB.getPossiblyBlankCells locs 
--  let formats = map getCellFormatType cells
--      vals' = map (\(v, ft) -> Formatted v ft) (zip vals formats)
--  return $ M.fromList $ zip locs vals'

-- see the comments above dispatch to see why an old evalContext is passed in.
getInitialContext :: Connection -> [ASReference] -> EvalContext -> EitherTExec EvalContext
getInitialContext conn ancs oldContext = do
   -- ::RITESH::  hella unsafe
   indices <- concat <$> mapM DB.refToIndices ancs
   cells <- lift $ DB.getPossiblyBlankCells indices
   let oldMap = contextMap oldContext
       newContext = oldContext { contextMap = insertMultiple oldMap indices cells }
   return newContext

retrieveValue :: Maybe CompositeCell -> CompositeValue
retrieveValue c = case c of
  Just (Single cell) -> CellValue $ cellValue cell
  Just (Fat fcell) -> DE.recomposeCompositeValue fcell
  Nothing -> CellValue NoValue

formatCell :: Maybe FormatType -> ASCell -> ASCell
formatCell mf c = case mf of 
  Nothing -> c
  Just f -> c { cellProps = setProp (ValueFormat f) (cellProps c) }

----------------------------------------------------------------------------------------------------------------------------------------------
-- EvalChain

-- | Evaluates a list of cells, in serial order, updating the reference/value map with each
-- cell that's updated. The cells passed in are guaranteed to be topologically sorted, i.e.,
-- if a cell references an ancestor, that ancestor is guaranteed to already have been
-- added in the map.
evalChainException :: Connection  -> EvalContext -> [ASCell] -> EitherTExec EvalContext
evalChainException conn ctx cells = 
  let whenCaught e = do
        printObj "Runtime exception caught" (e :: SomeException)
        return $ Left RuntimeEvalException
  in do
    result <- liftIO $ catch (runEitherT $ evalChain conn ctx cells) whenCaught
    hoistEither result

evalChain :: Connection -> EvalContext -> [ASCell] -> EitherTExec EvalContext
evalChain _ ctx [] = printWithTimeT "empty evalchain" >> return ctx
evalChain conn ctx (c@(Cell loc xp val ps):cs) = do
  printWithTimeT $ "running eval chain with cells: " ++ (show (c:cs))
  let getEvalResult = EC.evaluateLanguage conn loc ctx
  cvf <- case xp of 
    Expression _ _         ->  getEvalResult xp
    Coupled str lang _ key -> if (DU.isFatCellHead c)
      then getEvalResult $ Expression str lang
      else return $ Formatted (CellValue val) (formatType <$> getProp ValueFormatProp ps) -- temporary patch -- eval needs to get restructured
  newContext <- contextInsert conn c cvf ctx
  evalChain conn newContext cs 

contextInsert :: Connection -> ASCell -> Formatted CompositeValue -> EvalContext -> EitherTExec EvalContext
contextInsert conn c@(Cell idx xp _ ps) (Formatted cv f) (EvalContext mp cells removedDescriptors addedDescriptors) = do 
  printWithTimeT $ "running context insert with cell " ++ (show c)
  let maybeFatCell = DE.decomposeCompositeValue c cv
  -- The newly created cell(s) may have caused decouplings, which we're going to deal with. First, we get the decoupled keys 
  decoupledKeys <- lift $ case maybeFatCell of
    Nothing -> DU.getFatCellIntersections conn (Left [idx])
    Just (FatCell _ descriptor) -> DU.getFatCellIntersections conn (Right [descriptorKey descriptor])
  -- We then get the range descriptors of those keys from the DB. These are the descriptors that need 
  -- to be removed. There's currently no batched getDescriptors, so we mapM. 
  newlyRemovedDescriptors <- lift $ map fromJust <$> mapM (DB.getRangeDescriptor conn) decoupledKeys
  -- The locations we have to decouple can be constructed from the range keys, which have info about the
  -- head and size of the range
  let decoupledLocs = concat $ map DU.rangeKeyToIndices decoupledKeys
  -- Given the locs, we get the cells that we have to decouple from the DB and then change their expressions
  -- to be decoupled (by using the value of the cell)
  decoupledCells <- lift $ (map DU.decoupleCell) <$> (catMaybes <$> DB.getCells decoupledLocs)
  -- We want to update all of the decoupled cells in our mini-spreadsheet map
  let mpWithDecoupledCells = insertMultiple mp decoupledLocs decoupledCells
  -- Add our decoupled descriptors to the current list of removedDescriptors
  let finalRemovedDescriptors = removedDescriptors ++ newlyRemovedDescriptors
  -- Wrap up by modifying the context
  case maybeFatCell of
    Nothing -> do
      let CellValue v = cv
      -- Modify the props of our current cell to reflect any format
      let newCell = formatCell f $ Cell idx xp v ps
      -- The final updated map also has our newly evaluated cell in it, with updated cell value and props
      let finalMp = M.insert idx newCell mpWithDecoupledCells
      -- Add our decoupled descriptors to the current list of removedDescriptors, and note that
      -- a simple CellValue cannot add any descriptors
      let finalCells = mergeCells [newCell] $ mergeCells decoupledCells cells
          resultContext = EvalContext finalMp finalCells finalRemovedDescriptors addedDescriptors
      -- now, propagate the descandants of the decoupled cells
      dispatch conn decoupledCells resultContext True
    Just (FatCell cs descriptor) -> do 
      -- Modify the props of our current cells to reflect any format
      let newCells = map (formatCell f) cs
      -- The final updated map also has our newly evaluated cells in it, with updated cell value and props
      let finalMp = insertMultiple mpWithDecoupledCells (map cellLocation newCells) newCells
          finalAddedDescriptors = descriptor:addedDescriptors
          finalCells = mergeCells cs $ mergeCells decoupledCells cells
          finalContext = EvalContext finalMp finalCells finalRemovedDescriptors finalAddedDescriptors
      -- propagate the descandants of the decoupled cells
      finalContext' <- dispatch conn decoupledCells finalContext True
      -- propagate the descendants of the expanded cells (except for the list head)
      dispatch conn cs finalContext' True


  --case (DE.decomposeCompositeValue cv) of 
  --  Nothing -> do
  --    let CellValue v = cv
  --    -- The newly created cell may have caused decouplings, which we're going to deal with.  
  --    -- First, we get the decoupled keys 
  --    decoupledKeys <- DU.getFatCellIntersections conn (Left [idx]) 
  --    -- We then get the range descriptors of those keys from the DB. These are the descriptors that need 
  --    -- to be removed. There's currently no batched getDescriptors, so we mapM. 
  --    newlyRemovedDescriptors <- map fromJust <$> mapM (DB.getRangeDescriptor conn) decoupledKeys
  --    -- The locations we have to decouple can be constructed from the range keys, which have info about the
  --    -- head and size of the range
  --    let decoupledLocs = concat $ map DU.rangeKeyToIndices decoupledKeys
  --    -- Given the locs, we get the cells that we have to decouple from the DB and then change their expressions
  --    -- to be decoupled (by using the value of the cell)
  --    decoupledCells <- map DU.decoupleCell <$> DB.getCells decoupledLocs
  --    -- We want to update all of the decoupled cells in our mini-spreadsheet map
  --    let mpWithDecoupledCells = insertMultiple mp decoupledLocs decoupledCells
  --    -- Modify the props of our current cell to reflect any format
  --    let newCell = formatCell f $ Cell idx xp v ps
  --    -- The final updated map also has our newly evaluated cell in it, with updated cell value and props
  --    let finalMp = M.insert idx newCell mpWithDecoupledCells
  --    -- Add our decoupled descriptors to the current list of removedDescriptors, and note that
  --    -- a simple CellValue cannot add any descriptors
  --    let finalRemovedDescriptors = removedDescriptors ++ newlyRemovedDescriptors
  --        finalCells = mergeCells decoupledCells (newCell:cells)
  --        resultContext = EvalContext newMp finalCells finalRemovedDescriptors addedDescriptors
  --    -- now, propagate the descandants of the decoupled cells
  --    dispatch conn [decoupledCells] resultContext True

  --  Just (FatCell cs descriptor) -> do
  --    -- The newly created cell may have caused decouplings, which we're going to deal with.  
  --    -- First, we get the decoupled keys 
  --    decoupledKeys <- DU.getFatCellIntersections conn (Right [descriptorKey descriptor]) 
  --    -- We then get the range descriptors of those keys from the DB. These are the descriptors that need 
  --    -- to be removed. There's currently no batched getDescriptors, so we mapM. 
  --    newlyRemovedDescriptors <- map fromJust <$> mapM (DB.getRangeDescriptor conn) decoupledKeys
  --    -- The locations we have to decouple can be constructed from the range keys, which have info about the
  --    -- head and size of the range
  --    let decoupledLocs = concat $ map DU.rangeKeyToIndices decoupledKeys
  --    -- Given the locs, we get the cells that we have to decouple from the DB and then change their expressions
  --    -- to be decoupled (by using the value of the cell)
  --    decoupledCells <- map DU.decoupleCell <$> DB.getCells decoupledLocs
  --    -- We want to update all of the decoupled cells in our mini-spreadsheet map
  --    let mpWithDecoupledCells = insertMultiple mp decoupledLocs decoupledCells
  --    -- Modify the props of our current cells to reflect any format
  --    let newCells = map (formatCell f) cs
  --    -- The final updated map also has our newly evaluated cells in it, with updated cell value and props
  --    let finalMp = insertMultiple mpWithDecoupledCells (map cellLocation newCells) newCells
  --        finalRemovedDescriptors = removedDescriptors ++ newlyRemovedDescriptors
  --        finalAddedDescriptors = descriptor:addedDescriptors
  --        finalCells = mergeCells cs $ mergeCells decoupledCells cells
  --        finalContext = EvalContext finalMp finalCells finalRemovedDescriptors finalAddedDescriptors
  --    -- propagate the descandants of the decoupled cells
  --    finalContext' <- dispatch conn [decoupledCells] finalContext True
  --    -- propagate the descendants of the expanded cells (except for the list head)
  --    dispatch conn cs finalContext' True


---- | evalChain' works in two parts. First, it goes through the list of cells passed in and
---- evaluates them. Along the way, new fat cells (created as part of a list) are created
---- that may need to get re-evaluated. These get rnecorded in the fourth argument (type [FatCell]),
---- and the heads of these lists get recorded in the fifth.
----
---- When we finish evaluating the original list of cells, we go through the newly created fat cells
---- and re-evaluate those. We are NOT just running eval on this list of cells directly,
---- because we don't need to re-evaluate the individual cells in the list, ONLY their descendants.
---- We also need to check for circular dependencies, which is why the pastListHeads are passed in.
---- 
---- Note that we can get into arbitrarily many cycles of alternating between normal evalChains, 
---- then evalChains where we're processing fatCells, normal evalChains, fatCell processing evalChains, 
---- etc. Note also that pastFatCellHeads accumulates across all of these, and doesn't just reflect the
---- heads of the cells passed in from the latest eval. 
----
---- finally, in the second case of evalChain', we might evaluate a Coupled expression which belong to a 
---- list head. If so, we evaluate it, but "delete" the cells in the previous list by passing the argument
---- "deletedLocs", the 6th argument. This is how shrink lists work. In DT.writeTransaction, we turn these 
---- locations into actual blank cells and delete them. 
---- 
---- #needsrefactor there's probably a more Haskell way of doing this with a state monad or something.
--evalChain' :: Connection -> EvalContext -> [ASCell]  -> [ASIndex] -> [ASIndex] -> EitherTExec ([ASCell], [FatCell], [ASIndex], FormattedValMap)
--evalChain' _ ctx [] [] _ _ = return ([], [], [], valMap)

--evalChain' conn valuesMap [] fatCells pastFatCellHeads _ = 
--  -- get expanded cells from fat cells
--  let unwrap (FatCell fcells _) = fcells
--      cells                     = concat $ map unwrap fatCells
--      isFatCellHeadLoc loc      = loc `elem` pastFatCellHeads
--      nonHeadLocs               = filter (not . isFatCellHeadLoc) $ map cellLocation cells
--      isNotInMap loc            = loc `M.notMember` valuesMap
--  in do
--    printWithTimeT $ "pastCellHeads " ++ (show pastFatCellHeads)
--    printWithTimeT $ "nonHeadLocs " ++ (show nonHeadLocs)
--    -- If we've overwritten old cells with list cells, we remove their dependencies from the graph
--    -- database. 
--    DB.removeAncestorsAt nonHeadLocs
--    -- We only need to deal with proper descendants, because the starting locs can't possibly 
--    -- be in the value map and don't need to be re-evaluated. 
--    nonHeadDescs <- G.getDescendantsIndices $ nonHeadLocs
--    printWithTimeT $ "nonHeadDesc " ++ (show nonHeadDescs)
--    -- Check for circular dependencies. IF a circular dependency exists, it necessarily has to
--    -- involve one of the list heads, since the cells created as part of a list depend only
--    -- on the head. So we go through the descendants of the current list cells (sans the previous
--    -- list heads), and if those contain any of the previous list heads we know there's a cycle.
--    let checkCircular loc = if (isFatCellHeadLoc loc) then (left $ CircularDepError loc) else (return ())
--    mapM_ checkCircular $ nonHeadDescs
--    -- We can now remove the descendants that are already in the map, because they've already 
--    -- been evaluated. (We also MUST remove them, because some of the descendants might not have
--    -- existed before the eval, and if we include them among nextLocs we'll get an error when we
--    -- try to pull out the cell at that location in getCellsToEval. We couldn't remove them when checking
--    -- for circular dependencies though.)
--    let nextLocs = filter isNotInMap nonHeadDescs
--    -- #needsrefactor it seems like a large chunk of code here mirrors that in evalChain... should probably DRY
--    ancLocs <- G.getImmediateAncestors $ indicesToGraphReadInput nextLocs
--    formattedNewMap <- lift $ formatValsMap =<< getValuesMap conn ancLocs
--    cells' <- getCellsToEval conn nextLocs [] -- the origCells are the list cells, which got filtered out of nextLocs
--    evalChain' conn (M.union valuesMap formattedNewMap) cells' [] pastFatCellHeads []

--evalChain' conn valuesMap (c@(Cell loc xp oldVal ps):cs) fatCells fatCellHeads pastDeletedLocs = do
--  (cvf@(Formatted cv f), deletedLocs) <- case xp of 
--    Expression _ _ -> (,) <$> evalResult <*> return []
--      where evalResult = EC.evaluateLanguage (locSheetId loc) (cellLocation c) valuesMap xp
--    -- we might receive a non-list-head coupled cell to evaluate during copy/paste, row insertion, etc.  
--    -- if we receive a coupled cell to evaluate, and it's the head of the list, we should evaluate, as long as we get rid of cruft.
--    -- where "get rid of cruft" = get rid of all the non-head cells first, which is deletedCells
--    Coupled str lang _ key -> if (DU.isFatCellHead c)
--      then (,) <$> evalResult <*> return decoupleLocs
--      else (,) <$> (return $ Formatted (CellValue oldVal) (formatType <$> getProp ValueFormatProp ps)) <*> return [] -- temporary patch -- eval needs to get restructured
--        where evalResult = EC.evaluateLanguage (locSheetId loc) (cellLocation c) valuesMap xp'
--              xp' = Expression str lang
--              decoupleLocs = DU.rangeKeyToIndices key

--  let maybeFatCell              = DE.decomposeCompositeValue c cv
--      addCell (Cell l _ v _) mp = M.insert l (Formatted (CellValue v) f) mp
--      newValuesMap              = case maybeFatCell of
--              Nothing -> if (M.member ptr valuesMap) 
--                then M.insert ptr cvf idxInserted
--                else idxInserted
--                where 
--                  ptr = indexToPointer loc
--                  idxInserted = M.insert loc cvf valuesMap
--      -- ^ when updating a location in the map, check if there are Pointer references to the same location.
--      -- if so, update them too
--              Just (FatCell expandedCells _) -> foldr addCell valuesMap expandedCells
--      -- ^ adds all the cells in cellsList to the reference map
--      (fatCells', fatCellHeads') = case maybeFatCell of
--                Nothing -> (fatCells, fatCellHeads)
--                Just f  -> (f:fatCells, loc:fatCellHeads)
--  (restCells, restFatCells, restDeletedLocs, restValuesMap) <- evalChain' conn newValuesMap cs fatCells' fatCellHeads' []
--  -- TODO investigate strictness here
--  let (CellValue v) = cv
--      newCell       = formatCell f (Cell loc xp v ps)
--      (resultCells, resultFatCells) = case maybeFatCell of
--          Nothing      -> (newCell:restCells, restFatCells)
--          Just fatCell -> (restCells, fatCell:restFatCells)
--      resultDeletedLocs = pastDeletedLocs ++ deletedLocs ++ restDeletedLocs

--  right (resultCells, resultFatCells, resultDeletedLocs, restValuesMap)