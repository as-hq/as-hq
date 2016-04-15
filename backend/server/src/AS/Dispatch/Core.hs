module AS.Dispatch.Core where

-- AlphaSheets and base
import AS.Prelude
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.List  as L
import Data.Maybe (isNothing, catMaybes)
import Text.ParserCombinators.Parsec
import Control.Applicative
import Data.Text as T (unpack,pack)
import Control.Exception.Base

import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Commits
import AS.Types.DB
import AS.Types.Graph
import AS.Types.Eval
import AS.Types.Formats
import AS.Types.Locations
import AS.Types.Messages
import AS.Types.Network
import AS.Types.Updates
import AS.Logging

import AS.Dispatch.Expanding        as DE
import qualified AS.Eval.Core       as EC 
import qualified AS.DB.API          as DB
import qualified AS.DB.Eval         as DE
import qualified AS.DB.Transaction  as DT
import qualified AS.DB.Internal     as DI
import AS.Util                      as U
import AS.Eval.Middleware           as EM
import AS.Eval.Endware              as EE
import qualified AS.DB.Graph        as G
import AS.Logging
import AS.Parsing.Common
import AS.Parsing.Show hiding (first)
import AS.Parsing.Read

import Control.Monad
import Control.Lens
import Control.Applicative (liftA2)
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

-- an impure eval transform 
type EvalTransform = EvalContext -> EitherTExec EvalContext
-- an impure eval transform with extra info about the transform
type EvalTransformWithInfo a = EvalContext -> EitherTExec (EvalContext, a)
-- a pure function modifying context
type PureEvalTransform = EvalContext -> EvalContext


----------------------------------------------------------------------------------------------------------------------------------------------
-- Debugging

-- testDispatch :: State -> ASLanguage -> Coord -> String -> IO ClientMessage
-- testDispatch state lang crd str = runDispatchCycle state [cell] DescendantsWithParent src id
--   where 
--     cell = Cell (Index sid crd) (Expression str Python) NoValue emptyProps
--     src = CommitSource sid uid
--     sid = T.pack "sheetid"
--     uid = T.pack "userid"

----------------------------------------------------------------------------------------------------------------------------------------------
-- Exposed functions / regular eval route

-- dispatch invariants:
-- 

-- dispatch questions unanswered:
-- should we try to detect ambiguity when executing one of several possible eval paths? (e.g. where one path would cause a circ dep, and another would be fine)

-- assumes all evaled cells are in the same sheet
-- the only information we're really passed in from the cells is the locations and the expressions of
-- the cells getting evaluated. We pull the rest from the DB.
-- #lenses
runDispatchCycle :: ServerState -> MessageId -> [ASCell] -> DescendantsSetting -> CommitSource -> UpdateTransform -> IO (Either ASExecError SheetUpdate)
runDispatchCycle state mid cs descSetting src updateTransform = do
  roots <- EM.evalMiddleware cs
  time <- getASTime 
  let conn = state^.dbConn
  rangeDescriptorsInSheet <- DB.getRangeDescriptorsInSheet conn $ srcSheetId src
  errOrUpdate <- runEitherT $ do
    let initialEvalMap = M.fromList $ zip (mapCellLocation roots) roots
        initialContext = EvalContext initialEvalMap rangeDescriptorsInSheet $ sheetUpdateFromCommit $ emptyCommitWithTime time
    -- you must insert the roots into the initial context, because getCells.ToEval will give you cells to evaluate that
    -- are only in the context or in the DB (in that order of prececdence). IF neither, you won't get anything. 
    -- this maintains the invariant that context always contains the most up-to-date, complete information. 
    ctxAfterDispatch <- dispatch state mid roots initialContext descSetting
    let transformedCtx = ctxAfterDispatch & updateAfterEval %~ updateTransform -- #lenses

    finalCells <- EE.evalEndware state mid src transformedCtx evalChain

    let ctx = transformedCtx & updateAfterEval.cellUpdates.newVals .~ finalCells

    DT.updateDBWithContext conn src ctx
    return $ ctx^.updateAfterEval
  -- DAG may have changed during dispatch; if not committed, reset it. Note that it is *not* 
  -- fully correct to just recompute what's in the current sheet. If we start in sheet A, and 
  -- we updated something in sheet B that caused it to expand and eat up a cell that previously had
  -- a dependency in it, that dependency doesn't get restored by recomputing sheet A. For now though
  -- we aren't worrying about this. 
  -- #incomplete (--Alex 3/1/2016)
  either (const $ G.recomputeSheetDAG conn (srcSheetId src)) (const $ return ()) errOrUpdate
  return errOrUpdate

-- takes an old context, inserts the new values necessary for this round of eval, and evals using the new context.
-- this seems conceptually better than letting each round of dispatch produce a new context, 
-- and hoping we union them in the right order. This means that at any point in time, there is a *single*
-- EvalContext in existence, and we just continue writing to it every time dispatch is called recursively. 
dispatch :: ServerState -> MessageId -> [ASCell] -> EvalContext -> DescendantsSetting -> EitherTExec EvalContext
dispatch state _ [] context _ = return context
dispatch state mid roots oldContext descSetting = do
  let conn = state^.dbConn
  -- For all the original cells, add the edges in the graph DB; parse + setRelations
  G.setCellsAncestors roots
  descLocs       <- getEvalLocs state roots descSetting
  -- Turn the descLocs into Cells, but the roots are already given as cells, so no DB actions needed there
  cellsToEval    <- getCellsToEval conn oldContext descLocs
  ancLocs        <- G.getImmediateAncestors $ indicesToAncestryRequestInput descLocs
  -- The initial lookup cache has the ancestors of all descendants
  modifiedContext <- getModifiedContext conn ancLocs oldContext
  evalChainWithException state mid cellsToEval modifiedContext -- start with current cells, then go through descendants


----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval building blocks

-- | Return the locations of cells to evaluate. (Just the descendants of all the cells passed in
-- to the dispatch cycle, plus all the volatile cells -- the ones that always re-evaluate.)
-- List of locations guaranteed to be topologically sorted by dependency. (So the cells with no
-- dependencies are evaluated first.)
-- TODO: throw exceptions for permissions/locking
-- Currently, the graph returns only indices as descendants
getEvalLocs :: ServerState -> [ASCell] -> DescendantsSetting -> EitherTExec [ASIndex]
getEvalLocs state origCells descSetting = getter locs
  where
    locs = mapCellLocation origCells
    getter = case descSetting of 
      ProperDescendants -> G.getProperDescendantsIndices 
      DescendantsWithParent -> G.getDescendantsIndices 


-- | Given a set of locations to eval, return the corresponding set of cells to perform
-- the evaluations in. We look up locs in the DB, but give precedence to EvalContext (if a cell is in the context, we use that instead, 
-- as it is the most up-to-date info we have). 
-- this function is order-preserving
getCellsToEval :: Connection -> EvalContext -> [ASIndex] -> EitherTExec [ASCell]
getCellsToEval conn ctx locs = possiblyThrowException =<< (lift $ DE.getCellsWithContext conn ctx locs)
  where 
    possiblyThrowException mcells = if any isNothing mcells
      then left $ DBNothingException missingLocs
      else return $ map $fromJust mcells
        where missingLocs = map fst $ filter (isNothing . snd) $ zip locs mcells 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Maps 

-- see the comments above dispatch to see why an old evalContext is passed in.
getModifiedContext :: Connection -> [ASReference] -> EvalTransform
getModifiedContext conn ancs oldContext = do
   ancIndices <-  lift $ concat <$> mapM (DE.refToIndicesWithContextBeforeEval conn oldContext) ancs 
   let nonContextAncIndices = filter (not . (flip M.member (oldContext^.virtualCellsMap))) ancIndices
   cells <- lift $ DB.getPossiblyBlankCells conn nonContextAncIndices
   let oldMap = oldContext^.virtualCellsMap
       newContext = oldContext & virtualCellsMap .~ insertMultiple oldMap nonContextAncIndices cells
   return newContext

retrieveValue :: Maybe CompositeCell -> CompositeValue
retrieveValue c = case c of
  Just (Single cell) -> CellValue $ cell^.cellValue
  Just (Fat fcell) -> DE.recomposeCompositeValue fcell
  Nothing -> CellValue NoValue

formatCell :: Maybe Format -> ASCell -> ASCell
formatCell mf c = maybe c ((c &) . over cellProps . setProp . ValueFormat) mf

----------------------------------------------------------------------------------------------------------------------------------------------
-- EvalChain

-- A wrapper around evalChain which catches errors
evalChainWithException :: ServerState -> MessageId -> [ASCell] -> EvalTransform
evalChainWithException state mid cells ctx = 
  let whenCaught e = do
        let src = CommitSource  { srcSheetId = ($head cells)^.cellLocation.locSheetId
                                , srcUserId = "unknown_user"
                                }
        putsError src $ "Runtime exception caught" ++ show (e :: SomeException)
        return $ Left RuntimeEvalException
  in do
    result <- liftIO $ catchAny (runEitherT $ evalChain state mid cells ctx) whenCaught
    hoistEither result

-- If a cell input to evalChain is a coupled cell that's not a fat-cell-head, then we NEVER evaluate it. In addition, if there's a normal cell
-- in the list of cells that has a coupled counterpart in the context, we don't evaluate that cell either. The reason is that we want to give 
-- fat cells overwrite power (this is a UX feature that we're adding, so it requires special casing).  Example: A1 = 1, A2 = A1, A1 = range(10) 
-- should keep A1 coupled.  It shouldn't eval the range, then eval A2, which would decouple the range. In our function, A2 wouldn't even 
-- be evalled even if it were in the queue as a normal cell.

shouldEvalCell :: EvalContext -> ASCell -> Bool
shouldEvalCell ctx c = isEvaluable c && not hasCoupledCounterpartInMap
  where
    hasCoupledCounterpartInMap = case (c^.cellLocation) `M.lookup` (ctx^.virtualCellsMap) of
      Nothing -> False
      Just c' -> isCoupled c' && (not $ isFatCellHead c')

evalChain :: ServerState -> MessageId -> [ASCell] -> EvalTransform
evalChain _ _ [] ctx = return ctx
evalChain state mid (c@(Cell loc xp val ps rk disp):cs) ctx = 
  if shouldEvalCell ctx c
    then do 
      evalResult <- EC.evaluateLanguage state mid loc ctx xp evalChainWithoutPropagation
      newContext <- contextInsert state mid c evalResult ctx
      evalChain state mid cs newContext
    else evalChain state mid cs ctx

-- Evaluate a bunch of cells without calling dispatch again, just update the context
-- Used in sampling
evalChainWithoutPropagation :: ServerState -> MessageId -> [ASCell] -> EvalTransform
evalChainWithoutPropagation _ _ [] ctx = return ctx
evalChainWithoutPropagation state mid (c@(Cell loc xp val ps rk disp):cs) ctx = 
  if shouldEvalCell ctx c
    then do 
      evalResult <- EC.evaluateLanguage state mid loc ctx xp evalChainWithoutPropagation
      newContext <- contextInsertWithoutPropagation state c evalResult ctx
      evalChainWithoutPropagation state mid cs newContext
    else evalChainWithoutPropagation state mid cs ctx

  ----------------------------------------------------------------------------------------------------------------------------------------------
  -- Context modification

-- #needsrefactor the maybe logic sholdn't be dealt with here
-- Helper function that removes a maybe descriptor from a context and returns the updated context. #lens
removeMaybeDescriptorFromContext :: Maybe RangeDescriptor -> PureEvalTransform
removeMaybeDescriptorFromContext descriptor ctx =
  ctx & (updateAfterEval . descriptorUpdates) %~ descriptorTransform
    where 
      descriptorTransform = case descriptor of
        Nothing -> id
        Just d -> (removeKey . key) d

-- Helper function that removes multiple descriptors from the ddiff of the context. #lens
removeMultipleDescriptorsFromContext :: [RangeDescriptor] -> PureEvalTransform
removeMultipleDescriptorsFromContext descriptors ctx =
  ctx & (updateAfterEval . descriptorUpdates) %~ descriptorTransform
    where
      descriptorTransform = \ddiff ->  L.foldl' (flip removeKey) ddiff (map key descriptors)

-- Helper function  that adds a descriptor to the ddiff of a context
addValueToContext :: RangeDescriptor -> PureEvalTransform
addValueToContext descriptor ctx =
  ctx & (updateAfterEval . descriptorUpdates) %~ (addValue descriptor)


-- Deal with a possible shrink list. The ASCell passed in is a descendant during dispatch. 
-- We're not doing anything if it isn't coupled. Note that all descendants of a cell
-- must be fat cell heads or normal cells. If it's a fat cell head, we're going to blank out all indices
-- in the corresponding objectfor now and propagate those changes. 
-- The composite value returned by the previous eval chain will then write over this object in contextInsert 
-- Example: if range(10) becomes range(5) because of an upstream change, blank out A1:A10. 
-- The cv in the previous evalChain has the coupled range(5) cells and will replace the virtualCellsMap. 
-- Essentially, this transform exists because you should delete an object before overwriting 
-- Returns the new context and the blanked out indices, if any
delPrevFatCellFromContext :: Connection -> ASCell -> EvalTransformWithInfo (Maybe [ASIndex])
delPrevFatCellFromContext conn c@(Cell idx xp _ _ rk _) ctx = case rk of 
  Nothing  ->  return (ctx, Nothing)
  Just key -> if (isFatCellHead c)
    then do 
      let indices = finiteRangeKeyToIndices key
          blankCells = map blankCellAt indices
          descriptor = virtualRangeDescriptorAt ctx key
      -- Remove the descriptor from context, and then add the blank cells to the list of added cells. This must be done
      -- so that the blanked out cells make it into the commit (ie so that the context is correct up to now). If anything
      -- replaces these blanked out cells, they will be merged in by a future contextInsert.
      let ctx' = addCellsToContext blankCells $ removeMaybeDescriptorFromContext descriptor ctx
      return (ctx', Just indices)
    else left WillNotEvaluate

-- This transform conceptually does the following
-- (1) generates the range descriptors to remove, if any, from the new cell/fatcell produced during eval, 
-- and removes these descriptors from context
-- (2) gets the cells decoupled by the new cell/fatcell produced during eval, if any, and merges them into the
-- addedCells of the context as well as the virtualCellsMap
-- (3) add a descriptor to the context if a fatcell is produced
-- Note that the index passed in is the old location, and the maybe fatcell is the compositeValue from eval
-- Returns the new context and the decoupled cells caused by the possible fat cell
addCurFatCellToContext :: Connection -> ASIndex -> Maybe FatCell -> EvalTransformWithInfo [ASCell]
addCurFatCellToContext conn idx maybeFatCell ctx = do 
  -- The newly created cell(s) may have caused decouplings, which we're going to deal with. 
  -- First, we get a possible fatcell created, from which we get the newly removed descriptors, if any
  -- We need to remove the ones that intersect our newly produced cell/fatcell (they're decoupled now)
  let newlybeforeVals = case maybeFatCell of
                          Nothing -> getFatCellIntersections ctx (Left [idx])
                          Just (FatCell _ descriptor) -> getFatCellIntersections ctx (Right [descriptorKey descriptor])
  -- The locations we have to decouple can be constructed from the range keys
  -- Then, given the locs, we get the cells that we have to decouple from the DB and then change their expressions
  -- to be decoupled (by using the value of the cell)
  let decoupledLocs = concatMap (finiteRangeKeyToIndices . descriptorKey) newlybeforeVals
  decoupledCells <- lift $ ((map DI.toDecoupled) . catMaybes) <$> DE.getCellsWithContext conn ctx decoupledLocs
  let ctx' = removeMultipleDescriptorsFromContext newlybeforeVals $ addCellsToContext decoupledCells ctx
  let ctx'' = case maybeFatCell of
                Nothing -> ctx'
                Just (FatCell _ descriptor) -> addValueToContext descriptor ctx'
  return (ctx'', decoupledCells)


-- NEXT: do checks on rangekeys being in both added and removed.
-- the cell passed in is the old cell (we insert the old cell + new eval'ed value into the context at the end of this function, 
-- after all side effects due to insertion have been handled)
-- if you get here, your cell has already been evaluated, and we're from now on going to call dispatch with ProperDescendants set.
--  #lens
contextInsert :: ServerState -> MessageId -> ASCell -> Formatted EvalResult -> EvalTransform
contextInsert state mid c@(Cell idx xp _ ps _ _) (Formatted result f) ctx = do  
  let conn  = state^.dbConn
      cv    = result^.resultValue
      disp  = result^.resultDisplay 
  let maybeFatCell = DE.decomposeCompositeValue c cv
  -- Get the new cells created by eval, with formatting as well, and add them into the context
  let newCellsFromEval = case maybeFatCell of
                          Nothing -> [formatCell f $ Cell idx xp v ps Nothing disp] -- no rangeKey
                            where 
                              CellValue v = cv
                          Just (FatCell cs _) -> map (formatCell f . set cellDisplay disp) cs
  -- Account for overwriting a fat cell (blanking out), adding a new fat cell (decoupling), and adding eval cells
  (ctxWithBlanks, blankedIndices) <- delPrevFatCellFromContext conn c ctx 
  (ctxWithDecoupledCells, decoupledCells) <- addCurFatCellToContext conn idx maybeFatCell ctxWithBlanks
  let ctxWithEvalCells = addCellsToContext newCellsFromEval ctxWithDecoupledCells
  -- Wrap up and dispatch
  case maybeFatCell of
    Nothing -> do 
      dispatch state mid decoupledCells ctxWithEvalCells ProperDescendants
    Just (FatCell cs descriptor) -> do 
      let blankCells = case blankedIndices of 
                        Nothing -> []
                        Just inds -> map (flip $valAt $ ctxWithEvalCells^.virtualCellsMap) inds
          dispatchCells = mergeCells newCellsFromEval $ mergeCells decoupledCells blankCells
      dispatch state mid dispatchCells ctxWithEvalCells ProperDescendants
      -- propagate the descendants of the expanded cells (except for the list head)
      -- you don't set relations of the newly expanded cells, because those relations do not exist. 
      -- Only the $head of the list has an ancestor at this point.
      -- first, check if we blanked out anything as a result of possiblyDeletePreviousFatCell. 
      -- if so, look up the new cells from the map. 
      -- e.g. if range(5) -> range(2), possiblyDeletePrevious... will return indices A1...A5, 
      -- and we're going to need to run dispatch on all of those.
      -- so this particular dispatch merges the context transforms (1) expanded cells, 
      -- (2) blanked cells due to fat cell deletion


contextInsertWithoutPropagation :: ServerState -> ASCell -> Formatted EvalResult -> EvalTransform
contextInsertWithoutPropagation state c@(Cell idx xp _ ps _ _) (Formatted result f) ctx = do  
  let conn  = state^.dbConn
      cv    = result^.resultValue
      disp  = result^.resultDisplay 
  let maybeFatCell = DE.decomposeCompositeValue c cv
  let newCellsFromEval = case maybeFatCell of
                          Nothing -> [formatCell f $ Cell idx xp v ps Nothing disp] -- no rangeKey
                            where 
                              CellValue v = cv
                          Just (FatCell cs _) -> map (formatCell f . set cellDisplay disp) cs
  (ctxWithBlanks, blankedIndices) <- delPrevFatCellFromContext conn c ctx 
  (ctxWithDecoupledCells, decoupledCells) <- addCurFatCellToContext conn idx maybeFatCell ctxWithBlanks
  let ctxWithEvalCells = addCellsToContext newCellsFromEval ctxWithDecoupledCells
  return ctxWithEvalCells
