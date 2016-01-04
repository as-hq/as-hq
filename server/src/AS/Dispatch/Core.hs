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
import AS.Types.Commits
import AS.Types.Eval
import AS.Types.DB
import AS.Types.Updates

import AS.Dispatch.Expanding        as DE
import AS.DB.Eval
import qualified AS.Eval.Core       as EC (evaluateLanguage)
import qualified AS.DB.API          as DB
import qualified AS.DB.Transaction  as DT
import qualified AS.DB.Expanding    as DX
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

-- testDispatch :: MVar ServerState -> ASLanguage -> Coord -> String -> IO ClientMessage
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
runDispatchCycle :: MVar ServerState -> [ASCell] -> DescendantsSetting -> CommitSource -> UpdateTransform -> IO (Either ASExecError SheetUpdate)
runDispatchCycle mstate cs descSetting src updateTransform = do
  roots <- EM.evalMiddleware cs
  state <- readMVar mstate
  time <- getASTime 
  let conn = state^.dbConn
      graphAddress = state^.appSettings.graphDbAddress
  rangeDescriptorsInSheet <- DB.getRangeDescriptorsInSheet conn $ srcSheetId src
  errOrCommit <- runEitherT $ do
    printWithTimeT $ "about to start dispatch"
    let initialEvalMap = M.fromList $ zip (mapCellLocation cs) cs
        initialContext = EvalContext initialEvalMap rangeDescriptorsInSheet $ sheetUpdateFromCommit $ emptyCommitWithTime time
    -- you must insert the roots into the initial context, because getCells.ToEval will give you cells to evaluate that
    -- are only in the context or in the DB (in that order of prececdence). IF neither, you won't get anything. 
    -- this maintains the invariant that context always contains the most up-to-date, complete information. 
    ctxAfterDispatch <- dispatch state roots initialContext descSetting
    printWithTimeT "finished dispatch"
    let transformedCtx = ctxAfterDispatch { updateAfterEval = updateTransform (updateAfterEval ctxAfterDispatch) } -- #lenses
    finalCells <- EE.evalEndware mstate src transformedCtx
    let ctx = transformedCtx { updateAfterEval = (updateAfterEval transformedCtx) { cellUpdates = (cellUpdates . updateAfterEval $ transformedCtx) { newVals = finalCells } } } -- #lens
    DT.updateDBWithContext state src ctx
  either (const $ G.recompute graphAddress conn) (const $ return ()) errOrCommit 
  return . fmap sheetUpdateFromCommit $ errOrCommit

-- takes an old context, inserts the new values necessary for this round of eval, and evals using the new context.
-- this seems conceptually better than letting each round of dispatch produce a new context, 
-- and hoping we union them in the right order. This means that at any point in time, there is a *single*
-- EvalContext in existence, and we just continue writing to it every time dispatch is called recursively. 
dispatch :: ServerState -> [ASCell] -> EvalContext -> DescendantsSetting -> EitherTExec EvalContext
dispatch state [] context _ = printWithTimeT "empty dispatch" >> return context
dispatch state roots oldContext descSetting = do
  let conn = state^.dbConn
      graphAddress = state^.appSettings^.graphDbAddress
  printObjT "STARTING DISPATCH CYCLE WITH CELLS" roots
  printWithTimeT $ "Settings: Descendants: " ++ (show descSetting)
  -- For all the original cells, add the edges in the graph DB; parse + setRelations
  G.setCellsAncestors graphAddress roots
  descLocs       <- getEvalLocs state roots descSetting
  printObjT "Got eval locations" descLocs
  -- Turn the descLocs into Cells, but the roots are already given as cells, so no DB actions needed there
  cellsToEval    <- getCellsToEval conn oldContext descLocs
  printObjT "Got cells to evaluate" cellsToEval
  ancLocs        <- G.getImmediateAncestors graphAddress $ indicesToGraphReadInput descLocs
  printObjT "Got ancestor locs" ancLocs
  -- The initial lookup cache has the ancestors of all descendants
  modifiedContext <- getModifiedContext conn ancLocs oldContext
  printWithTimeT "Created initial context"  -- ++ (show modifiedContext)
  printWithTimeT "Starting eval chain"
  evalChainWithException state cellsToEval modifiedContext -- start with current cells, then go through descendants


----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval building blocks

-- | Return the locations of cells to evaluate. (Just the descendants of all the cells passed in
-- to the dispatch cycle, plus all the volatile cells -- the ones that always re-evaluate.)
-- List of locations guaranteed to be topologically sorted by dependency. (So the cells with no
-- dependencies are evaluated first.)
-- TODO: throw exceptions for permissions/locking
-- Currently, the graph returns only indices as descendants
getEvalLocs :: ServerState -> [ASCell] -> DescendantsSetting -> EitherTExec [ASIndex]
getEvalLocs state origCells descSetting = do
  let locs = mapCellLocation origCells
  vLocs <- lift $ DB.getVolatileLocs (state^.dbConn) -- Accounts for volatile cells being reevaluated each time
  let graphAddress = state^.appSettings.graphDbAddress
  case descSetting of 
    ProperDescendants -> G.getProperDescendantsIndices graphAddress $ (locs ++ vLocs)
    DescendantsWithParent -> G.getDescendantsIndices graphAddress $ (locs ++ vLocs)

-- | Given a set of locations to eval, return the corresponding set of cells to perform
-- the evaluations in. We look up locs in the DB, but give precedence to EvalContext (if a cell is in the context, we use that instead, 
-- as it is the most up-to-date info we have). 
-- this function is order-preserving
getCellsToEval :: Connection -> EvalContext -> [ASIndex] -> EitherTExec [ASCell]
getCellsToEval conn ctx locs = possiblyThrowException =<< (lift $ getCellsWithContext conn ctx locs)
  where 
    possiblyThrowException mcells = if any isNothing mcells
      then left $ DBNothingException missingLocs
      else return $ map fromJust mcells
        where missingLocs = map fst $ filter (\(_,mc) -> isNothing mc) $ zip locs mcells 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Maps 

-- see the comments above dispatch to see why an old evalContext is passed in.
getModifiedContext :: Connection -> [ASReference] -> EvalTransform
getModifiedContext conn ancs oldContext = do
   ancIndices <-  lift $ concat <$> mapM (refToIndicesWithContextBeforeEval conn oldContext) ancs 
   let nonContextAncIndices = filter (not . (flip M.member (virtualCellsMap oldContext))) ancIndices
   cells <- lift $ DB.getPossiblyBlankCells conn nonContextAncIndices
   let oldMap = virtualCellsMap oldContext
       newContext = oldContext { virtualCellsMap = insertMultiple oldMap nonContextAncIndices cells }
   return newContext

retrieveValue :: Maybe CompositeCell -> CompositeValue
retrieveValue c = case c of
  Just (Single cell) -> CellValue $ cell^.cellValue
  Just (Fat fcell) -> DE.recomposeCompositeValue fcell
  Nothing -> CellValue NoValue

formatCell :: Maybe FormatType -> ASCell -> ASCell
formatCell mf c = maybe c ((c &) . over cellProps . setProp . ValueFormat) mf

----------------------------------------------------------------------------------------------------------------------------------------------
-- EvalChain

-- A wrapper around evalChain which catches errors
evalChainWithException :: ServerState -> [ASCell] -> EvalTransform
evalChainWithException state cells ctx = 
  let whenCaught e = do
        printObj "Runtime exception caught" (e :: SomeException)
        return $ Left RuntimeEvalException
  in do
    result <- liftIO $ catch (runEitherT $ evalChain state cells ctx) whenCaught
    hoistEither result

-- If a cell input to evalChain is a coupled cell that's not a fat-cell-head, then we NEVER evaluate it. In addition, if there's a normal cell
-- in the list of cells that has a coupled counterpart in the context, we don't evaluate that cell either. The reason is that we want to give 
-- fat cells overwrite power (this is a UX feature that we're adding, so it requires special casing).  Example: A1 = 1, A2 = A1, A1 = range(10) 
-- should keep A1 coupled.  It shouldn't eval the range, then eval A2, which would decouple the range. In our function, A2 wouldn't even 
-- be evalled even if it were in the queue as a normal cell. 
evalChain :: ServerState -> [ASCell] -> EvalTransform
evalChain state cells ctx = evalChain' state cells' ctx
  where 
    hasCoupledCounterpartInMap c = case (c^.cellLocation) `M.lookup` (virtualCellsMap ctx) of
      Nothing -> False
      Just c' -> (isCoupled c') && (not $ isFatCellHead c')
    cells' = filter (liftA2 (&&) isEvaluable (not . hasCoupledCounterpartInMap)) cells

evalChain' :: ServerState -> [ASCell] -> EvalTransform
evalChain' _ [] ctx = printWithTimeT "empty evalchain" >> return ctx
evalChain' state (c@(Cell loc xp val ps rk):cs) ctx = do
  printWithTimeT $ "running eval chain with cells: " ++ (show (c:cs))
  let getEvalResult expression = EC.evaluateLanguage state loc ctx expression 
  cvf <- case rk of 
    Nothing  ->  getEvalResult xp
    Just key -> if (isFatCellHead c)
      then getEvalResult xp 
      else return $ Formatted (CellValue val) (formatType <$> getProp ValueFormatProp ps) 
  newContext <- contextInsert state c cvf ctx
  evalChain state cs newContext

  ----------------------------------------------------------------------------------------------------------------------------------------------
  -- Context modification

-- #needsrefactor the maybe logic sholdn't be dealt with here
-- Helper function that removes a maybe descriptor from a context and returns the updated context. 
removeMaybeDescriptorFromContext :: Maybe RangeDescriptor -> PureEvalTransform
removeMaybeDescriptorFromContext descriptor ctx = ctx { updateAfterEval = (updateAfterEval ctx) { descriptorUpdates = ddiff' } } -- #lens
  where 
    ddiff = descriptorUpdates $ updateAfterEval ctx
    ddiff' = case descriptor of
      Nothing -> ddiff
      Just d -> removeKey ddiff (key d)

-- Helper function that removes multiple descriptors from the ddiff of the context. 
removeMultipleDescriptorsFromContext :: [RangeDescriptor] -> PureEvalTransform
removeMultipleDescriptorsFromContext descriptors ctx = ctx { updateAfterEval = (updateAfterEval ctx) { descriptorUpdates = ddiffWithbeforeVals } } -- #lens
  where
    ddiff = descriptorUpdates $ updateAfterEval ctx
    ddiffWithbeforeVals = L.foldl' removeKey ddiff (map key descriptors)

-- Helper function  that adds a descriptor to the ddiff of a context
addValueToContext :: RangeDescriptor -> PureEvalTransform
addValueToContext descriptor ctx = ctx { updateAfterEval = (updateAfterEval ctx) { descriptorUpdates = ddiff' } } -- #lens
  where
    ddiff  = descriptorUpdates $ updateAfterEval ctx
    ddiff' = addValue ddiff descriptor

-- Helper function that adds cells to a context, by merging them to addedCells and the map (with priority).
addCellsToContext :: [ASCell] -> PureEvalTransform
addCellsToContext cells ctx = ctx { virtualCellsMap = newMap, updateAfterEval = (updateAfterEval ctx) { cellUpdates = Update newAddedCells [] } } -- #lens
  where
    newAddedCells = mergeCells cells (newCellsInContext ctx)
    newMap   = insertMultiple (virtualCellsMap ctx) (mapCellLocation cells) cells


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
delPrevFatCellFromContext conn c@(Cell idx xp _ _ rk) ctx = case rk of 
  Nothing  ->  return (ctx, Nothing)
  Just key -> if (isFatCellHead c)
    then do 
      let indices = rangeKeyToIndices key
          blankCells = map blankCellAt indices
          descriptor = virtualRangeDescriptorAt ctx key
      printWithTimeT $ "REMOVED DESCRIPTOR IN DELETE PREVIOUS FAT CELL: " ++ (show descriptor)
      printWithTimeT $ "BLANK CELLS: " ++ (show blankCells)          
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
  printObjT "GOT DECOUPLED DESCRIPTORS" newlybeforeVals
  -- The locations we have to decouple can be constructed from the range keys
  -- Then, given the locs, we get the cells that we have to decouple from the DB and then change their expressions
  -- to be decoupled (by using the value of the cell)
  let decoupledLocs = concatMap (rangeKeyToIndices . descriptorKey) newlybeforeVals
  decoupledCells <- lift $ ((map DI.toDecoupled) . catMaybes) <$> getCellsWithContext conn ctx decoupledLocs
  let ctx' = removeMultipleDescriptorsFromContext newlybeforeVals $ addCellsToContext decoupledCells ctx
  let ctx'' = case maybeFatCell of
                Nothing -> ctx'
                Just (FatCell _ descriptor) -> addValueToContext descriptor ctx'
  return (ctx'', decoupledCells)


  -- NEXT: do checks on rangekeys being in both added and removed.
  -- the cell passed in is the old cell (we insert the old cell + new eval'ed value into the context at the end of this function, 
  -- after all side effects due to insertion have been handled)
  -- if you get here, your cell has already been evaluated, and we're from now on going to call dispatch with ProperDescendants set.
contextInsert :: ServerState -> ASCell -> Formatted CompositeValue -> EvalTransform
contextInsert state c@(Cell idx xp _ ps rk) (Formatted cv f) ctx = do  -- #lens
  let conn = state^.dbConn
  printWithTimeT $ "running context insert with old cell " ++ (show c)
  printWithTimeT $ "the context insert has value " ++ (show cv)
  let maybeFatCell = DE.decomposeCompositeValue c cv
  -- Get the new cells created by eval, with formatting as well, and add them into the context
  let newCellsFromEval = case maybeFatCell of
                          Nothing -> [formatCell f $ Cell idx xp v ps Nothing]
                            where 
                              CellValue v = cv
                          Just (FatCell cs _) -> map (formatCell f) cs
  -- Account for overwriting a fat cell (blanking out), adding a new fat cell (decoupling), and adding eval cells
  (ctxWithBlanks, blankedIndices) <- delPrevFatCellFromContext conn c ctx 
  (ctxWithDecoupledCells, decoupledCells) <- addCurFatCellToContext conn idx maybeFatCell ctxWithBlanks
  let ctxWithEvalCells = addCellsToContext newCellsFromEval ctxWithDecoupledCells
  printWithTimeT $ "DECOUPLED CELLS" ++ (show decoupledCells)
  -- Wrap up and dispatch
  case maybeFatCell of
    Nothing -> do 
      printWithTimeT "\nrunning decouple transform"
      dispatch state decoupledCells ctxWithEvalCells ProperDescendants
    Just (FatCell cs descriptor) -> do 
      printWithTimeT "\nrunning expanded cells transform"
      let blankCells = case blankedIndices of 
                        Nothing -> []
                        Just inds -> map ((M.!) (virtualCellsMap ctxWithEvalCells)) inds
          dispatchCells = mergeCells newCellsFromEval $ mergeCells decoupledCells blankCells
      dispatch state dispatchCells ctxWithEvalCells ProperDescendants
      -- ^ propagate the descendants of the expanded cells (except for the list head)
      -- you don't set relations of the newly expanded cells, because those relations do not exist. 
      -- Only the head of the list has an ancestor at this point.
      -- first, check if we blanked out anything as a result of possiblyDeletePreviousFatCell. 
      -- if so, look up the new cells from the map. 
      -- e.g. if range(5) -> range(2), possiblyDeletePrevious... will return indices A1...A5, 
      -- and we're going to need to run dispatch on all of those.
      -- so this particular dispatch merges the context transforms (1) expanded cells, 
      -- (2) blanked cells due to fat cell deletion
