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
testDispatch state lang crd str = runDispatchCycle state [Cell (Index sid crd) (Expression str Python) NoValue emptyProps] DescendantsWithParent (sid, uid)
  where
    sid = T.pack "sheetid"
    uid = T.pack "userid"

----------------------------------------------------------------------------------------------------------------------------------------------
-- Exposed functions / regular eval route

-- dispatch invariants:
-- 

-- dispatch questions unanswered:
-- should we try to detect ambiguity when executing one of several possible eval paths? (e.g. where one path would cause a circ dep, and another would be fine)

-- assumes all evaled cells are in the same sheet
-- the only information we're really passed in from the cells is the locations and the expressions of
-- the cells getting evaluated. We pull the rest from the DB. 

-- ||| BEGIN TIM CHU EDITS
--

evalContextToCommit :: EvalContext -> IO (ASCommit, Bool)
evalContextToCommit (EvalContext mp cells ddiff) = do
  mbcells <- DB.getCells (map cellLocation cells)
  time <- getASTime
  let cdiff   = CellDiff { beforeCells = (catMaybes mbcells), afterCells = cells}
      commit  = Commit cdiff ddiff time
      rd      = removedDescriptors ddiff
      didDecouple = any isDecouplePair $ zip mbcells cells
      -- determines whether to send a decouple message.
      -- we send a decouple message if there are any decoupled, *visible* cells remaining on the spreadsheet.
      -- i.e. deleting an entire range will not cause a decouple message, but deleting part of it will because 
      -- decoupled cells remain visible on the sheet.
      isDecouplePair (mbcell, acell) = case mbcell of 
        Nothing -> False
        Just bcell -> (isCoupled bcell) && (not $ isCoupled acell) && (not $ isBlank acell)
  printWithTime $ "DEALING WITH DECOUPLING OF DESCRIPTORS " ++ (show rd)
  return (commit, didDecouple)

pushTempCommit :: Connection -> CommitSource -> ASCommit -> IO()
pushTempCommit conn src commit = do
  DT.setTempCommit conn commit src

pushCommitOrTempCommit :: Connection -> CommitSource -> Bool -> ASCommit -> IO()
pushCommitOrTempCommit conn src shouldDecouple commit =
  if shouldDecouple
    then pushTempCommit conn src commit
    else DT.updateDBAfterEval conn src commit

runDispatchCycle :: MVar ServerState -> [ASCell] -> DescendantsSetting -> CommitSource -> IO ASServerMessage
runDispatchCycle state cs descSetting src = do
  liftIO $ putStrLn $ "run dispatch cycle with cells: " ++ (show cs) 
  roots <- EM.evalMiddleware cs
  conn <- dbConn <$> readMVar state
  -- UPDATED TO ERR OR COMMIT
  -- BEGIN BOILERPLATE
  errOrCommit <- runEitherT $ do
    printWithTimeT $ "about to start dispatch"
    let initialEvalMap = M.fromList $ zip (map cellLocation cs) cs
        initialContext = EvalContext initialEvalMap [] emptyDiff
    -- you must insert the roots into the initial context, because getCellsToEval will give you cells to evaluate that
    -- are only in the context or in the DB (in that order of prececdence). IF neither, you won't get anything. 
    -- this maintains the invariant that context always contains the most up-to-date, complete information. 
    ctxAfterDispatch <- dispatch conn roots initialContext descSetting
    printWithTimeT "finished dispatch"
    finalizedCells <- EE.evalEndware state (addedCells ctxAfterDispatch) src roots ctxAfterDispatch
    let ctx = ctxAfterDispatch { addedCells = finalizedCells }
        -- END BOILERPLATE
        --
    (commit,didDecouple) <- lift $ evalContextToCommit ctx
    lift $ pushCommitOrTempCommit conn src didDecouple commit
    if (didDecouple)
      then left DecoupleAttempt
      else return commit

  let msg = newMakeUpdateMessage errOrCommit
  printObj "made message: " msg
  return msg

makeUpdateMessageFromCommit :: Either ASExecError ASCommit -> ASServerMessage
makeUpdateMessageFromCommit (Left err) = makeErrorMessage err Update
makeUpdateMessageFromCommit (Right comm) = ServerMessage Update Success (PayloadCL $ afterCells $ cellDiff comm)

-- takes an old context, inserts the new values necessary for this round of eval, and evals using the new context.
-- this seems conceptually better than letting each round of dispatch produce a new context, 
-- and hoping we union them in the right order. This means that at any point in time, there is a *single*
-- EvalContext in existence, and we just continue writing to it every time dispatch is called recursively. 
dispatch :: Connection -> [ASCell] -> EvalContext -> DescendantsSetting -> EitherTExec EvalContext
dispatch conn [] context _ = printWithTimeT "empty dispatch" >> return context
dispatch conn roots oldContext descSetting = do
  printObjT "STARTING DISPATCH CYCLE WITH CELLS" roots
  printWithTimeT $ "Settings: Descendants: " ++ (show descSetting)
  -- For all the original cells, add the edges in the graph DB; parse + setRelations
  G.setCellsAncestors roots
  descLocs       <- getEvalLocs conn roots descSetting
  printObjT "Got eval locations" descLocs
  -- Turn the descLocs into Cells, but the roots are already given as cells, so no DB actions needed there
  cellsToEval    <- getCellsToEval oldContext descLocs
  printObjT "Got cells to evaluate" cellsToEval
  ancLocs        <- G.getImmediateAncestors $ indicesToGraphReadInput descLocs
  printObjT "Got ancestor locs" ancLocs
  -- The initial lookup cache has the ancestors of all descendants
  modifiedContext <- getModifiedContext conn ancLocs oldContext
  lift $ putStrLn $ "Created initial context"  -- ++ (show modifiedContext)
  printWithTimeT "Starting eval chain"
  evalChainWithException conn modifiedContext cellsToEval -- start with current cells, then go through descendants


----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval building blocks

-- | Return the locations of cells to evaluate. (Just the descendants of all the cells passed in
-- to the dispatch cycle, plus all the volatile cells -- the ones that always re-evaluate.)
-- List of locations guaranteed to be topologically sorted by dependency. (So the cells with no
-- dependencies are evaluated first.)
-- TODO: throw exceptions for permissions/locking
-- Currently, the graph returns only indices as descendants
getEvalLocs :: Connection -> [ASCell] -> DescendantsSetting -> EitherTExec [ASIndex]
getEvalLocs conn origCells descSetting = do
  let locs = map cellLocation origCells
  vLocs <- lift $ DB.getVolatileLocs conn -- Accounts for volatile cells being reevaluated each time
  case descSetting of 
    ProperDescendants -> G.getProperDescendantsIndices $ (locs ++ vLocs)
    DescendantsWithParent -> G.getDescendantsIndices $ (locs ++ vLocs)

-- | Given a set of locations to eval, return the corresponding set of cells to perform
-- the evaluations in. We look up locs in the DB, but give precedence to EvalContext (if a cell is in the context, we use that instead, 
-- as it is the most up-to-date info we have). 
-- this function is order-preserving
getCellsToEval :: EvalContext -> [ASIndex] -> EitherTExec [ASCell]
getCellsToEval ctx locs = possiblyThrowException =<< (lift $ DB.getCellsWithContext ctx locs)
  where 
    possiblyThrowException mcells = if any isNothing mcells
      then left $ DBNothingException missingLocs
      else return $ map fromJust mcells
        where missingLocs = map fst $ filter (\(_,mc) -> isNothing mc) $ zip locs mcells 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Maps 

-- see the comments above dispatch to see why an old evalContext is passed in.
getModifiedContext :: Connection -> [ASReference] -> EvalContext -> EitherTExec EvalContext
getModifiedContext conn ancs oldContext = do
   ancIndices <-  lift $ concat <$> mapM (DB.refToIndicesWithContextBeforeEval oldContext) ancs 
   let nonContextAncIndices = filter (not . (flip M.member (contextMap oldContext))) ancIndices
   cells <- lift $ DB.getPossiblyBlankCells nonContextAncIndices
   let oldMap = contextMap oldContext
       newContext = oldContext { contextMap = insertMultiple oldMap nonContextAncIndices cells }
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

-- A wrapper around evalChain which catches errors
evalChainWithException :: Connection  -> EvalContext -> [ASCell] -> EitherTExec EvalContext
evalChainWithException conn ctx cells = 
  let whenCaught e = do
        printObj "Runtime exception caught" (e :: SomeException)
        return $ Left RuntimeEvalException
  in do
    result <- liftIO $ catch (runEitherT $ evalChain conn ctx cells) whenCaught
    hoistEither result

-- If a cell input to evalChain is a coupled cell that's not a fat-cell-head, then we NEVER evaluate it. In addition, if there's a normal cell
-- in the list of cells that has a coupled counterpart in the context, we don't evaluate that cell either. The reason is that we want to give 
-- fat cells overwrite power (this is a UX feature that we're adding, so it requires special casing).  Example: A1 = 1, A2 = A1, A1 = range(10) 
-- should keep A1 coupled.  It shouldn't eval the range, then eval A2, which would decouple the range. In our function, A2 wouldn't even 
-- be evalled even if it were in the queue as a normal cell. 
evalChain :: Connection -> EvalContext -> [ASCell] -> EitherTExec EvalContext
evalChain conn ctx cells = evalChain' conn ctx cells''
  where 
    hasCoupledCounterpartInMap c = case (cellLocation c) `M.lookup` (contextMap ctx) of
      Nothing -> False
      Just c' -> (isCoupled c') && (not $ isFatCellHead c')
    cells' = filter isEvaluable cells
    cells'' = filter (not . hasCoupledCounterpartInMap) cells'

evalChain' :: Connection -> EvalContext -> [ASCell] -> EitherTExec EvalContext
evalChain' _ ctx [] = printWithTimeT "empty evalchain" >> return ctx
evalChain' conn ctx (c@(Cell loc xp val ps):cs) = do
  printWithTimeT $ "running eval chain with cells: " ++ (show (c:cs))
  let getEvalResult expression = EC.evaluateLanguage conn loc ctx expression 
  cvf <- case xp of 
    Expression _ _         ->  getEvalResult xp
    Coupled str lang _ key -> if (isFatCellHead c)
      then getEvalResult $ Expression str lang
      else return $ Formatted (CellValue val) (formatType <$> getProp ValueFormatProp ps) 
  newContext <- contextInsert conn c cvf ctx
  evalChain conn newContext cs


-- Deal with a possible shrink list. The ASCell passed in is a descendant during dispatch. We're not doing anything if it isn't coupled. All descendants of a cell
-- must be fat cell heads or normal cells. If it's a fat cell head, we're going to blank out all indices in the corresponding objectfor now and propagate 
-- those changes. The composite value returned by the previous eval chain will then write over this object in contextInsert. 
-- Example: if range(10) becomes range(5) because of an upstream change, blank out A1:A10. The cv in the previous evalChain has the coupled range(5) cells and 
-- will replace the contextMap. Essentially, this transform exists because you should delete an object before overwriting it while evaluating a descendant. 
-- its return type is (new context, list of indices that were blanked out as a result)
possiblyDeletePreviousFatCell :: Connection -> ASCell -> EvalContext -> EitherTExec (EvalContext, Maybe [ASIndex])
possiblyDeletePreviousFatCell conn c@(Cell idx xp _ ps) ctx@(EvalContext mp addedCells ddiff) = case xp of 
  Expression _ _         ->  return (ctx, Nothing)
  Coupled str lang _ key -> if (isFatCellHead c)
    then do 
      let indices = rangeKeyToIndices key
          blankCells = map blankCellAt indices
      descriptor <- lift $ DB.getRangeDescriptorUsingContext conn ctx key
      printWithTimeT $ "REMOVED DESCRIPTOR IN DELETE PREVIOUS FAT CELL: " ++ (show descriptor)
      printWithTimeT $ "BLANK CELLS: " ++ (show blankCells)          
      -- remove the descriptor
      let ddiff = descriptorDiff ctx
      let ddiff' = case descriptor of
                Nothing -> ddiff
                Just d  -> removeDescriptor ddiff d
          -- the blanked cells have to be added to the list of added cells. If anything replaces these blank cells, contextInsert will merge them in. 
          newAddedCells = mergeCells blankCells addedCells
      let ctx' = ctx { contextMap = insertMultiple (contextMap ctx) indices blankCells
                     , addedCells = newAddedCells
                     , descriptorDiff = ddiff' }
      return (ctx', Just indices)
    else left WillNotEvaluate

-- NEXT: do checks on rangekeys being in both added and removed.

-- the cell passed in is the old cell (we insert the old cell + new eval'ed value into the context at the end of this function, 
-- after all side effects due to insertion have been handled)
-- if you get here, your cell has already been evaluated, and we're from now on going to call dispatch with ProperDescendants set.
contextInsert :: Connection -> ASCell -> Formatted CompositeValue -> EvalContext -> EitherTExec EvalContext
contextInsert conn c@(Cell idx xp _ ps) (Formatted cv f) ctx = do 
  printWithTimeT $ "running context insert with old cell " ++ (show c)
  printWithTimeT $ "the context insert has value " ++ (show cv)
  -- this should be the very first transform to EvalContext applied, because you should delete an object before overwriting it while evaluating a descendant. 
  lift $ putStrLn "running previous fatcell removal transform"
  (ctx'@(EvalContext mp cells ddiff), blankedIndices) <- possiblyDeletePreviousFatCell conn c ctx
  -- The newly created cell(s) may have caused decouplings, which we're going to deal with. First, we get the decoupled keys 
  let maybeFatCell = DE.decomposeCompositeValue c cv
  newlyRemovedDescriptors <- lift $ case maybeFatCell of
    Nothing -> DX.getFatCellIntersections conn ctx' (Left [idx])
    Just (FatCell _ descriptor) -> DX.getFatCellIntersections conn ctx' (Right [descriptorKey descriptor])
  printObjT "got decoupled descriptors in contextInsert" newlyRemovedDescriptors
  -- The locations we have to decouple can be constructed from the range keys, which have info about the
  -- head and size of the range
  let decoupledLocs = concat $ map (rangeKeyToIndices . descriptorKey) newlyRemovedDescriptors
  -- Given the locs, we get the cells that we have to decouple from the DB and then change their expressions
  -- to be decoupled (by using the value of the cell)
  decoupledCells <- lift $ ((map DI.toDecoupled) . catMaybes) <$> DB.getCellsWithContext ctx' decoupledLocs
  printWithTimeT $ "DECOUPLED CELLS: " ++ (show decoupledCells)
  -- We want to update all of the decoupled cells in our mini-spreadsheet map
  let mpWithDecoupledCells = insertMultiple mp decoupledLocs decoupledCells
  -- Add our decoupled descriptors to the current list of removedDescriptors
  let ddiff = descriptorDiff ctx'
  let ddiffWithRemovedDescriptors = L.foldl' removeDescriptor ddiff newlyRemovedDescriptors
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
          resultContext = EvalContext finalMp finalCells ddiffWithRemovedDescriptors
      -- now, propagate the descandants of the decoupled cells
      lift $ putStrLn "running decouple transform"
      dispatch conn decoupledCells resultContext ProperDescendants
    Just (FatCell cs descriptor) -> do 
      -- Modify the props of our current cells to reflect any format
      let newCells = map (formatCell f) cs
      printWithTimeT $ "FAT CELL EXPANDED CELLS: " ++ (show cs)
      -- The final updated map also has our newly evaluated cells in it, with updated cell value and props
      let finalMp = insertMultiple mpWithDecoupledCells (map cellLocation newCells) newCells
          finalDDiff = addDescriptor ddiffWithRemovedDescriptors descriptor
          finalCells = mergeCells cs $ mergeCells decoupledCells cells
          finalContext = EvalContext finalMp finalCells finalDDiff
      lift $ putStrLn "running expanded cells transform"
      -- propagate the descendants of the expanded cells (except for the list head)
      -- you don't set relations of the newly expanded cells, because those relations do not exist. Only the head of the list has an ancestor at this point.
      -- first, check if we blanked out anything as a result of possiblyDeletePreviousFatCell. if so, look up the new cells from the map. 
      -- e.g. if range(5) -> range(2), possiblyDeletePrevious... will return indices A1...A5, and we're going to need to run dispatch on all of those.
      -- so this particular dispatch merges the context transforms (1) expanded cells, (2) blanked cells due to fat cell deletion
      let blankCells = case blankedIndices of 
                        Nothing -> []
                        Just inds -> map ((M.!) (contextMap finalContext)) inds
          cs' = mergeCells cs $ mergeCells decoupledCells blankCells
      printWithTimeT $ "CELLS SENT TO DISPATCH FOR EXPANSION: " ++ (show cs')
      dispatch conn cs' finalContext ProperDescendants
