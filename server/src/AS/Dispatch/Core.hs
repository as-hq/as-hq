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

-- assumes all evaled cells are in the same sheet
-- the only information we're really passed in from the cells is the locations and the expressions of
-- the cells getting evaluated. We pull the rest from the DB. 
runDispatchCycle :: MVar ServerState -> [ASCell] -> DescendantsSetting -> CommitSource -> IO ASServerMessage
runDispatchCycle state cs descSetting src = do
  printObj "cs" cs
  roots <- EM.evalMiddleware cs
  conn <- dbConn <$> readMVar state
  errOrCells <- runEitherT $ do
    printWithTimeT $ "about to start dispatch"
    ctxAfterDispatch <- dispatch conn roots emptyContext descSetting SetAncestry
    printWithTimeT $ "finished dispatch"
    finalizedCells <- EE.evalEndware state (addedCells ctxAfterDispatch) src roots ctxAfterDispatch
    let ctx = ctxAfterDispatch { addedCells = finalizedCells }
    broadcastCells <- DT.updateDBFromEvalContext conn src ctx
    return broadcastCells
  case errOrCells of 
    Left _ -> G.recompute conn -- #needsrefactor. Overkill. But recording all cells that might have changed is a PITA. (Alex 11/20)
    _      -> return ()
  return $ makeUpdateMessage errOrCells

-- takes an old context, inserts the new values necessary for this round of eval, and evals using the new context.
-- this seems conceptually better than letting each round of dispatch produce a new context, 
-- and hoping we union them in the right order. This means that at any point in time, there is a *single*
-- EvalContext in existence, and we just continue writing to it every time dispatch is called recursively. 
dispatch :: Connection -> [ASCell] -> EvalContext -> DescendantsSetting -> AncestrySetting -> EitherTExec EvalContext
dispatch conn [] context _ _ = return context
dispatch conn roots oldContext descSetting ancSetting = do
  printObjT "STARTING DISPATCH CYCLE WITH CELLS" roots
  printWithTimeT $ "Settings: Descendants: " ++ (show descSetting) ++ ", Ancestors: " ++ (show ancSetting)
  -- For all the original cells, add the edges in the graph DB; parse + setRelations
  rootsDepSets <- case ancSetting of 
    SetAncestry -> G.setCellsAncestors roots
    DontSetAncestry -> return []
  printObjT "Set cell ancestors" rootsDepSets
  descLocs       <- getEvalLocs conn roots descSetting
  printObjT "Got eval locations" descLocs
  -- Turn the descLocs into Cells, but the roots are already given as cells, so no DB actions needed there
  cellsToEval    <- getCellsToEval conn descLocs roots
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
getModifiedContext :: Connection -> [ASReference] -> EvalContext -> EitherTExec EvalContext
getModifiedContext conn ancs oldContext = do
   -- ::RITESH::  hella unsafe
   ancIndices <- concat <$> mapM (DB.refToIndicesWithContext oldContext) ancs 
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


evalChain :: Connection -> EvalContext -> [ASCell] -> EitherTExec EvalContext
evalChain _ ctx [] = printWithTimeT "empty evalchain" >> return ctx
evalChain conn ctx (c@(Cell loc xp val ps):cs) = do
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
possiblyDeletePreviousFatCell :: Connection -> ASCell -> EvalContext -> EitherTExec EvalContext
possiblyDeletePreviousFatCell conn c@(Cell idx xp _ ps) ctx@(EvalContext mp addedCells removedDescriptors addedDescriptors) = case xp of 
  Expression _ _         ->  return ctx
  Coupled str lang _ key -> if (isFatCellHead c)
    then do 
      let indices = rangeKeyToIndices key
          blankCells = map blankCellAt indices
      descriptor <- lift $ DB.getRangeDescriptorUsingContext conn ctx key
      printWithTimeT $ "REMOVED DESCRIPTOR IN DELETE PREVIOUS FAT CELL: " ++ (show descriptor)
      printWithTimeT $ "BLANK CELLS: " ++ (show blankCells)
      -- remove the descriptor
      let newRemovedDescriptors = case descriptor of
                Nothing -> removedDescriptors
                Just d  -> d:removedDescriptors
          -- the blanked cells have to be added to the list of added cells. If anything replaces these blank cells, contextInsert will merge them in. 
          newAddedCells = mergeCells blankCells addedCells
      let ctx' = ctx { contextMap = insertMultiple (contextMap ctx) indices blankCells, removedDescriptors = newRemovedDescriptors, addedCells = newAddedCells }
      dispatch conn blankCells ctx' ProperDescendants DontSetAncestry
    else left WillNotEvaluate


-- current state: when you do a1=5, a2=range(a1), then a1=2, it doesn't work. We think this is bc the old range descriptor still exists when you do a dispatch with
-- the deleted cells. getFatCellIntersetions should sometimes look at context first.

-- the cell passed in is the old cell (we insert the old cell + new eval'ed value into the context at the end of this function, 
-- after all side effects due to insertion have been handled)
-- if you get here, your cell has already been evaluated, and we're from now on going to call dispatch with ProperDescendants set.
contextInsert :: Connection -> ASCell -> Formatted CompositeValue -> EvalContext -> EitherTExec EvalContext
contextInsert conn c@(Cell idx xp _ ps) (Formatted cv f) ctx = do 
  -- this should be the very first transform to EvalContext applied, because you should delete an object before overwriting it while evaluating a descendant. 
  (EvalContext mp cells removedDescriptors addedDescriptors) <- possiblyDeletePreviousFatCell conn c ctx
  printWithTimeT $ "running context insert with cell " ++ (show c)
  printWithTimeT $ "the context insert has value " ++ (show cv)
  let maybeFatCell = DE.decomposeCompositeValue c cv
  -- The newly created cell(s) may have caused decouplings, which we're going to deal with. First, we get the decoupled keys 
  newlyRemovedDescriptors <- lift $ case maybeFatCell of
    Nothing -> DX.getFatCellIntersections conn ctx (Left [idx])
    Just (FatCell _ descriptor) -> DX.getFatCellIntersections conn ctx (Right [descriptorKey descriptor])
  printObjT "got decoupled descriptors in contextInsert" newlyRemovedDescriptors
  -- The locations we have to decouple can be constructed from the range keys, which have info about the
  -- head and size of the range
  let decoupledLocs = concat $ map (rangeKeyToIndices . descriptorKey) newlyRemovedDescriptors
  -- Given the locs, we get the cells that we have to decouple from the DB and then change their expressions
  -- to be decoupled (by using the value of the cell)
  decoupledCells <- lift $ ((map DI.toDecoupled) . catMaybes) <$> DB.getCells decoupledLocs
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
      dispatch conn decoupledCells resultContext ProperDescendants DontSetAncestry
    Just (FatCell cs descriptor) -> do 
      -- Modify the props of our current cells to reflect any format
      let newCells = map (formatCell f) cs
      -- The final updated map also has our newly evaluated cells in it, with updated cell value and props
      let finalMp = insertMultiple mpWithDecoupledCells (map cellLocation newCells) newCells
          finalAddedDescriptors = descriptor:addedDescriptors
          finalCells = mergeCells cs $ mergeCells decoupledCells cells
          finalContext = EvalContext finalMp finalCells finalRemovedDescriptors finalAddedDescriptors
      -- propagate the descandants of the decoupled cells
      -- decoupled cells don't have ancestors, so you don't set relations.
      finalContext' <- dispatch conn decoupledCells finalContext ProperDescendants DontSetAncestry 
      -- propagate the descendants of the expanded cells (except for the list head)
      -- you don't set relations of the newly expanded cells, because those relations do not exist. Only the head of the list has an ancestor at this point.
      dispatch conn cs finalContext' ProperDescendants DontSetAncestry

