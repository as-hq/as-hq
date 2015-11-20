module AS.Dispatch.Core where

-- AlphaSheets and base
import Prelude
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.List  as L
import Data.Maybe (fromJust, isNothing,catMaybes)
import Text.ParserCombinators.Parsec
import Control.Applicative
import Data.Time.Clock
import Data.Text as T (unpack,pack)
import Control.Exception.Base

import AS.Types.Core
import AS.Types.DB
import AS.Dispatch.Expanding        as DE
import qualified AS.Eval.Core       as EC (evaluateLanguage)
import qualified AS.DB.API          as DB
import qualified AS.DB.Transaction  as DT
import qualified AS.DB.Util         as DU
import AS.Util                      as U
import AS.Eval.Middleware           as EM
import AS.Eval.Endware              as EE
import qualified AS.DB.Graph        as G
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
testDispatch state lang crd str = runDispatchCycle state [Cell (Index sid crd) (Expression str Python) NoValue []] (sid, uid)
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
  roots <- EM.evalMiddleware cs
  conn <- dbConn <$> readMVar state
  errOrCells <- runEitherT $ do
    transaction <- dispatch state roots src
    broadcastCells <- DT.writeTransaction conn transaction-- atomically performs DB ops. (Sort of a lie -- writing to server is not atomic.)
    return broadcastCells
  case errOrCells of 
    Left _ -> G.rollbackGraph
    _      -> return ()
  return $ U.makeUpdateMessage errOrCells

dispatch :: MVar ServerState -> [ASCell] -> CommitSource -> EitherTExec ASTransaction
dispatch state roots src = do
  conn <- lift $ dbConn <$> readMVar state
  printObjT "STARTING DISPATCH CYCLE WITH CELLS: " roots
  rootsDepSets   <- DB.setCellsAncestors roots
  printObjT "Set cell ancestors" rootsDepSets
  descLocs       <- getEvalLocs conn roots
  printObjT "Got eval locations" descLocs
  cellsToEval    <- getCellsToEval conn descLocs roots
  printObjT "Got cells to evaluate" cellsToEval
  ancLocs        <- G.getImmediateAncestors descLocs
  printObjT "Got ancestor locs" ancLocs
  initValuesMap  <- lift $ getValuesMap conn ancLocs
  formattedValuesMap <- lift $ formatValsMap initValuesMap
  printObjT "Created initial values map" initValuesMap
  printWithTimeT "Starting eval chain"
  (afterCells, fatCells) <- evalChain conn formattedValuesMap cellsToEval src -- start with current cells, then go through descendants
  printObjT "Eval chain produced cells" afterCells
  liftIO $ putStrLn $ "Eval chain produced fatcells" ++ (show fatCells)
  -- Apply endware
  finalizedCells <- lift $ EE.evalEndware state afterCells src roots
  right $ Transaction src finalizedCells fatCells

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval building blocks

-- | Return the locations of cells to evaluate. (Just the descendants of all the cells passed in
-- to the dispatch cycle, plus all the volatile cells -- the ones that always re-evaluate.)
-- List of locations guaranteed to be topologically sorted by dependency. (So the cells with no
-- dependencies are evaluated first.)
-- TODO: throw exceptions for permissions/locking
getEvalLocs :: Connection -> [ASCell] -> EitherTExec [ASIndex]
getEvalLocs conn origCells = do
  let locs = map cellLocation origCells
  vLocs <- lift $ DB.getVolatileLocs conn -- Accounts for volatile cells being reevaluated each time
  G.getDescendants (locs ++ vLocs)

-- | Given a set of locations to eval, return the corresponding set of cells to perform
-- the evaluations in (which includes info about tags, language, and expression string).
-- Distinguishes between new cells to evaluate (the ones passed into runDispatchCycle)
-- and old cells already in the database, which all reference the new cells. For the new 
-- cells, just evaluate them as-is; for old cells, pull them from the database.
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

-- Takes in a value map, and change it from (Index -> ASValue) to (Index -> Formatted ASValue), which
-- is necessary for Excel evals.  Since this is only relevant for Excel evaluations, I'd ideally have
-- this in Eval/Core and only call it for Excel evals, but that would require O(n) calls to the DB, 
-- which is prohibitively expensive. 
-- TODO refactor s.t. we construct a formatted map from the beginning
formatValsMap :: ValMap -> IO FormattedValMap
formatValsMap mp = do
  let mp' = M.toList mp  
      locs = map fst mp'
      vals = map snd mp' 
  cells <- DB.getPossiblyBlankCells locs 
  let formats = map getCellFormatType cells
      vals' = map (\(v, ft) -> Formatted v ft) (zip vals formats)
  return $ M.fromList $ zip locs vals'

getValuesMap :: Connection -> [ASIndex] -> IO ValMap
getValuesMap conn locs = do
  vals <- map retrieveValue <$> DB.getCompositeCells conn locs
  return $ M.fromList $ zip locs vals

retrieveValue :: Maybe CompositeCell -> CompositeValue
retrieveValue c = case c of
  Just (Single cell) -> CellValue $ cellValue cell
  Just (Fat fcell) -> DE.recomposeCompositeValue fcell
  Nothing -> CellValue NoValue

formatCell :: Maybe FormatType -> ASCell -> ASCell
formatCell Nothing c = c
formatCell (Just f) c = c' 
  where 
    ts  = cellTags c
    ts' = filter (U.differentTagType (Format f)) ts -- the cell without that tag
    c' = case f of 
      NoFormat -> c { cellTags = ts' }
      _        -> c { cellTags = (Format f):ts' }

----------------------------------------------------------------------------------------------------------------------------------------------
-- EvalChain

-- | Evaluates a list of cells, in serial order, updating the reference/value map with each
-- cell that's updated. The cells passed in are guaranteed to be topologically sorted, i.e.,
-- if a cell references an ancestor, that ancestor is guaranteed to already have been
-- added in the map.
evalChain :: Connection -> FormattedValMap -> [ASCell] -> CommitSource -> EitherTExec ([ASCell], [FatCell])
evalChain conn valuesMap cells src = 
  let whenCaught e = do
          printObj "Runtime exception caught" (e :: SomeException)
          U.writeErrToLog ("Runtime exception caught" ++ (show e)) src
          return $ Left RuntimeEvalException
  in do
    result <- liftIO $ catch (runEitherT $ evalChain' conn valuesMap cells [] []) whenCaught
    hoistEither result

-- | evalChain' works in two parts. First, it goes through the list of cells passed in and
-- evaluates them. Along the way, new list cells (created as part of a list) are created
-- that may need to get re-evaluated. These get recorded in the fourth argument (type [ASList]),
-- and the heads of these lists get recorded in the fifth.
--
-- When we finish evaluating the original list of cells, we go through the newly created list cells
-- and essentially re-evaluate those. We are NOT just running eval on this list of cells directly,
-- because we don't need to re-evaluate the individual cells in the list, ONLY their descendants.
-- We also need to check for circular dependencies, which is why the pastListHeads are passed in.
-- #needsrefactor there's probably a more Haskell way of doing this with a state monad or something.

evalChain' :: Connection -> FormattedValMap -> [ASCell] -> [FatCell] -> [ASIndex] -> EitherTExec ([ASCell], [FatCell])
evalChain' _ _ [] [] _ = return ([], [])

evalChain' conn valuesMap [] fatCells pastFatCellHeads = 
  -- get expanded cells from fat cells
  let unwrap (FatCell fcells _)     = fcells
      cells                           = concat $ map unwrap fatCells
      isFatCellHead loc               = loc `elem` pastFatCellHeads
      nonHeadLocs                     = filter (not . isFatCellHead) $ map cellLocation cells
      checkCircular loc               = if (isFatCellHead loc) then (left $ CircularDepError loc) else (return ())
      isNotInMap loc                  = loc `M.notMember` valuesMap
  in do
    -- NOTE: we only need to deal with properDescLocs (not including original start locs) in this method, and not all desc locs
    -- This is because listCellLocs' already filters for not being in a pastListHead, 
    -- and also because the starting locs can't possibly be in the value map, and don't need to be re-evaluated
    descLocs <- filter isNotInMap <$> G.getDescendants nonHeadLocs
    -- check for circular dependencies. IF a circular dependency exists, it necessarily has to
    -- involve one of the list heads, since the cells created as part of a list depend only
    -- on the head. So we go through the descendants of the current list cells (sans the previous
    -- list heads), so if those contain any of the previous list heads we know there's a cycle.
    mapM_ checkCircular descLocs
    -- DON'T need to re-eval anything that's already been evaluated
    -- #needsrefactor it seems like a large chunk of code here mirrors that in evalChain... should probably DRY
    ancLocs <- G.getImmediateAncestors descLocs
    formattedNewMap <- lift $ formatValsMap =<< getValuesMap conn ancLocs
    cells' <- getCellsToEval conn descLocs [] -- the origCells are the list cells, which got filtered out of descLocs
    evalChain' conn (M.union valuesMap formattedNewMap) cells' [] pastFatCellHeads

evalChain' conn valuesMap (c@(Cell loc xp _ ts):cs) fatCells fatCellHeads = do
  cvf@(Formatted cv f) <- EC.evaluateLanguage (locSheetId loc) (cellLocation c) valuesMap xp
  let maybeFatCell              = DE.decomposeCompositeValue c cv
      addCell (Cell l _ v _) mp = M.insert l (Formatted (CellValue v) f) mp
      newValuesMap              = case maybeFatCell of
              Nothing -> if (M.member ptr valuesMap) 
                then M.insert ptr cvf idxInserted
                else idxInserted
                where 
                  ptr = indexToPointer loc
                  idxInserted = M.insert loc cvf valuesMap
      -- ^ when updating a location in the map, check if there are Pointer references to the same location.
      -- if so, update them too
              Just (FatCell expandedCells _) -> foldr addCell valuesMap expandedCells
      -- ^ adds all the cells in cellsList to the reference map
      (fatCells', fatCellHeads') = case maybeFatCell of
                Nothing -> (fatCells, fatCellHeads)
                Just f  -> (f:fatCells, loc:fatCellHeads)
  (restCells, restFatCells) <- evalChain' conn newValuesMap cs fatCells' fatCellHeads'
  -- TODO investigate strictness here
  return $ case maybeFatCell of
    Nothing        -> (newCell:restCells, restFatCells)
      where
        (CellValue v) = cv
        newCell = formatCell f (Cell loc xp v ts)
    Just fatCell -> (restCells, fatCell:restFatCells)