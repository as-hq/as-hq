module AS.Dispatch.Core where

-- AlphaSheets and base
import AS.Types.Core
import AS.Types.DB
import Prelude
import qualified AS.Eval.Core as EC (evaluateLanguage)
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified AS.DB.API  as DB
import qualified AS.DB.Util as DU
import qualified Data.List  as L
import AS.Parsing.Common
import AS.Parsing.Show hiding (first)
import AS.Parsing.Read
import Data.Maybe (fromJust, isNothing,catMaybes)
import Text.ParserCombinators.Parsec
import Control.Applicative
import Data.Time.Clock
import Data.Text as T (unpack,pack)
import AS.Util as U
import AS.Eval.Middleware as EM
import AS.Eval.Endware as EE
import qualified AS.DB.Graph as G
import Control.Exception.Base

-- Websockets
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
-- Regular eval route

-- assumes all evaled cells are in the same sheet
runDispatchCycle :: MVar ServerState -> [ASCell] -> CommitSource -> IO ASServerMessage
runDispatchCycle state cs src = do
  let sid = locSheetId . cellLocation $ head cs
  errOrCells <- runEitherT $ do
    printObjT "STARTING DISPATCH CYCLE WITH CELLS: " cs
    roots          <- lift $ EM.evalMiddleware cs
    conn           <- lift $ fmap dbConn $ readMVar state
    rootsDepSets   <- DB.setCellsAncestors roots
    printObjT "Set cell ancestors" rootsDepSets
    evalLocs       <- getEvalLocs conn roots
    printObjT "Got eval locations" evalLocs
    cellsToEval    <- getCellsToEval conn evalLocs roots
    printObjT "Got cells to evaluate" cellsToEval
    ancLocs        <- G.getImmediateAncestors evalLocs
    printObjT "Got ancestor locs" ancLocs
    initValuesMap  <- lift $ getValuesMap (zip roots rootsDepSets) ancLocs
    printObjT "Created initial values map" initValuesMap
    printWithTimeT "Starting eval chain"
    (afterCells, cellLists) <- evalChain conn initValuesMap cellsToEval src -- start with current cells, then go through descendants
    -- Apply endware
    finalizedCells <- lift $ EE.evalEndware state afterCells src roots
    let transaction = Transaction src roots finalizedCells cellLists
    broadcastCells <- DB.updateAfterEval conn transaction -- atomically performs DB ops. (Sort of a lie -- writing to server is not atomic.)
    return broadcastCells
  runEitherT $ rollbackGraphIfError errOrCells
  return $ U.makeUpdateMessage errOrCells

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
  indices <- G.getDescendants (locs ++ vLocs)
  return indices

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

-- pass in the root deps in addition to the complete set of ancestors,
-- because retaining ASRange in the root dep set makes checking for
-- dataframes O(# lists) instead of O(n)
-- rootDeps is a subset of locs and we set both, but no need to remove duplicates from the map.
getValuesMap :: [(ASCell, [ASReference])] -> [ASIndex] -> IO RefValMap
getValuesMap rootRelations locs = do
  maybeCells <- DB.getCells locs
  let simpleFormatted = formatValuesForMap $ zip locs maybeCells
  groupedRefs <- concat <$> mapM groupRefs rootRelations
  return $ M.fromList $ groupedRefs ++ simpleFormatted

groupRefs :: (ASCell, [ASReference]) -> IO [(ASReference, ASValue)]
groupRefs relation@(cell, refs) = if (shouldGroupRefs relation)
  then do
    let rngs = map (\(RangeRef rng) -> rng) $ filter isRange refs
    rngValSets <- mapM DB.getCellsByRange rngs
    let rngValSets' = map (\rs -> map getSanitizedCellValue rs) rngValSets
        groupedRefs = map (groupRef . language . cellExpression $ cell) $ zip rngs rngValSets'
    return $ filterNothing groupedRefs
  else return []

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval helpers

-- | Evaluates a list of cells, in serial order, updating the reference/value map with each
-- cell that's updated. The cells passed in are guaranteed to be topologically sorted, i.e.,
-- if a cell references an ancestor, that ancestor is guaranteed to already have been
-- added in the map.
evalChain :: Connection -> RefValMap -> [ASCell] -> CommitSource -> EitherTExec ([ASCell], [ASList])
evalChain conn valuesMap cells src = do
  result <- liftIO $ catch (runEitherT $ evalChain' conn valuesMap cells [] []) (\e -> do
    printObj "Runtime exception caught" (e :: SomeException)
    U.writeErrToLog ("Runtime exception caught" ++ (show e)) src
    return $ Left RuntimeEvalException)
  case result of
    (Left e) -> left e
    (Right e) -> right e


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
evalChain' :: Connection -> RefValMap -> [ASCell] -> [ASList] -> [ASIndex] -> EitherTExec ([ASCell], [ASList])
evalChain' _ _ [] [] _ = return ([], [])
evalChain' conn valuesMap [] lists pastListHeads = do
  -- get cells from lists
  let cells         = concat $ map snd lists
      listCellLocs  = map cellLocation cells
      listCellLocs' = filter (\d -> not $ d `elem` pastListHeads) listCellLocs
  descLocs <- G.getDescendants listCellLocs'
  -- check for circular dependencies. IF a circular dependency exists, it necessarily has to
  -- involve one of the list heads, since the cells created as part of a list depend only
  -- on the head. So we go through the descendants of the current list cells (sans the previous
  -- list heads), so if those contain any of the previous list heads we know there's a cycle.
  mapM_ (\d -> if (d `elem` pastListHeads) then (left $ CircularDepError d) else (return ())) descLocs
  -- DON'T need to re-eval anything that's already been evaluated
  let descLocs' = filter (\d -> (IndexRef d) `M.notMember` valuesMap) descLocs
  -- #needsrefactor it seems like a large chunk of code here mirrors that in evalChain... should probably DRY
  ancLocs <- G.getImmediateAncestors descLocs'
  newKnownValues <- lift $ getValuesMap [] ancLocs
  cells' <- getCellsToEval conn descLocs' [] -- the origCells are the list cells, which got filtered out of descLocs
  evalChain' conn (M.union valuesMap newKnownValues) cells' [] pastListHeads

evalChain' conn valuesMap (c@(Cell loc xp _ ts):cs) next listHeads = do
  cv <- EC.evaluateLanguage (IndexRef (cellLocation c)) (locSheetId loc) valuesMap xp
  let listResult = createListCells c cv
      newValuesMap = case listResult of
        Nothing          -> M.insert (IndexRef loc) cv valuesMap
        (Just cellsList) -> foldr (\(Cell l _ v _) mp -> M.insert (IndexRef l) v mp) valuesMap (snd cellsList)
      -- ^ adds all the cells in cellsList to the reference map
      next' = case listResult of
        Nothing        -> next
        Just cellsList -> cellsList:next
      listHeads' = case listResult of
        Nothing -> listHeads
        Just _  -> loc:listHeads
  (restCells, restLists) <- evalChain' conn newValuesMap cs next' listHeads'
  return $ case listResult of
    Nothing          -> ((Cell loc xp cv ts):restCells, restLists)
    (Just cellsList) -> (restCells, cellsList:restLists)

-- Removed for now for a number of reasons: 
-- 1) confusing UX
-- 2) frontend sorta handles the thing we want to avoid by not sending eval messages if the expression 
-- was the same. 
-- 3) slows things down nontrivially. 
-- -- | You should always re-eval a cell, UNLESS you're a part of a list, your expression was 
-- -- the same as last time's, and you're not the head of the list. 
-- shouldReEval :: ASCell -> IO Bool
-- shouldReEval c@(Cell loc xp _ ts) = do 
--   maybeOldCell <- DB.getCell loc
--   case maybeOldCell of 
--     Nothing -> return True
--     Just oldCell -> return $ (not $ isListMember oldCell) || (xp /= (cellExpression oldCell)) || (DU.isListHead oldCell)

-- type signature is sort of janky
rollbackGraphIfError :: Either ASExecError [ASCell] -> EitherTExec [ASIndex]
rollbackGraphIfError (Left e) = G.rollbackGraph
rollbackGraphIfError _ = return []
