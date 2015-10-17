module AS.Dispatch.Core where

-- AlphaSheets and base
import AS.Types.Core
import AS.Types.DB
import Prelude
import qualified AS.Eval.Core as EC (evaluateLanguage)
import qualified Data.Map   as M
import qualified AS.DB.API  as DB
import qualified AS.DB.Util as DU
import qualified Data.List  as L
import AS.Parsing.Common
import AS.Parsing.Out hiding (first)
import AS.Parsing.In
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

-- list of temp edges
-- remove edges method
-- commit edges

-- check for cycles method
-- do rollback in error

----------------------------------------------------------------------------------------------------------------------------------------------
-- Regular eval route

-- assumes all evaled cells are in the same sheet
runDispatchCycle :: MVar ServerState -> [ASCell] -> ASUserId -> IO ASServerMessage
runDispatchCycle state cs uid = do
  let sid = locSheetId . cellLocation $ head cs
  errOrCells <- runEitherT $ do
    printWithTimeT $ "STARTING DISPATCH CYCLE WITH CELLS: " ++ (show cs)
    roots          <- lift $ EM.evalMiddleware cs
    conn           <- lift $ fmap dbConn $ readMVar state
    rootsDepSets   <- setCellsAncestorsInDb conn roots
    evalLocs       <- getEvalLocs conn roots
    cellsToEval    <- getCellsToEval conn evalLocs roots
    ancLocs        <- G.getImmediateAncestors evalLocs
    printWithTimeT $ "got ancestor locs: " ++ (show ancLocs)
    initValuesMap  <- lift $ getValuesMap (zip roots rootsDepSets) ancLocs
    printWithTimeT "Starting eval chain"
    (afterCells, cellLists) <- evalChain conn initValuesMap cellsToEval -- start with current cells, then go through descendants
    -- Apply endware
    finalizedCells <- lift $ EE.evalEndware state afterCells uid roots
    let transaction = Transaction uid sid roots cellsToEval finalizedCells cellLists
    broadcastCells <- DB.updateAfterEval conn transaction -- atomically performs DB ops
    return broadcastCells
  runEitherT $ rollbackGraphIfError errOrCells
  return $ U.getCellMessage errOrCells

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval building blocks

-- | Update the ancestors of a cell, and set the ancestor relationships in the DB.
setCellsAncestorsInDb :: Connection -> [ASCell] -> EitherTExec [[ASReference]]
setCellsAncestorsInDb conn cells = (flip mapM) cells (\(Cell loc expr _ ts) -> do
  let deps = getDependencies (locSheetId loc) expr
      deps' = concat $ map refToIndices deps
  G.setRelations [(loc, deps')]
  return deps)

-- | Return the locations of cells to evaluate. (Just the descendants of all the cells passed in
-- to the dispatch cycle, plus all the volatile cells -- the ones that always re-evaluate.)
-- List of locations guaranteed to be topologically sorted by dependency. (So the cells with no
-- dependencies are evaluated first.)
-- TODO: throw exceptions for permissions/locking
getEvalLocs :: Connection -> [ASCell] -> EitherTExec [ASIndex]
getEvalLocs conn origCells = do
  let locs = map cellLocation origCells
  vLocs <- lift $ DB.getVolatileLocs conn -- Accounts for volatile cells being reevaluated each time
  printDebugT "locs" (locs ++ vLocs)
  indices <- G.getDescendants (locs ++ vLocs)
  return indices

-- | Given a set of locations to eval, return the corresponding set of cells to perform
-- the evaluations in (which includes info about tags, language, and expression string).
-- Distinguishes between new cells to evaluate (the ones passed into runDispatchCycle)
-- and old cells already in the database.  For the new cells, just evaluate them as-is;
-- for old cells, pull them from the database.
getCellsToEval :: Connection -> [ASIndex] -> [ASCell] -> EitherTExec [ASCell]
getCellsToEval conn locs origCells = do
  let locCellMap = M.fromList $ map (\c -> (cellLocation c, c)) origCells
  lift $ mapM (\loc ->
    if loc `M.member` locCellMap
      then return (locCellMap M.! loc)
      else do
        mCell <- DB.getCell loc
        return $ fromJust mCell) locs

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
evalChain :: Connection -> RefValMap -> [ASCell] -> EitherTExec ([ASCell], [ASList])
evalChain _ _ [] = return ([], [])
evalChain conn valuesMap (c@(Cell loc xp _ ts):cs) = do
  cv <- EC.evaluateLanguage (IndexRef (cellLocation c)) (locSheetId loc) valuesMap xp
  case cv of
    ValueL lst -> do
      let cellsList = createListCells c cv
          newMp     = M.insert (IndexRef loc) (head lst) valuesMap
      (restCells, restLists) <- evalChain conn newMp cs
      return (restCells, cellsList:restLists)
    RDataFrame rows -> do
      let cellsList = createListCells c cv
          headValue = case (head rows) of
            (ValueL l) -> head l
            a -> a
          newMp     = M.insert (IndexRef loc) headValue valuesMap
      (restCells, restLists) <- evalChain conn newMp cs
      return (restCells, cellsList:restLists)
    _ -> do
      let newValuesMap = M.insert (IndexRef loc) cv valuesMap
      (restCells, restLists) <- evalChain conn newValuesMap cs
      return $ ((Cell loc xp cv ts):restCells, restLists)
-- TODO: The Haskell way is probably to write this using foldM somehow, but that's not very urgent.

-- | If a cell C contains a 1D or 2D list, it'll be represented in the grid as a matrix.
-- This function takes in the starting cell with the starting expression, and creates the list
-- of cells. For example, [[1,2],[3,4]] entered into A1 should put values of 1 in A1, 2 in B1,
-- 3 in A2, and 4 in B2. createListCells called on A1 with the expression [[1,2],[3,4]] in it
-- should return a list of cells located at A1, A2, B1, B2, with:
--   1) the values 1, 2, 3, and 4, in them,
--   2) references to the original cell being called
--   3) the expression [[1,2],[3,4]] stored in them.
createListCells :: ASCell -> ASValue -> ASList
createListCells _ (ValueL []) = ("",[])
createListCells _ (RDataFrame []) = ("",[])
createListCells (Cell (Index sheet (a,b)) xp _ ts) cv = (listKey, cells)
  where
    values    = getValuesFromCV cv
    origLoc   = Index sheet (a,b)
    rows      = map toList values
    zipVals   = zip [0..] values
    locs      = map (Index sheet) (concat $ map (\(row, val) -> shift val row (a,b)) zipVals)
    height    = length values
    width     = maximum $ map length rows
    listKey   = DU.getListKey origLoc (height, width)
    tags      = getExtraTagsFromCV cv
    cells     = map (\(loc, val) -> Cell loc xp val ((ListMember listKey):tags)) $ zip locs (concat rows)

    getValuesFromCV (ValueL l)        = l
    getValuesFromCV (RDataFrame rows) = U.transposeList rows

    getExtraTagsFromCV (ValueL _)     = []
    getExtraTagsFromCV (RDataFrame _) = [DFMember]

    shift (ValueL v) r (a,b)  = [(a+c,b+r) | c<-[0..length(v)-1] ]
    shift other r (a,b)       = [(a,b+r)]


rollbackGraphIfError :: Either ASExecError [ASCell] -> EitherTExec [ASIndex]
rollbackGraphIfError (Left e) = G.rollbackGraph
rollbackGraphIfError _ = return []
