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
-- Regular eval route

-- assumes all evaled cells are in the same sheet
runDispatchCycle :: MVar ServerState -> [ASCell] -> CommitSource -> IO ASServerMessage
runDispatchCycle state cs src = do
  let sid = locSheetId . cellLocation $ head cs
  errOrCells <- runEitherT $ do
    printObjT "STARTING DISPATCH CYCLE WITH CELLS: " cs
    roots          <- lift $ EM.evalMiddleware cs
    conn           <- lift $ dbConn <$> readMVar state
    rootsDepSets   <- DB.setCellsAncestors roots
    printObjT "Set cell ancestors" rootsDepSets
    descLocs       <- getEvalLocs conn roots
    printObjT "Got eval locations" descLocs
    cellsToEval    <- getCellsToEval conn descLocs roots
    printObjT "Got cells to evaluate" cellsToEval
    ancLocs        <- G.getImmediateAncestors descLocs
    printObjT "Got ancestor locs" ancLocs
    initValuesMap  <- lift $ getValuesMap conn ancLocs
    printObjT "Created initial values map" initValuesMap
    printWithTimeT "Starting eval chain"
    (afterCells, fatCells) <- evalChain conn initValuesMap cellsToEval src -- start with current cells, then go through descendants
    printObjT "Eval chain produced cells" afterCells
    liftIO $ putStrLn $ "Eval chain produced fatcells" ++ (show fatCells)
    -- Apply endware
    finalizedCells <- lift $ EE.evalEndware state afterCells src roots
    let transaction = Transaction src roots finalizedCells fatCells
    broadcastCells <- DT.writeTransaction conn transaction -- atomically performs DB ops. (Sort of a lie -- writing to server is not atomic.)
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

getValuesMap :: Connection -> [ASIndex] -> IO ValMap
getValuesMap conn locs = do
  vals <- map retrieveValue <$> DB.getCompositeCells conn locs
  return $ M.fromList $ zip locs vals

retrieveValue :: Maybe CompositeCell -> CompositeValue
retrieveValue c = case c of
  Just (Single cell) -> CellValue $ cellValue cell
  Just (Fat fcell) -> DE.recomposeCompositeValue fcell
  Nothing -> CellValue NoValue
----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval helpers

-- | Evaluates a list of cells, in serial order, updating the reference/value map with each
-- cell that's updated. The cells passed in are guaranteed to be topologically sorted, i.e.,
-- if a cell references an ancestor, that ancestor is guaranteed to already have been
-- added in the map.
evalChain :: Connection -> ValMap -> [ASCell] -> CommitSource -> EitherTExec ([ASCell], [FatCell])
evalChain conn valuesMap cells src = do
  result <- liftIO $ catch (runEitherT $ evalChain' conn valuesMap cells []) (\e -> do
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
evalChain' :: Connection -> ValMap -> [ASCell] -> [FatCell] -> EitherTExec ([ASCell], [FatCell])
evalChain' _ _ [] fcells = return ([], [])
evalChain' conn valuesMap [] fatCells = 
  -- get expanded cells from fat cells
  let unwrap (FatCell fcells fhead _) = (fcells, [fhead])
      (cells, fatCellHeads)           = U.liftListTuple $ map unwrap fatCells
      isFatCellHead loc               = loc `elem` fatCellHeads
      locs                            = filter (not . isFatCellHead) $ map cellLocation cells
      checkCircular loc               = if (isFatCellHead loc) then (left $ CircularDepError loc) else (return ())
      isInMap idx                     = idx `M.notMember` valuesMap
  in do
    descLocs <- filter isInMap <$> G.getDescendants locs
    -- check for circular dependencies. IF a circular dependency exists, it necessarily has to
    -- involve one of the list heads, since the cells created as part of a list depend only
    -- on the head. So we go through the descendants of the current list cells (sans the previous
    -- list heads), so if those contain any of the previous list heads we know there's a cycle.
    mapM_ checkCircular descLocs
    -- DON'T need to re-eval anything that's already been evaluated
    -- #needsrefactor it seems like a large chunk of code here mirrors that in evalChain... should probably DRY
    ancLocs <- G.getImmediateAncestors descLocs
    newKnownValues <- lift $ getValuesMap conn ancLocs
    cells' <- getCellsToEval conn descLocs [] -- the origCells are the list cells, which got filtered out of descLocs
    evalChain' conn (M.union valuesMap newKnownValues) cells' fatCells

evalChain' conn valuesMap (c@(Cell loc xp _ ts):cs) fatCells = do
  cv <- EC.evaluateLanguage (cellLocation c) (locSheetId loc) valuesMap xp
  liftIO $ putStrLn $ "PYTHON EVAL GOT VALUE: " ++ (show cv) ++ " AT INDEX: " ++ (show loc)
  let maybeFatCell              = DE.decomposeCompositeValue c cv
      addCell (Cell l _ v _) mp = M.insert l (CellValue v) mp
      newValuesMap              = case maybeFatCell of
              Nothing -> if (M.member ptr valuesMap) 
                then M.insert ptr cv idxInserted
                else idxInserted
      -- ^^ when updating a location in the map, check if there are Pointer references to the same location.
      -- if so, update them too
                where 
                  ptr = indexToPointer loc
                  idxInserted = M.insert loc cv valuesMap
              Just (FatCell expandedCells _ _) -> foldr addCell valuesMap expandedCells
      -- ^ adds all the cells in cellsList to the reference map
      fatCells' = case maybeFatCell of
                Nothing -> fatCells
                Just f  -> f:fatCells
  (restCells, restFatCells) <- evalChain' conn newValuesMap cs fatCells'
  -- TODO investigate strictness here
  return $ case maybeFatCell of
    Nothing        -> let {(CellValue v) = cv} 
      in ((Cell loc xp v ts):restCells, restFatCells)
    (Just fatCell) -> (restCells, fatCell:restFatCells)

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
