module AS.Dispatch.Core where

-- AlphaSheets and base
import AS.Types.Core
import Prelude 
import qualified AS.Eval.Core as R (evalCode)
import qualified Data.Map   as M
import qualified AS.DB.API  as DB
import qualified AS.DB.Util as DU
import qualified Data.List  as L (head,last,tail,length,splitAt) 
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

----------------------------------------------------------------------------------------------------------------------------------------------
-- Regular eval route

runDispatchCycle :: MVar ServerState -> [ASCell] -> ASUserId -> IO ASServerMessage
runDispatchCycle state cs uid = do
  errOrCells <- runEitherT $ do
    printWithTimeT $ "STARTING DISPATCH CYCLE WITH CELLS: " ++ (show cs)
    cs'            <- lift $ EM.evalMiddleware cs
    conn           <- lift $ fmap dbConn $ readMVar state
    decoupledCells <- decoupleCells conn cs' 
    desc <- getDescendants conn cs'
    ancLocs <- getNewAncLocs conn cs' desc 
    printWithTimeT $ "got ancestor locs: " ++ (show ancLocs)
    initValuesMap  <- lift $ getValuesMap ancLocs
    cs''           <- evalChain conn initValuesMap (cs' ++ desc) -- start with current cells, then go through descendants
    -- Apply endware
    finalizedCells <- lift $ EE.evalEndware state cs'' uid cs
    let allCells = decoupledCells ++ finalizedCells -- ORDER IS IMPORTANT, since decoupledCells and finalizedCells might overlap. Further right in list = higher precedence
    lift $ DB.updateAfterEval conn uid cs desc allCells -- does set cells and commit (should rename)
    setCellsAncestorsInDb conn cs' -- move to upateAfterEval, maybe? 
    return allCells
  return $ U.getCellMessage errOrCells

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval building blocks

-- | If a cell is not a part of any list or reference, do nothing and return nothing. Otherwise, 
-- decouple all the cells associated with that cell and return the list of decoupled cells. 
decoupleCells :: Connection -> [ASCell] -> EitherTExec [ASCell] 
decoupleCells conn cells = do 
  oldCells <- lift $ DB.getCells (map cellLocation cells)
  lift $ (fmap concat) $ (flip mapM) oldCells (\oldCell -> case oldCell of 
    Just cell' -> if (isListMember cell') 
      then DB.decoupleList conn cell'
      else return []
    _ -> return [])

-- for some very strange reason, it sometimes happens that a cell is tagged as a part of the list in the
-- backend, but the cell that gets passed to us from the frontend doesn't have that. this is why
-- we need to pull oldCell to make this work right now. Otherwise we can just use the below:  
-- decoupleCells conn cell = if (isListMember cell) 
--   then lift $ DB.decoupleList conn cell
--   else return []

-- | Given a set of cells and their descendants, return the new set of cell locations they will
-- depend on for evaluation. 
getNewAncLocs :: Connection -> [ASCell] -> [ASCell] -> EitherTExec [ASIndex]
getNewAncLocs conn curCells desc = do 
  descAncLocs <- G.getImmediateAncestors $ map cellLocation desc
  let curCellsDeps = map (\(Cell loc expr _ _) -> getDependencies (locSheetId loc) expr) curCells
  return $ descAncLocs ++ (concat curCellsDeps)

-- | Update the ancestors of a cell, and set the ancestor relationships in the DB. 
setCellsAncestorsInDb :: Connection -> [ASCell] -> EitherTExec ()
setCellsAncestorsInDb conn cells = (flip mapM_) cells (\(Cell loc expr _ ts) -> do
  let deps = getDependencies (locSheetId loc) expr
  G.setRelations [(loc, deps)])

-- | Return the descendants of a cell, which will always exist but may be locked
-- TODO: throw exceptions for permissions/locking
getDescendants :: Connection -> [ASCell] -> EitherTExec [ASCell]
getDescendants conn cells = do 
  let locs = map cellLocation cells
  vLocs <- lift $ DB.getVolatileLocs conn -- Accounts for volatile cells being reevaluated each time
  indexes <- G.getDescendants (locs ++ vLocs) 
  let indexes' = minus indexes (map cellLocation cells)
  desc <- lift $ DB.getCells indexes'
  -- lift $ printDebug "locs" locs
  -- lift $ printDebug "cells" cells
  -- lift $ printDebug "indexes" indexes
  -- lift $ printDebug "vLocs" vLocs 
  printWithTimeT $ "got descendant cells"
  return $ map fromJust desc

-- temporary, until getDescendants is re-implemented to not include the current cell as a descendant. 
-- ALSO, this is currently wrong. (if you pass in a list [C1, C2] and C2 is a descendant of C1, 
-- the list of descendants returned should be all descendnats of C1 sans C1, but not it'll be missing
-- C2 as well) Will not be an issue when getDescendants is reimplemented. (Alex 10/12)
minus :: (Eq a) => [a] -> [a] -> [a]
minus [] xs                      = []
minus (y:ys) xs | y `notElem` xs = y : (minus ys xs)
                | otherwise      = minus ys xs

-- | Returns a map that sends locations to the value corresponding to that location in the DB. 
getValuesMap :: [ASIndex] -> IO RefValMap
getValuesMap locs = do 
  maybeCells <- DB.getCells locs
  return $ M.fromList $ zip (map IndexRef locs) (map (\mc -> case mc of
    Just c  -> cellValue c
    Nothing -> NoValue) maybeCells)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval helpers

-- | Evaluates a list of cells, in serial order, updating the reference/value map with each 
-- cell that's updated. The cells passed in are guaranteed to be topologically sorted, i.e., 
-- if a cell references an ancestor, that ancestor is guaranteed to already have been 
-- added in the map. 
evalChain :: Connection -> RefValMap -> [ASCell] -> EitherTExec [ASCell]
evalChain _ _ [] = return []
evalChain conn valuesMap (c@(Cell loc xp _ ts):cs) = do  
  printWithTimeT "Starting eval chain"
  cv <- R.evalCode (locSheetId loc) valuesMap xp
  case cv of 
    ValueL lst -> do
      let listCells    = createListCells conn c lst
          newValuesMap = M.insert (IndexRef loc) (head lst) valuesMap
      lift $ DB.setList conn $ map cellLocation listCells
      rest <- evalChain conn newValuesMap cs
      return $ listCells ++ rest
    _ -> do
      let newValuesMap = M.insert (IndexRef loc) cv valuesMap
      rest <- evalChain conn newValuesMap cs
      return $ (Cell loc xp cv ts):rest
-- The Haskell way is probably to write this using foldM somehow, but that's not very urgent. 

-- | If a cell C contains a 1D or 2D list, it'll be represented in the grid as a matrix. 
-- This function takes in the starting cell with the starting expression, and creates the list 
-- of cells. For example, [[1,2],[3,4]] entered into A1 should put values of 1 in A1, 2 in B1, 
-- 3 in A2, and 4 in B2. createListCells called on A1 with the expression [[1,2],[3,4]] in it
-- should return a list of cells located at A1, A2, B1, B2, with:
--   1) the values 1, 2, 3, and 4, in them,
--   2) references to the original cell being called
--   3) the expression [[1,2],[3,4]] stored in them. 
createListCells :: Connection -> ASCell -> [ASValue] -> [ASCell]
createListCells _ _ [] = []
createListCells conn (Cell (Index sheet (a,b)) xp _ ts) values = cells
  where
    origLoc   = Index sheet (a,b)
    vals      = concat $ map toList values
    zipVals   = zip [0..] values
    locs      = map (Index sheet) (concat $ map (\(row, val) -> shift val row (a,b)) zipVals)
    listKey   = DU.getListKey origLoc
    cells     = map (\(loc, val) -> Cell loc xp val [ListMember listKey]) $ zip locs vals

    shift (ValueL v) r (a,b)  = [(a+c,b+r) | c<-[0..length(v)-1] ]
    shift other r (a,b)       = [(a,b+r)]
