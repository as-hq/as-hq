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

runDispatchCycle :: MVar ServerState -> ASCell -> ASUserId -> IO ASServerMessage
runDispatchCycle state c' uid = do
  errOrCells <- runEitherT $ do
    printWithTimeT $ "STARTING DISPATCH CYCLE WITH PAYLOADC " ++ (show c')
    c              <- lift $ EM.evalMiddleware c'
    conn           <- lift $ fmap dbConn $ readMVar state
    setCellInDb conn c
    setCellAncestorsInDb conn c
    decoupledCells <- decoupleCells conn c
    desc           <- getDescendants conn c
    ancLocs        <- G.getImmediateAncestors $ map cellLocation desc
    printWithTimeT $ "got ancestor locs: " ++ (show ancLocs)
    anc            <- lift $ fmap catMaybes $ DB.getCells ancLocs
    cells          <- initEval conn anc desc 

    -- Apply endware
    finalizedCells <- lift $ EE.evalEndware state c' cells uid
    let allCells = finalizedCells ++ decoupledCells
    lift $ DB.updateAfterEval conn uid c' desc allCells -- does set cells and commit
    return allCells
  return $ U.getCellMessage errOrCells

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval building blocks

-- | Puts the cell in the database for the first time / overwrites cell in DB with fresh new cell. 
setCellInDb :: Connection -> ASCell -> EitherTExec ()
setCellInDb conn (Cell loc expr _ ts) = do 
  let initCell = Cell loc expr NoValue ts -- NoValue because the value hasn't been computed yet
  lift $ DB.setCell initCell
  printWithTimeT "set init cells"

-- | Get the ancestors of a cell, and set the ancestor relationships in the DB. 
setCellAncestorsInDb :: Connection -> ASCell -> EitherTExec ()
setCellAncestorsInDb conn (Cell loc expr _ ts) = do
  let deps = fst $ getDependenciesAndExpressions (locSheetId loc) expr
  ancestorCells <- lift $ DB.getCells deps
  printWithTimeT "got ancestor cells"
  if (all isJust ancestorCells)
    then G.setRelations [(loc, deps)]
    else left $ DBNothingException [] -- one of the ancestors doesn't exist. TODO: return list of missing ancestors.

-- | If a cell is not a part of any list or reference, do nothing and return nothing. Otherwise, 
-- decouple all the cells associated with that cell and return the list of decoupled cells. 
decoupleCells :: Connection -> ASCell -> EitherTExec [ASCell] 
decoupleCells conn cell = if (isListMember cell) 
  then lift $ DB.decoupleList conn cell
  else return []

-- | Return the descendants of a cell, which will always exist but may be locked
-- TODO: throw exceptions for permissions/locking
getDescendants :: Connection -> ASCell -> EitherTExec [ASCell]
getDescendants conn cell = do 
  let loc = cellLocation cell
  printWithTimeT $ "output 1: " ++ (show $ locSheetId loc)
  printWithTimeT $ "output 2: " ++ (show $ index loc)
  vLocs <- lift $ DB.getVolatileLocs conn
  printWithTimeT "got volatile locs"
  --Account for volatile cells being reevaluated each time
  indexes <- G.getDescendants (loc:vLocs) 
  desc <- lift $ DB.getCells indexes
  printWithTimeT $ "got descendant cells"
  return $ map fromJust desc

-- | Takes ancestors and descendants, create lookup map, and starts eval
initEval :: Connection -> [ASCell] -> [ASCell] -> EitherTExec [ASCell]
initEval conn anc dec = do 
  let mp = M.fromList $ map (\c -> (IndexRef $ cellLocation c, cellValue c)) anc
  evalChain conn mp dec

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval helpers

-- | Evaluates expression in current cell (first element of 3rd argument). Once that's done, continue doing
-- this down the list of cells. The list of cells is guaranteed to be be topologically sorted -- by the time
-- we reach any cell, all references to any of its ancestors should already have been computed
-- and stored in the second argument. Returns the list of all the cells it's evaluated  
evalChain :: Connection -> RefValMap -> [ASCell] -> EitherTExec [ASCell]
evalChain _ _ [] = return []
evalChain conn mp (c@(Cell loc xp _ ts):cs) = do  
  printWithTimeT "Starting eval chain" -- ++ (show mp)
  cv <- R.evalCode (locSheetId loc) mp xp
  case cv of 
    ValueL lst -> do
      let listCells = createListCells conn c lst
          newMp     = M.insert (IndexRef loc) (head lst) mp
      lift $ DB.setList conn $ map cellLocation listCells
      rest <- evalChain conn newMp cs
      return $ listCells ++ rest
    _ -> do
      let newMp = M.insert (IndexRef loc) cv mp
      rest <- evalChain conn newMp cs
      return $ (Cell loc xp cv ts):rest

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
