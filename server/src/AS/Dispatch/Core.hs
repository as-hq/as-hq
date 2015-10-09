module AS.Dispatch.Core where

-- AlphaSheets and base
import AS.Types.Core
import Prelude 
import qualified AS.Eval.Core as R (evaluateLanguage)
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
    lift $ putStrLn $ "STARTING DISPATCH CYCLE WITH PAYLOADC " ++ (show c')
    c           <- lift $ EM.evalMiddleware c'
    conn        <- lift $ fmap dbConn $ readMVar state
    updateCells <- updateCell conn c
    desc        <- getDescendants conn c
    ancLocs     <- G.getImmediateAncestors $ map cellLocation desc
    showTime $ "got ancestor locs: " ++ (show ancLocs)
    anc         <- lift $ fmap catMaybes $ DB.getCells ancLocs
    cells'      <- propagate conn anc desc 
    -- Apply endware
    cells       <- lift $  EE.evalEndware state c' cells' uid
    let allCells = cells ++ updateCells
    lift $ DB.updateAfterEval conn uid c' desc allCells -- does set cells and commit
    return allCells
  return $ U.getCellMessage errOrCells

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval building blocks

-- | Takes a cell and returns an error if it tries to access a non-existent cell
-- Otherwise, it returns all of the immediate ancestors (used to make the lookup map)
-- returns cells that were created by side effects (e.g. list mutation)
-- NOTE: this is expensive when modifying a list.
-- TODO when modifying a list, asynchronously send the modified cell without blocking on all the other expressions being modified.
updateCell :: Connection -> ASCell -> EitherTExec [ASCell] 
updateCell conn (Cell loc xp val ts) = do 
  oldCell <- lift $ DB.getCell loc
  unlistCells <- case oldCell of 
    Just cell -> if (isListMember cell) 
      then lift $ DB.decoupleList conn cell
      else return []
    Nothing   -> return []
  let (deps, expr) = getDependenciesAndExpressions (locSheetId loc) xp
  ancCells <- lift $ DB.getCells deps
  if (any isNothing ancCells)
    then left $ DBNothingException []
    else do 
      let initCell = Cell loc expr NoValue ts
      G.setRelations [(loc, deps)]
      showTime $ "init cells: " ++ (show initCell)
      lift $ DB.setCell initCell
      showTime "set init cells"
      return unlistCells

-- | Return the descendants of a cell, which will always exist but may be locked
-- TODO: throw exceptions for permissions/locking
getDescendants :: Connection -> ASCell -> EitherTExec [ASCell]
getDescendants conn cell = do 
  let loc = cellLocation cell
  showTime $ "output 1: " ++ (show $ locSheetId loc)
  showTime $ "output 2: " ++ (show $ index loc)
  vLocs <- lift $ DB.getVolatileLocs conn
  showTime "got volatile locs"
  --Account for volatile cells being reevaluated each time
  indexes <- G.getDescendants (loc:vLocs) 
  desc <- lift $ DB.getCells indexes
  showTime $ "got descendant cells"
  return $ map fromJust desc

-- | Takes ancestors and descendants, create lookup map, and run eval
propagate :: Connection -> [ASCell] -> [ASCell] -> EitherTExec [ASCell]
propagate conn anc dec = do 
  let mp = M.fromList $ map (\c -> (IndexRef $ cellLocation c, cellValue c)) anc
  evalChain conn mp dec

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval helpers

evalChain :: Connection -> M.Map ASReference ASValue -> [ASCell] -> EitherTExec [ASCell]
evalChain _ _ [] = return []
evalChain conn mp (c@(Cell loc xp _ ts):cs) = do  
  showTime $ "Starting eval chain" -- ++ (show mp)
  cv <- R.evaluateLanguage xp loc mp
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

-- | If a cell C contains an array, createListCells makes the cells at C and below reflect the elements
-- of that array. 
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
