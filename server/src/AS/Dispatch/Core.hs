module AS.Dispatch.Core where

-- AlphaSheets and base
import AS.Types.Core
import Prelude 
import qualified AS.Eval.Core as R (evaluateLanguage)
import qualified Data.Map   as M
import qualified AS.DB.API  as DB
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
    c <- lift $ EM.evalMiddleware c'
    conn <- lift $ fmap dbConn $ readMVar state
    update <- updateCell c
    desc <- getDescendants conn c
    ancLocs <- G.getImmediateAncestors $ map cellLocation desc
    showTime $ "got ancestor locs: " ++ (show ancLocs)
    anc <- lift $ fmap catMaybes $ DB.getCells ancLocs
    cells' <- propagate conn anc desc 
    -- Apply endware
    cells <- lift $  EE.evalEndware state c' cells' uid
    lift $ DB.updateAfterEval conn uid c' desc cells -- does set cells and commit
    return cells
  return $ U.getCellMessage errOrCells

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval building blocks

-- | Takes a cell and returns an error if it tries to access a non-existent cell
-- Otherwise, it returns all of the immediate ancestors (used to make the lookup map)
updateCell :: ASCell -> EitherTExec ()
updateCell (Cell loc xp val ts) = do 
  let (deps, expr) = getDependenciesAndExpressions (locSheetId loc) xp
  ancCells <- lift $ DB.getCells deps
  showTime $ "got cells: "
  if (any isNothing ancCells)
    then left $ DBNothingException []
    else do 
      let initCell = Cell loc expr NoValue ts
      G.setRelations [(loc, deps)]
      showTime $ "init cells: " ++ (show initCell)
      lift $ DB.setCell initCell
      showTime "set init cells"

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
evalChain conn mp ((Cell loc xp _ ts):cs) = do  
  showTime $ "Starting eval chain" 
  cv <- R.evaluateLanguage xp loc mp
  otherCells <- case loc of
    Index sheet (a, b) -> case cv of
      ValueL lstValues -> lift $ createListCells conn (Index sheet (a, b)) lstValues
      otherwise -> return [] 
    otherwise -> return []
  let newMp = M.insert (IndexRef loc) cv mp
  rest <- evalChain conn newMp cs
  return $ (Cell loc xp cv ts):(otherCells ++ rest)


-- | Create a list of cells, also modify the DB for references 
-- Not currently handling [[[]]] type things
createListCells :: Connection -> ASIndex -> [ASValue] -> IO [ASCell]
createListCells conn (Index sheet (a,b)) [] = return []
createListCells conn (Index sheet (a,b)) values = 
  let 
    origLoc = Index sheet (a,b)
    vals = concat $ map lst values
    locs = map (Index sheet) (concat $ [(shift (values!!row) row (a,b)) | row <- [0..(length values)-1]])
    exprs = map (\(Index _ (x,y)) -> Reference (IndexRef origLoc) (x-a,y-b)) locs
    cells = L.tail $ map (\(l,e,v) -> Cell l e v []) (zip3 locs exprs vals)
    shift (ValueL v) r (a,b) = [(a+c,b+r) | c<-[0..length(v)-1] ]
    shift other r (a,b)  = [(a,b+r)]
  in do
    runEitherT $ G.setRelations (zip (L.tail locs) (repeat [origLoc]))
    return cells
