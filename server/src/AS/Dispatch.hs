module AS.Dispatch where

-- AlphaSheets and base
import AS.Types
import Prelude 
import qualified AS.Eval    as R (evalExpression,evalExcel)
import qualified Data.Map   as M
import qualified AS.DAG     as DAG
import qualified AS.DB.API  as DB
import qualified Data.List  as L (head,last,tail,length,splitAt) 
import AS.Parsing.Common
import AS.Parsing.Out hiding (first)
import AS.Parsing.In
import Data.Maybe (fromJust, isNothing)
import Text.ParserCombinators.Parsec
import Control.Applicative
import Data.Time.Clock
import Data.Text as T (unpack,pack)
import AS.Util as U
import AS.Eval.Middleware as EM
import AS.Eval.Endware as EE

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


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Eval building blocks

-- | Takes a cell and returns an error if it tries to access a non-existant cell
-- | Otherwise, it returns all of the immediate ancestors (used to make the lookup map)
updateCell :: Connection -> ASCell -> IO (Either ASExecError [ASCell])
updateCell conn (Cell loc xp val ts) = do 
  let locs = decomposeLocs loc
  let (deps,exprs) = getDependenciesAndExpressions (locSheetId loc) xp (getOffsets loc)
  ancCells <- DB.getCells conn (concat deps)
  printTimed $ "got cells"
  if (any isNothing ancCells)
    then do 
      let bl = map snd $ filter (\(a,b) -> (a == Nothing)) (zip ancCells (concat deps))
      return $ Left (DBNothingException bl)
    else do 
      let initCells = map (\(l,e,v)-> Cell l e v ts) (zip3 locs exprs (repeat NoValue))  
      _ <- DB.updateDAG conn (zip deps locs)
      _ <- DB.setCells conn initCells
      printTimed $ "set init cells"
      return $ Right (map fromJust ancCells)

-- | Return the descendants of a cell, which will always exist but may be locked
-- | TODO: throw exceptions for permissions/locking
getDescendants :: Connection -> ASCell -> IO (Either ASExecError [ASCell])
getDescendants conn cell = do 
  let locs = decomposeLocs (cellLocation cell)
  dag <- DB.getDAG conn
  printTimed "got dag"
  vLocs <- DB.getVolatileLocs conn
  printTimed "got volatile locs"
   --| Account for volatile cells being reevaluated each time
  let descendantLocs = DAG.descendants (locs ++ vLocs) dag
  printTimed $ "got descendant locs: " ++ (show descendantLocs)
  desc <- DB.getCells conn locs
  printTimed $ "got descendant cells"
  return $ Right $ map fromJust desc 

-- | Takes ancestors and descendants, create lookup map, and run eval
reEvalCell :: Connection -> [ASCell] -> [ASCell] -> IO (Either ASExecError [ASCell])
reEvalCell conn anc dec = do 
  let mp = M.fromList $ map (\c -> (cellLocation c, cellValue c)) anc
  result <- evalChain conn mp dec
  return $ Right result

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Eval helpers

evalChain :: Connection -> M.Map ASLocation ASValue -> [ASCell] -> IO [ASCell]
evalChain _ _ [] = return []
evalChain conn mp ((Cell loc xp _ ts):cs) = do  
  printTimed $ "Starting eval chain" ++ (show mp)
  cv <- R.evalExpression loc mp xp 
  otherCells <- case loc of
    Index sheet (a, b) -> case cv of
      ValueL lstValues -> createListCells conn (Index sheet (a, b)) lstValues
      otherwise -> return [] 
    otherwise -> return []
  let newMp = M.insert loc cv mp
  rest <- evalChain conn newMp cs
  return $ [Cell loc xp cv ts] ++ otherCells ++ rest 


-- | Create a list of cells, also modify the DB for references 
-- | Not currently handling [[[]]] type things
createListCells :: Connection -> ASLocation -> [ASValue] -> IO [ASCell]
createListCells conn (Index sheet (a,b)) [] = return []
createListCells conn (Index sheet (a,b)) values = do 
  let origLoc = Index sheet (a,b)
  let vals = concat $ map lst values
  let locs = map (Index sheet) (concat $ [(shift (values!!row) row (a,b)) | row <- [0..(length values)-1]])
  let exprs = map (\(Index _ (x,y)) -> Reference origLoc (x-a,y-b)) locs
  let cells = L.tail $ map (\(l,e,v) -> Cell l e v []) (zip3 locs exprs vals)
  DB.updateDAG conn (zip (repeat [origLoc]) (L.tail locs))
  return cells
    where
      shift (ValueL v) r (a,b) = [(a+c,b+r) | c<-[0..length(v)-1] ]
      shift other r (a,b)  = [(a,b+r)]

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Regular eval route

-- | Go through the regular eval route
runDispatchCycle :: ASUser -> MVar ServerState -> ASMessage -> IO ASMessage
runDispatchCycle user state msg@(Message _ _ _ (PayloadC c')) = do 
  -- Apply middlewares
  putStrLn $ "STARTING DISPATCH CYCLE " ++ (show c')
  c <- EM.evalMiddleware c'
  conn <- fmap dbConn (readMVar state)
  update <- updateCell conn c 
  case update of 
    Left e -> return $ U.getCellMessage user (Left e)
    Right anc -> do 
      d <- getDescendants conn c 
      case d of -- for example, error if DB is locked
        Left de -> return $  U.getCellMessage user (Left de)
        Right desc -> do 
          res <- reEvalCell conn anc desc 
          case res of 
            Left e' -> return $ U.getCellMessage user (Left e')
            Right cells' -> do
              -- Apply endwares
              cells <- (EE.evalEndware user state msg) cells'
              DB.updateAfterEval conn user c' desc cells -- does set cells and commit
              return $ U.getCellMessage user (Right cells)

----------------------------------------------------------------------------------------------------------------------------------------------

 

