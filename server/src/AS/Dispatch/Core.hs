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
import Data.Maybe (fromJust, isNothing)
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

----------------------------------------------------------------------------------------------------------------------------------------------
-- Regular eval route

-- | Go through the regular eval route
runDispatchCycle :: MVar ServerState -> ASCell -> ASUserId -> IO ASServerMessage
runDispatchCycle state c' uid = do 
  -- Apply middlewares
  putStrLn $ "STARTING DISPATCH CYCLE WITH PAYLOADC: " ++ (show c')
  c <- EM.evalMiddleware c'
  conn <- fmap dbConn (readMVar state)
  update <- updateCell conn c 
  case update of 
    Left e -> return $ U.getCellMessage (Left e)
    Right updateCells -> do 
      d <- getDescendants conn c 
      case d of -- for example, error if DB is locked
        Left de -> return $ U.getCellMessage (Left de)
        Right desc -> do 
          ancResult <- G.getImmediateAncestors $ map cellLocation desc
          case ancResult of 
            (Left e') -> return $ U.getCellMessage (Left e') 
            (Right ancLocs) -> do
              printTimed $ "got ancestor locs: " ++ (show ancLocs)
              anc <- fmap U.fromJustList $ DB.getCells ancLocs
              res <- propagate conn anc desc 
              case res of 
                Left e' -> return $ U.getCellMessage (Left e')
                Right cells' -> do
                  -- Apply endwares
                  cells <- EE.evalEndware state c' cells' uid
                  let allCells = cells ++ updateCells
                  DB.updateAfterEval conn uid c' desc allCells -- does set allCells and commit
                  return $ U.getCellMessage (Right allCells) -- reply message

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval building blocks

-- | Takes a cell and returns an error if it tries to access a non-existent cell
-- Otherwise, it returns all of the immediate ancestors (used to make the lookup map)
-- returns cells that were created by side effects (e.g. list mutation)
updateCell :: Connection -> ASCell -> IO (Either ASExecError [ASCell])
updateCell conn (Cell loc xp val ts) = do 
  oldCell <- DB.getCell loc
  unlistCells <- case oldCell of 
    Just cell -> if (isList cell) 
      then DB.decoupleList conn loc
      else return []
    Nothing   -> return []
  let (deps, expr) = getDependenciesAndExpressions (locSheetId loc) xp
  ancCells <- DB.getCells deps
  printTimed $ "got cells: "
  if (any isNothing ancCells)
    then return $ Left (DBNothingException [])
    else do 
      let initCell = Cell loc expr NoValue ts
      setResult <- G.setRelations [(loc, deps)]
      printTimed $ "init cells: " ++ (show initCell)
      DB.setCell initCell
      printTimed $ "set init cells"
      return $ case setResult of 
        (Right ()) -> Right unlistCells
        (Left e) -> Left e 

-- | Return the descendants of a cell, which will always exist but may be locked
-- TODO: throw exceptions for permissions/locking
getDescendants :: Connection -> ASCell -> IO (Either ASExecError [ASCell])
getDescendants conn cell = do 
  let loc = cellLocation cell
  printTimed $ "output 1: " ++ (show $ locSheetId loc)
  printTimed $ "output 2: " ++ (show $ index loc)
  --dag <- DB.getDAG conn
  --printTimed "got dag"
  vLocs <- DB.getVolatileLocs conn
  printTimed "got volatile locs"
 --Account for volatile cells being reevaluated each time
  graphResult <- G.getDescendants (loc:vLocs) 
  --let descendantLocs = DAG.descendants (locs ++ vLocs) dag
  --desc <- DB.getCells conn descendantLocs
  --let graphResult = Right descendantLocs
  case graphResult of
    (Right descendantLocs) -> do
      desc <- DB.getCells descendantLocs
      printTimed $ "got descendant cells: " -- ++ (show desc)
      return . Right $ map fromJust desc 
    (Left e) -> return $ Left e

-- | Takes ancestors and descendants, create lookup map, and run eval
propagate :: Connection -> [ASCell] -> [ASCell] -> IO (Either ASExecError [ASCell])
propagate conn anc dec = do 
  let mp = M.fromList $ map (\c -> (IndexRef $ cellLocation c, cellValue c)) anc
  evalChain conn mp dec

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval helpers

evalChain :: Connection -> M.Map ASReference ASValue -> [ASCell] -> IO (Either ASExecError [ASCell])
evalChain _ _ [] = return $ Right []
evalChain conn mp (c@(Cell loc xp _ ts):cs) = do  
  printTimed $ "Starting eval chain" -- ++ (show mp)
  evalResult <- R.evaluateLanguage xp loc mp
  case evalResult of 
    (Left e) -> return $ Left e
    (Right (ValueL lst)) -> do
      let listCells = createListCells conn c lst
          newMp     = M.insert (IndexRef loc) (head lst) mp
      rest <- evalChain conn newMp cs
      return $ case rest of 
        (Left e) -> Left e
        (Right moreCells) -> Right $ listCells ++ moreCells
    (Right cv) -> do
      let newMp = M.insert (IndexRef loc) cv mp
      rest <- evalChain conn newMp cs
      return $ case rest of 
        (Left e) -> Left e
        (Right moreCells) -> Right $ (Cell loc xp cv ts):moreCells


-- | Create a list of cells, also modify the DB for references 
-- Not currently handling [[[]]] type things
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