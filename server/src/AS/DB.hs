module AS.DB where

import AS.Types	hiding (location,expression,value)
import Data.Maybe (isNothing, fromJust)
import Prelude
import AS.Util as U

import Data.List (zip4,head)

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Time
import Database.Redis hiding (decode, Message)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson hiding (Success)
import qualified Data.ByteString.Char8 as B 
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List.Split

-- | Haskell Redis connection object
cInfo :: ConnectInfo
cInfo = ConnInfo
    { connectHost           = "localhost"
    , connectPort           = PortNumber 6379
    , connectAuth           = Nothing
    , connectDatabase       = 0               
    , connectMaxConnections = 100
    , connectMaxIdleTime    = 100
    }

----------------------------------------------------------------------------------------------------------------------
-- | ByteString and Maybe conversions  

bStrToASExpression :: Maybe B.ByteString -> Maybe ASExpression
bStrToASExpression (Just b) = Just (read (B.unpack b) :: ASExpression)
bStrToASExpression Nothing = Nothing

bStrToASValue :: Maybe B.ByteString -> Maybe ASValue
bStrToASValue (Just b) = Just (read (B.unpack b) :: ASValue)
bStrToASValue Nothing = Nothing     

bStrToTags :: Maybe B.ByteString -> Maybe [ASCellTag]
bStrToTags (Just b) = Just (read (B.unpack b) :: [ASCellTag])
bStrToTags Nothing = Nothing 

maybeASCell :: (ASLocation,Maybe ASExpression,Maybe ASValue, Maybe [ASCellTag]) -> Maybe ASCell
maybeASCell (l, Just e, Just v, Just tags) = Just $ Cell l e v tags
maybeASCell _ = Nothing

bStrToASLocation :: B.ByteString -> ASLocation
bStrToASLocation b = (read (B.unpack b) :: ASLocation)

bStrToRelation :: (B.ByteString,B.ByteString) -> (ASLocation,ASLocation)
bStrToRelation (r,s) = (bStrToASLocation r, bStrToASLocation s)

bStrToASCommit :: Maybe B.ByteString -> Maybe ASCommit 
bStrToASCommit (Just b) = Just (read (B.unpack b) :: ASCommit)
bStrToASCommit Nothing = Nothing

----------------------------------------------------------------------------------------------------------------------
-- | Chunking methods

cellChunkSize :: Int
cellChunkSize = 1000

genericChunkSize :: Int
genericChunkSize = 1000

chunkM :: ([a] -> Redis [b]) -> (Int) -> [a] -> IO [b]
chunkM f size lst = do 
  let chunks = chunksOf size lst
  conn <- connect cInfo 
  runRedis conn $ do
    res <- mapM f chunks
    return $ concat res

chunkM_ :: ([a] -> Redis ()) -> (Int) -> [a] -> IO ()
chunkM_ f size lst = do 
  let chunks = chunksOf size lst
  conn <- connect cInfo 
  runRedis conn $ mapM_ f chunks

----------------------------------------------------------------------------------------------------------------------
-- | DB Cell, location methods

getCell :: ASLocation -> IO (Maybe ASCell)
getCell loc = return . head =<< getCells [loc]

getCells :: [ASLocation] -> IO [Maybe ASCell]
getCells [] = return []
getCells locs = chunkM getChunkCells cellChunkSize degenerateLocs
  where degenerateLocs = concat $ map U.decomposeLocs locs

setCell :: ASCell -> IO ()
setCell c = setCells [c]

setCells :: [ASCell] -> IO ()
setCells [] = return ()
setCells cells = (chunkM_ setChunkCells cellChunkSize cells) >> (chunkM_ setChunkVolatileCells cellChunkSize cells)

deleteCells :: [ASCell] -> IO ()
deleteCells [] = return ()
deleteCells cells = (chunkM_ deleteChunkCells cellChunkSize cells) >> (chunkM_ deleteChunkVolatileCells cellChunkSize cells)

deleteLocs :: [ASLocation] -> IO ()
deleteLocs [] = return ()
deleteLocs locs = chunkM_ deleteChunkLocs genericChunkSize degenerateLocs
  where degenerateLocs = concat $ map U.decomposeLocs locs
----------------------------------------------------------------------------------------------------------------------
-- | DB Sheet, workbook

createSheet :: ASSheet -> IO ASSheet
createSheet (Sheet _ name permissions) = do
  sheetid <- U.getUniqueId
  let sheet = Sheet sheetid name permissions
  -- TODO insert sheet in DB
  return sheet

createWorkbook :: ASWorkbook -> IO ()
createWorkbook workbook = return () -- TODO

getSheet :: ASSheetId -> IO (Maybe ASSheet)
getSheet sheet = return . head =<< getSheets [sheet] 

getSheets :: [ASSheetId] -> IO [Maybe ASSheet]
getSheets sheets = return [] -- TODO

getAllSheets :: IO [ASSheet]
getAllSheets = return [] -- TODO

getWorkbook :: String -> IO (Maybe ASWorkbook)
getWorkbook name = return . head =<< getWorkbooks [name]

getWorkbooks :: [String] -> IO [Maybe ASWorkbook] 
getWorkbooks names = return [] -- TODO

getAllWorkbooks :: IO [ASWorkbook]
getAllWorkbooks = return [] -- TODO

deleteSheet :: ASSheetId -> IO ()
deleteSheet sheet = return () -- TODO all cells in sheet, then delete sheet, then remove sheet from workbooks

deleteWorkbook :: String -> IO ()
deleteWorkbook name = return () -- TODO delete just the workbook, leave sheets and cells intact

----------------------------------------------------------------------------------------------------------------------
-- | DB permissions

canAccessSheet :: ASUserId -> ASSheetId -> IO Bool
canAccessSheet uid sheetId = do
  sheet <- getSheet sheetId
  case sheet of 
    Nothing -> return False
    (Just someSheet) -> return $ hasPermissions uid (sheetPermissions someSheet)

canAccess :: ASUserId -> ASLocation -> IO Bool
canAccess uid loc = canAccessSheet uid (locSheetId loc)

canAccessAll :: ASUserId -> [ASLocation] -> IO Bool
canAccessAll uid locs = return . all id =<< mapM (canAccess uid) locs

isPermissibleMessage :: ASUserId -> ASMessage -> IO Bool
isPermissibleMessage uid (Message _ _ _ (PayloadC cell))      = canAccess uid (cellLocation cell)
isPermissibleMessage uid (Message _ _ _ (PayloadCL cells))    = canAccessAll uid (map cellLocation cells)
isPermissibleMessage uid (Message _ _ _ (PayloadL loc))       = canAccess uid loc
isPermissibleMessage uid (Message _ _ _ (PayloadLL locs))     = canAccessAll uid locs
isPermissibleMessage uid (Message _ _ _ (PayloadS sheet))     = canAccessSheet uid (sheetId sheet)
isPermissibleMessage uid (Message _ _ _ (PayloadW window))    = canAccessSheet uid (windowSheetId window)
isPermissibleMessage uid (Message _ _ _ (PayloadTags _ loc))  = canAccess uid loc
isPermissibleMessage _ _ = return True

----------------------------------------------------------------------------------------------------------------------
-- | DB Cell Chunking methods

getChunkCells :: [ASLocation] -> Redis [Maybe ASCell]
getChunkCells locs = do 
  TxSuccess ((justExps, justVals), justTags) <- multiExec $ do 
    let locStrs = map (B.pack . show) locs
    xp <- hmget (B.pack "exp") locStrs
    vs <- hmget (B.pack "val") locStrs
    ts <- hmget (B.pack "tags") locStrs
    let fstTuple = (,) <$> xp <*> vs
    return $ (,) <$> fstTuple <*> ts 
  let exps = map bStrToASExpression justExps
  let vals = map bStrToASValue justVals
  let tags = map bStrToTags justTags
  return $ map maybeASCell (zip4 locs exps vals tags)

setChunkCells :: [ASCell] -> Redis ()
setChunkCells cells = do 
  let expStrs = map (B.pack . show . cellExpression) cells
  let valStrs = map (B.pack . show . cellValue) cells
  let locStrs = map (B.pack . show . cellLocation) cells
  let tagStrs = map (B.pack . show . cellTags) cells
  TxSuccess _ <- multiExec $ do 
      s1 <- hmset (B.pack "exp") (zip locStrs expStrs)
      s2 <- hmset (B.pack "val") (zip locStrs valStrs)
      s3 <- hmset (B.pack "tags") (zip locStrs tagStrs)
      return $ (,) <$> s1 <*> s2 
  return ()

deleteChunkCells :: [ASCell] -> Redis ()
deleteChunkCells cells = do 
  let locStrs = map (B.pack . show . cellLocation) cells
  TxSuccess _ <- multiExec $ do 
      s1 <- hdel (B.pack "exp") locStrs
      s2 <- hdel (B.pack "val") locStrs
      s3 <- hdel (B.pack "tags") locStrs
      return $ (,) <$> s1 <*> s2 
  return ()

deleteChunkLocs :: [ASLocation] -> Redis ()
deleteChunkLocs locs = do 
  let locStrs = map (B.pack . show) locs
  TxSuccess _ <- multiExec $ do 
      s1 <- hdel (B.pack "exp") locStrs
      s2 <- hdel (B.pack "val") locStrs
      s3 <- hdel (B.pack "tags") locStrs
      return $ (,) <$> s1 <*> s2 
  return ()

----------------------------------------------------------------------------------------------------------------------
-- | Volatile cell methods

getVolatileLocs :: IO [ASLocation]
getVolatileLocs = do 
  conn <- connect cInfo
  runRedis conn $ do
      Right vl <- smembers (B.pack "volLocs")
      return $ map bStrToASLocation vl

-- TODO: some of the cells may change from volatile -> not volatile, but they're still in volLocs
setChunkVolatileCells :: [ASCell] -> Redis ()
setChunkVolatileCells cells = do 
  let vLocs = map cellLocation $ filter (U.hasVolatileTag) cells
  let locStrs = map (B.pack . show) vLocs
  sadd "volLocs" locStrs
  return ()

deleteChunkVolatileCells :: [ASCell] -> Redis ()
deleteChunkVolatileCells cells = do 
  let vLocs = map cellLocation $ filter (U.hasVolatileTag) cells
  let locStrs = map (B.pack . show) vLocs
  srem "volLocs" locStrs
  return ()

----------------------------------------------------------------------------------------------------------------------
-- | DB Edge methods

dagChunkSize :: Int
dagChunkSize = 1000

updateDAG :: [([ASLocation],ASLocation)] -> IO ()
updateDAG [] = return ()
updateDAG rels = chunkM_ updateChunkDAG dagChunkSize rels

updateChunkDAG :: [([ASLocation],ASLocation)] -> Redis ()
updateChunkDAG rels = do 
  let update = filter (\(a,b) -> (a /= [])) $ map (\(a,b) -> (map (B.pack . show) a, (B.pack . show) b)) rels
  if (update == [])
    then return ()
    else do 
      TxSuccess _ <- multiExec $ do 
          s1 <- mapM (\(a,b) -> sadd b a) update
          s2 <- sadd (B.pack "toLocSet") (map snd update)
          return s2
      return ()

getDAG :: IO [(ASLocation,ASLocation)]
getDAG = do 
  conn <- connect cInfo
  runRedis conn $ do
      Right tl <- smembers (B.pack "toLocSet")
      TxSuccess fromLocs <- multiExec $ do 
          fl' <- mapM (\t -> (smembers t)) tl -- because Queued is a monad
          return $ sequence fl'
      let rels' = concat $ map (\(a,b) -> (zip a (repeat b))) (zip fromLocs tl)
      let rels = map bStrToRelation rels'
      return rels

----------------------------------------------------------------------------------------------------------------------
-- | DB Commit methods

-- | TODO: need to deal with large commit sizes and max number of commits

pushCommit :: ASCommit -> IO ()
pushCommit c = do 
  let commit = (B.pack . show) c 
  conn <- connect cInfo
  runRedis conn $ do
    TxSuccess _ <- multiExec $ do 
      rpush (B.pack "commits1") [commit]
      numCommits <- get (B.pack "numCommits")
      incrbyfloat (B.pack "numCommits") 1
      return numCommits
    return ()

-- | Return a commit if possible (not possible if you undo past the beginning of time, etc)
-- | Update the DB so that there's always a source of truth (ie we will propagate undo to all relevant users)
undo :: IO (Maybe ASCommit)
undo = do 
  conn <- connect cInfo
  commit <- runRedis conn $ do 
    TxSuccess justC <- multiExec $ do 
      commit <- rpoplpush (B.pack "commits1") (B.pack "commits2")
      return commit
    return $ bStrToASCommit justC
  case commit of
    Nothing -> return Nothing
    Just c@(ASCommit uid b a t) -> do 
      deleteCells a 
      setCells b
      return $ Just c

redo :: IO (Maybe ASCommit)
redo = do 
  conn <- connect cInfo
  commit <- runRedis conn $ do 
    Right (Just commit) <- lpop (B.pack "commits2") 
    rpush (B.pack "commits1") [commit]
    return $ bStrToASCommit (Just commit)
  case commit of
    Nothing -> return Nothing
    Just c@(ASCommit uid b a t) -> do 
      deleteCells b 
      setCells a
      return $ Just c

----------------------------------------------------------------------------------------------------------------------

-- | Deal with updating all DB-related things after an eval
updateAfterEval :: ASUser -> ASCell -> [ASCell] -> [ASCell] -> IO ()
updateAfterEval user origCell desc cells = do 
  setCells cells
  addCommit user desc cells
  if (U.containsTrackingTag (cellTags origCell))
    then return () -- TODO: implement some redundancy in DB for tracking
    else return ()

-- | Creates and pushes a commit to the DB
addCommit :: ASUser -> [ASCell] -> [ASCell] -> IO ()
addCommit user b a = do 
  time <- getASTime
  let commit = ASCommit (userId user) b a time
  pushCommit commit
  putStrLn $ show commit

----------------------------------------------------------------------------------------------------------------------
