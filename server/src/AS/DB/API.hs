{-# LANGUAGE OverloadedStrings #-}

module AS.DB.API where

import Prelude

import AS.Types hiding (location,expression,value)
import AS.Util as U
import AS.DB.Util as DU

import Data.List (zip4,head)
import Data.Maybe (isNothing, fromJust)

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

----------------------------------------------------------------------------------------------------------------------
-- | Storage Documentation

-- | Cells
-- key-value hashes
-- key is produced by cellLocation (see DU.getLocationKey) and is unique
-- fields are "cellExpression", "cellValue", "cellTags" with corresponding stringified values

-- | DAG
-- same as before: a set of relations
-- access set with key "DAGLocSet"

-- | Sheets
-- stored as key (DU.getSheetKey ASSheetId) value (stringified ASSheet)
-- additionally, the set of all locations belonging to a sheet are stored as
-- set key (DU.getSheetSetKey ASSheetId) members (ASLocationKey)
-- this set is updated automatically during setCells.
-- finally, a record of all sheetKeys is stored as a set with key "sheets" and members (DU.getSheetKey sheetid)

-- | Workbooks
-- stored identically to Sheets

-- | Commits
-- stored as before, as a list of commits

-- | Volatile locs
-- stored as before, as a set with key volatileLocs

----------------------------------------------------------------------------------------------------------------------
-- | Cells

getCell :: ASLocation -> IO (Maybe ASCell)
getCell loc = return . head =<< getCells [loc]

getCells :: [ASLocation] -> IO [Maybe ASCell]
getCells [] = return []
getCells locs = 
  let dlocs = concat $ map U.decomposeLocs locs in 
  do
    conn <- connect cInfo 
    runRedis conn $ do
        cells <- mapM DU.getCellRedis locs
        return cells

setCell :: ASCell -> IO ()
setCell c = setCells [c]

setCells :: [ASCell] -> IO ()
setCells [] = return ()
setCells cells = do
    conn <- connect cInfo 
    runRedis conn $ do
        _ <- mapM_ DU.setCellRedis cells
        return ()

deleteCells :: [ASCell] -> IO ()
deleteCells [] = return ()
deleteCells cells = deleteLocs $ map cellLocation cells

deleteLocs :: [ASLocation] -> IO ()
deleteLocs [] = return ()
deleteLocs locs = 
    let degenerateLocs = concat $ map U.decomposeLocs locs in
    do
        conn <- connect cInfo 
        runRedis conn $ do
            _ <- mapM_ DU.deleteLocRedis locs
            return ()

----------------------------------------------------------------------------------------------------------------------
-- | DAG

getDAG :: IO [(ASLocation,ASLocation)]
getDAG = do 
  conn <- connect cInfo
  runRedis conn $ do
      Right tl <- smembers (B.pack "DAGLocSet")
      TxSuccess fromLocs <- multiExec $ do 
          fl' <- mapM (\t -> (smembers t)) tl -- because Queued is a monad
          return $ sequence fl'
      let rels' = concat $ map (\(a,b) -> (zip a (repeat b))) (zip fromLocs tl)
      let rels = map bStrToRelation rels'
      return rels

updateDAG :: [([ASLocation],ASLocation)] -> IO ()
updateDAG [] = return ()
updateDAG rels = DU.chunkM_ DU.updateChunkDAG DU.dagChunkSize rels

----------------------------------------------------------------------------------------------------------------------
-- | Commits

-- | TODO: need to deal with large commit sizes and max number of commits

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

----------------------------------------------------------------------------------------------------------------------
-- | Sheets and workbooks

getSheet :: ASSheetId -> IO (Maybe ASSheet)
getSheet sid = do
    conn <- connect cInfo 
    runRedis conn $ do
        msheet <- get $ DU.getSheetKey sid
        case msheet of 
            (Right sheet) -> return $ DU.bStrToSheet sheet
            (Left _) -> return Nothing

getWorkbook :: String -> IO (Maybe ASWorkbook)
getWorkbook name = do
    conn <- connect cInfo 
    runRedis conn $ do
        mwb <- get $ DU.getWorkbookKey name
        case mwb of 
            (Right wb) -> return $ DU.bStrToWorkbook wb
            (Left _) -> return Nothing

getAllSheets :: IO [ASSheet]
getAllSheets = do
    conn <- connect cInfo 
    runRedis conn $ do
        Right sheetKeys <- smembers (B.pack "sheetKeys")
        sheets <- mapM get sheetKeys
        return $ map (\(Right (Just s)) -> read (B.unpack s) :: ASSheet) sheets

getAllWorkbooks :: IO [ASWorkbook]
getAllWorkbooks = do
    conn <- connect cInfo 
    runRedis conn $ do
        Right wbKeys <- smembers (B.pack "workbookKeys")
        wbs <- mapM get wbKeys
        return $ map (\(Right (Just w)) -> read (B.unpack w) :: ASWorkbook) wbs

-- creates a sheet with unique id
createSheet :: ASSheet -> IO ASSheet
createSheet (Sheet sid sname sperms) = do
    sid' <- U.getUniqueId
    let newSheet = Sheet sid' sname sperms
    setSheet newSheet
    return newSheet

setSheet :: ASSheet -> IO ()
setSheet sheet = do
    conn <- connect cInfo 
    runRedis conn $ do
        let sheetKey = DU.getSheetKey . sheetId $ sheet
        TxSuccess _ <- multiExec $ do
            set sheetKey (B.pack . show $ sheet)  -- set the sheet as key-value
            sadd (B.pack "sheetKeys") [sheetKey]  -- add the sheet key to the set of all sheets  
        return ()

setWorkbook :: ASWorkbook -> IO () 
setWorkbook wb = do
    conn <- connect cInfo 
    runRedis conn $ do
        let workbookKey = DU.getWorkbookKey . workbookName $ wb
        TxSuccess _ <- multiExec $ do
            set workbookKey (B.pack . show $ wb)  -- set the workbook as key-value
            sadd (B.pack "workbookKeys") [workbookKey]  -- add the workbook key to the set of all sheets  
        return ()

deleteSheet :: ASSheetId -> IO ()
deleteSheet sid = do
    conn <- connect cInfo 
    runRedis conn $ do
        let setKey = DU.getSheetSetKey sid
            sheetKey = DU.getSheetKey sid
        mlocKeys <- smembers setKey
        TxSuccess _ <- multiExec $ do
            case mlocKeys of 
                (Right locKeys) -> do
                    del locKeys -- delete all locs in the sheet
                    return ()
                (Left _) -> return ()
            del [setKey]      -- delete the loc set
            del [sheetKey]    -- delete the sheet
            srem (B.pack "sheetKeys") [sheetKey] -- remove the sheet key from the set of sheets
        return ()

-- only removes the workbook, not contained sheets
deleteWorkbook :: String -> IO ()
deleteWorkbook name = do
    conn <- connect cInfo 
    runRedis conn $ do
        let workbookKey = DU.getWorkbookKey name
        _ <- multiExec $ do
            del [workbookKey] 
            srem (B.pack "workbookKeys") [workbookKey]
        return ()

-- note: this is an expensive operation
deleteWorkbookAndSheets :: String -> IO ()
deleteWorkbookAndSheets name = do
    mwb <- getWorkbook name
    case mwb of 
        Nothing -> return ()
        (Just wb) -> do
            mapM_ deleteSheet (workbookSheets wb) -- remove sheets
            conn <- connect cInfo 
            runRedis conn $ do
                let workbookKey = DU.getWorkbookKey name
                TxSuccess _ <- multiExec $ do
                    del [workbookKey]   -- remove workbook from key-value
                    srem (B.pack "workbookKeys") [workbookKey] -- remove workbook from set
                return ()

----------------------------------------------------------------------------------------------------------------------
-- | Volatile cell methods

getVolatileLocs :: IO [ASLocation]
getVolatileLocs = do 
  conn <- connect cInfo
  runRedis conn $ do
      Right vl <- smembers (B.pack "volatileLocs")
      return $ map bStrToASLocation vl

-- TODO: some of the cells may change from volatile -> not volatile, but they're still in volLocs
setChunkVolatileCells :: [ASCell] -> Redis ()
setChunkVolatileCells cells = do 
  let vLocs = map cellLocation $ filter (U.hasVolatileTag) cells
  let locStrs = map (B.pack . show) vLocs
  sadd "volatileLocs" locStrs
  return ()

deleteChunkVolatileCells :: [ASCell] -> Redis ()
deleteChunkVolatileCells cells = do 
  let vLocs = map cellLocation $ filter (U.hasVolatileTag) cells
  let locStrs = map (B.pack . show) vLocs
  srem "volatileLocs" locStrs
  return ()

----------------------------------------------------------------------------------------------------------------------
-- | Users TODO