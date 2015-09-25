{-# LANGUAGE OverloadedStrings #-}

module AS.DB.API where

import Prelude

import AS.Types hiding (location,expression,value,min)
import AS.Util as U
import AS.DB.Util as DU

import Data.List (zip4,head,partition,nub)
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
-- Storage Documentation

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
-- Cells

getCell :: Connection -> ASLocation -> IO (Maybe ASCell)
getCell conn loc = return . head =<< getCells conn [loc]

getCells :: Connection -> [ASLocation] -> IO [Maybe ASCell]
getCells _ [] = return []
getCells conn locs = 
  let 
    (cols, nonCols) = partition isColumn locs
    dlocs = concat $ map U.decomposeLocs nonCols
    keys = map DU.getLocationKey dlocs
  in do
    cells <- getCellsByKeys conn keys
    columnCells <- mapM (getColumnCells conn) cols
    return $ cells ++ (concat columnCells) 
    

getCellsByKeys :: Connection -> [B.ByteString] -> IO [Maybe ASCell]
getCellsByKeys _ [] = return []
getCellsByKeys conn keys = do
  printTimed "redis about to run"
  runRedis conn $ do
    liftIO $ printTimed "redis about to get cells"
    cells <- mapM DU.getCellByKeyRedis keys
    liftIO $ printTimed "redis got cells"
    return cells

setCell :: Connection -> ASCell -> IO ()
setCell conn c = setCells conn [c]

setCells :: Connection -> [ASCell] -> IO ()
setCells _ [] = return ()
setCells conn cells = do
    runRedis conn $ do
        mapM_ DU.setCellRedis cells
        liftIO $ printTimed "redis set cells"
        return ()

deleteCells :: Connection -> [ASCell] -> IO ()
deleteCells _ [] = return ()
deleteCells conn cells = deleteLocs conn $ map cellLocation cells

deleteLocs :: Connection -> [ASLocation] -> IO ()
deleteLocs _ [] = return ()
deleteLocs conn locs = 
    let degenerateLocs = concat $ map U.decomposeLocs locs in
    do
        runRedis conn $ do
            _ <- mapM_ DU.deleteLocRedis degenerateLocs
            return ()

locationsExist :: Connection -> [ASLocation] -> IO [Bool]
locationsExist conn locs = do
  runRedis conn $ do
    TxSuccess results <- multiExec $ do
      bools <- mapM (\l -> exists $ DU.getLocationKey l) locs
      return $ sequence bools
    return results

getColumnCells :: Connection -> ASLocation -> IO [Maybe ASCell]
getColumnCells conn (Column sheetid col) = do
  runRedis conn $ do
    locKeys <- DU.getSheetLocsRedis sheetid
    liftIO $ printTimed "redis got column"
    let rows = map DU.keyToRow locKeys
    let firstRowKey = minBy keyToRow locKeys
    let locKeys = map (\i -> DU.incrementLocKey (1,i) firstRowKey) [(minimum rows)..(maximum rows)]
    cells <- mapM DU.getCellByKeyRedis locKeys
    liftIO $ printTimed "redis got cells"
    return cells

----------------------------------------------------------------------------------------------------------------------
-- DAG

getDAG :: Connection -> IO [(ASLocation,ASLocation)]
getDAG conn = do 
  runRedis conn $ do
      Right tl <- smembers (B.pack "DAGLocSet")
      TxSuccess fromLocs <- multiExec $ do 
          fl' <- mapM (\t -> (smembers t)) tl -- because Queued is a monad
          return $ sequence fl'
      let rels' = concat $ map (\(a,b) -> (zip a (repeat b))) (zip fromLocs tl)
      let rels = map bStrToRelation rels'
      return rels

updateDAG :: Connection -> [([ASLocation],ASLocation)] -> IO ()
updateDAG _ [] = return ()
updateDAG conn rels = (DU.chunkM_ conn) DU.updateChunkDAG DU.dagChunkSize rels

----------------------------------------------------------------------------------------------------------------------
-- Commits

-- | TODO: need to deal with large commit sizes and max number of commits

-- | Deal with updating all DB-related things after an eval
updateAfterEval :: Connection -> ASUserId -> ASCell -> [ASCell] -> [ASCell] -> IO ()
updateAfterEval conn uid origCell desc cells = do 
  printTimed "begin set cells"
  setCells conn cells
  printTimed "finished set cells"
  addCommit conn uid desc cells
  printTimed "added commit"
  if (U.containsTrackingTag (cellTags origCell))
    then return () -- TODO: implement some redundancy in DB for tracking
    else return ()

-- | Creates and pushes a commit to the DB
addCommit :: Connection -> ASUserId -> [ASCell] -> [ASCell] -> IO ()
addCommit conn uid b a = do 
  time <- getASTime
  let commit = ASCommit uid b a time
  pushCommit conn commit
  --putStrLn $ show commit

-- | Return a commit if possible (not possible if you undo past the beginning of time, etc)
-- | Update the DB so that there's always a source of truth (ie we will propagate undo to all relevant users)
undo :: Connection -> IO (Maybe ASCommit)
undo conn = do 
  commit <- runRedis conn $ do 
    TxSuccess justC <- multiExec $ do 
      commit <- rpoplpush (B.pack "pushed") (B.pack "popped")
      return commit
    return $ bStrToASCommit justC
  case commit of
    Nothing -> return Nothing
    Just c@(ASCommit uid b a t) -> do 
      deleteCells conn a 
      setCells conn b
      return $ Just c

redo :: Connection -> IO (Maybe ASCommit)
redo conn = do 
  commit <- runRedis conn $ do 
    Right result <- lpop (B.pack "popped") 
    case result of 
      (Just commit) -> do
        rpush (B.pack "pushed") [commit]
        return $ bStrToASCommit (Just commit)
      _ -> return Nothing
  case commit of
    Nothing -> return Nothing
    Just c@(ASCommit uid b a t) -> do 
      deleteCells conn b 
      setCells conn a
      return $ Just c

pushCommit :: Connection -> ASCommit -> IO ()
pushCommit conn c = do 
  let commit = (B.pack . show) c 
  runRedis conn $ do
    TxSuccess _ <- multiExec $ do 
      rpush (B.pack "pushed") [commit]
      incrbyfloat (B.pack "numCommits") 1
      del [(B.pack "popped")]
    return ()

----------------------------------------------------------------------------------------------------------------------
-- WorkbookSheets (for frontend API)

getAllWorkbookSheets :: Connection -> IO [WorkbookSheet]
getAllWorkbookSheets conn = do
  ws <- getAllWorkbooks conn
  ss <- getAllSheets conn
  return $ U.matchSheets ws ss

createWorkbookSheet :: Connection -> WorkbookSheet -> IO WorkbookSheet
createWorkbookSheet conn wbs = do
  let newSheets = wsSheets wbs
  newSheets' <- mapM (createSheet conn) newSheets
  let newSheetIds = map sheetId newSheets'
  wbResult <- getWorkbook conn $ wsName wbs
  case wbResult of 
    (Just wb) -> do
      modifyWorkbookSheets conn (\ss -> nub $ newSheetIds ++ ss) (workbookName wb)
      return wbs
    Nothing -> do
      wb <- createWorkbook conn newSheetIds
      return $ WorkbookSheet (workbookName wb) newSheets'

deleteWorkbookSheet :: Connection -> WorkbookSheet -> IO ()
deleteWorkbookSheet conn wbs = do
  let delSheets = map sheetId $ wsSheets wbs
  mapM_ (deleteSheetUnsafe conn) delSheets
  wbResult <- getWorkbook conn $ wsName wbs
  case wbResult of 
    (Just wb) -> modifyWorkbookSheets conn (\ss -> deleteSubset delSheets ss) (workbookName wb)
    Nothing -> return ()

modifyWorkbookSheets :: Connection -> ([ASSheetId] -> [ASSheetId]) -> String -> IO ()
modifyWorkbookSheets conn f wName = do
  (Just (Workbook wsName sheetIds)) <- getWorkbook conn wName
  let wbNew = Workbook wsName $ f sheetIds
  setWorkbook conn wbNew

----------------------------------------------------------------------------------------------------------------------
-- Raw workbooks

createWorkbook :: Connection -> [ASSheetId] -> IO ASWorkbook
createWorkbook conn sheetids = do
  wbName <- getUniqueWbName conn
  let wb = Workbook wbName sheetids
  setWorkbook conn wb
  return wb

getUniqueWbName :: Connection -> IO String
getUniqueWbName conn = do
  wbs <- getAllWorkbooks conn
  return $ DU.getUniquePrefixedName "Untitled" $ map workbookName wbs

getWorkbook :: Connection -> String -> IO (Maybe ASWorkbook)
getWorkbook conn name = do
    runRedis conn $ do
        mwb <- get $ DU.getWorkbookKey name
        case mwb of 
            (Right wb) -> return $ DU.bStrToWorkbook wb
            (Left _) -> return Nothing

getAllWorkbooks :: Connection -> IO [ASWorkbook]
getAllWorkbooks conn = do
    runRedis conn $ do
        Right wbKeys <- smembers (B.pack "workbookKeys")
        wbs <- mapM get wbKeys
        return $ map (\(Right (Just w)) -> read (B.unpack w) :: ASWorkbook) wbs

setWorkbook :: Connection -> ASWorkbook -> IO () 
setWorkbook conn wb = do
    runRedis conn $ do
        let workbookKey = DU.getWorkbookKey . workbookName $ wb
        TxSuccess _ <- multiExec $ do
            set workbookKey (B.pack . show $ wb)  -- set the workbook as key-value
            sadd (B.pack "workbookKeys") [workbookKey]  -- add the workbook key to the set of all sheets  
        return ()

workbookExists :: Connection -> String -> IO Bool
workbookExists conn wName = do
  runRedis conn $ do
    Right result <- exists $ DU.getWorkbookKey wName
    return result

-- only removes the workbook, not contained sheets
deleteWorkbook :: Connection -> String -> IO ()
deleteWorkbook conn name = do
    runRedis conn $ do
        let workbookKey = DU.getWorkbookKey name
        _ <- multiExec $ do
            del [workbookKey] 
            srem (B.pack "workbookKeys") [workbookKey]
        return ()

-- note: this is an expensive operation
deleteWorkbookAndSheets :: Connection -> String -> IO ()
deleteWorkbookAndSheets conn name = do
    mwb <- getWorkbook conn name
    case mwb of 
        Nothing -> return ()
        (Just wb) -> do
            mapM_ (deleteSheetUnsafe conn) (workbookSheets wb) -- remove sheets
            runRedis conn $ do
                let workbookKey = DU.getWorkbookKey name
                TxSuccess _ <- multiExec $ do
                    del [workbookKey]   -- remove workbook from key-value
                    srem (B.pack "workbookKeys") [workbookKey] -- remove workbook from set
                return ()

----------------------------------------------------------------------------------------------------------------------
-- Raw sheets

getSheet :: Connection -> ASSheetId -> IO (Maybe ASSheet)
getSheet conn sid = do
    runRedis conn $ do
        msheet <- get $ DU.getSheetKey sid
        case msheet of 
            (Right sheet) -> return $ DU.bStrToSheet sheet
            (Left _) -> return Nothing

getAllSheets :: Connection -> IO [ASSheet]
getAllSheets conn = do
    runRedis conn $ do
        Right sheetKeys <- smembers (B.pack "sheetKeys")
        sheets <- mapM get sheetKeys
        return $ map (\(Right (Just s)) -> read (B.unpack s) :: ASSheet) sheets

-- creates a sheet with unique id
createSheet :: Connection -> ASSheet -> IO ASSheet
createSheet conn (Sheet sid sname sperms) = do
    sid' <- U.getUniqueId
    let newSheet = Sheet sid' sname sperms
    setSheet conn newSheet
    return newSheet

setSheet :: Connection -> ASSheet -> IO ()
setSheet conn sheet = do
    runRedis conn $ do
        let sheetKey = DU.getSheetKey . sheetId $ sheet
        TxSuccess _ <- multiExec $ do
            set sheetKey (B.pack . show $ sheet)  -- set the sheet as key-value
            sadd (B.pack "sheetKeys") [sheetKey]  -- add the sheet key to the set of all sheets  
        return ()

-- deletes the sheet only, does not remove from any containing workbooks
deleteSheetUnsafe :: Connection -> ASSheetId -> IO ()
deleteSheetUnsafe conn sid = do
    runRedis conn $ do
        let setKey = DU.getSheetSetKey sid
            sheetKey = DU.getSheetKey sid

        mlocKeys <- smembers setKey
        TxSuccess _ <- multiExec $ do
            case mlocKeys of 
                (Right []) -> return () -- hedis can't delete empty list
                (Right locKeys) -> do
                    del locKeys -- delete all locs in the sheet
                    return ()
                (Left _) -> return ()
            del [setKey]      -- delete the loc set
            del [sheetKey]    -- delete the sheet
            srem (B.pack "sheetKeys") [sheetKey] -- remove the sheet key from the set of sheets
        return ()

----------------------------------------------------------------------------------------------------------------------
-- Volatile cell methods

getVolatileLocs :: Connection -> IO [ASLocation]
getVolatileLocs conn = do 
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
-- Permissions

canAccessSheet :: Connection -> ASUserId -> ASSheetId -> IO Bool
canAccessSheet conn uid sheetId = do
  sheet <- getSheet conn sheetId
  case sheet of 
    Nothing -> return False
    (Just someSheet) -> return $ hasPermissions uid (sheetPermissions someSheet)

canAccess :: Connection -> ASUserId -> ASLocation -> IO Bool
canAccess conn uid loc = canAccessSheet conn uid (locSheetId loc)

canAccessAll :: Connection -> ASUserId -> [ASLocation] -> IO Bool
canAccessAll conn uid locs = return . all id =<< mapM (canAccess conn uid) locs

isPermissibleMessage :: Connection -> ASMessage -> IO Bool
isPermissibleMessage conn (Message uid _ _ (PayloadC cell))      = canAccess conn uid (cellLocation cell)
isPermissibleMessage conn (Message uid _ _ (PayloadCL cells))    = canAccessAll conn uid (map cellLocation cells)
isPermissibleMessage conn (Message uid _ _ (PayloadL loc))       = canAccess conn uid loc
isPermissibleMessage conn (Message uid _ _ (PayloadLL locs))     = canAccessAll conn uid locs
isPermissibleMessage conn (Message uid _ _ (PayloadS sheet))     = canAccessSheet conn uid (sheetId sheet)
isPermissibleMessage conn (Message uid _ _ (PayloadW window))    = canAccessSheet conn uid (windowSheetId window)
isPermissibleMessage conn (Message uid _ _ (PayloadTags _ loc))  = canAccess conn uid loc
isPermissibleMessage _ _ = return True


----------------------------------------------------------------------------------------------------------------------
-- Users and Permissons TODO

