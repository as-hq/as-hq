{-# LANGUAGE OverloadedStrings #-}

module AS.DB.API where

import Prelude

import AS.Types.Core hiding (location,expression,value,min)
import AS.Types.DB
import AS.Util as U
import AS.DB.Util as DU

import Data.List (zip4,head,partition,nub,intercalate)
import Data.Maybe (isNothing, fromJust)

import Foreign
import Foreign.C.Types
import Foreign.C.String(CString(..))
import Foreign.C

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Time
import Database.Redis hiding (decode)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson hiding (Success)
import qualified Data.ByteString.Char8 as B 
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Unsafe as BU
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
-- FFI 

foreign import ccall unsafe "hiredis/redis_db.c getCells" c_getCells :: CString -> CInt -> IO (Ptr CString)
foreign import ccall unsafe "hiredis/redis_db.c setCells" c_setCells :: CString -> CInt -> IO ()

 
----------------------------------------------------------------------------------------------------------------------
-- Cells

getCell :: ASLocation -> IO (Maybe ASCell)
getCell loc = return . head =<< getCells [loc]

getCells :: [ASLocation] -> IO [Maybe ASCell]
getCells [] = return []
getCells locs = 
  let  
    msg = showB $ intercalate "@" $ map show2 locs
    num = length locs 
  in do
    ptrCells <- BU.unsafeUseAsCString msg $ \str -> c_getCells str (fromIntegral num)
    --ptrCells <- withCString str $ \cstr ->  c_getCells cstr (fromIntegral num)
    cCells <- peekArray (fromIntegral num) ptrCells
    res <- mapM DU.cToASCell cCells  
    free ptrCells 
    return res 
 
setCell :: ASCell -> IO () 
setCell c = setCells [c] 

setCells :: [ASCell] -> IO () 
setCells [] = return () 
setCells cells = do
  let str = intercalate "@" $ (map (show2 . cellLocation) cells) ++ (map show2 cells)
  let msg = showB str
  --B.putStrLn msg
  _ <- unsafeUseAsCString msg $ \lstr -> do
    --liftIO $ printTimed "packed message" 
    c_setCells lstr (fromIntegral . length $ cells)
  return ()

deleteCells :: Connection -> [ASCell] -> IO ()
deleteCells _ [] = return ()
deleteCells conn cells = deleteLocs conn $ map cellLocation cells

deleteLocs :: Connection -> [ASLocation] -> IO ()
deleteLocs _ [] = return ()
deleteLocs conn locs = do
  runRedis conn $ do
      _ <- mapM_ DU.deleteLocRedis locs
      return ()

-- bug: not working correctly for Alex
locationsExist :: Connection -> [ASLocation] -> IO [Bool]
locationsExist conn locs = do
  runRedis conn $ do
    TxSuccess results <- multiExec $ do
      bools <- mapM (\l -> exists $ DU.getLocationKey l) locs
      return $ sequence bools
    return results

-- locationsExist :: [ASLocation] -> IO [Bool]
-- locationsExist locs = do 
--     mCells <- getCells locs 
--     return $ map isJust mCells 

getColumnCells :: Connection -> ASColumn -> IO [Maybe ASCell]
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
-- Commits

-- | TODO: need to deal with large commit sizes and max number of commits

-- | Deal with updating all DB-related things after an eval
updateAfterEval :: Connection -> ASUserId -> ASCell -> [ASCell] -> [ASCell] -> IO ()
updateAfterEval conn uid origCell desc cells = do 
  printTimed "begin set cells"
  setCells cells
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
      commit <- rpoplpush "pushed" "popped"
      return commit
    return $ bStrToASCommit justC
  case commit of
    Nothing -> return Nothing
    Just c@(ASCommit uid b a t) -> do 
      deleteCells conn a 
      setCells b
      return $ Just c

redo :: Connection -> IO (Maybe ASCommit)
redo conn = do 
  commit <- runRedis conn $ do 
    Right result <- lpop "popped" 
    case result of 
      (Just commit) -> do
        rpush "pushed" [commit]
        return $ bStrToASCommit (Just commit)
      _ -> return Nothing
  case commit of
    Nothing -> return Nothing
    Just c@(ASCommit uid b a t) -> do 
      deleteCells conn b 
      setCells a
      return $ Just c

pushCommit :: Connection -> ASCommit -> IO ()
pushCommit conn c = do 
  let commit = (B.pack . show) c 
  runRedis conn $ do
    TxSuccess _ <- multiExec $ do 
      rpush "pushed" [commit]
      incrbyfloat "numCommits" 1
      del ["popped"]
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
        Right wbKeys <- smembers "workbookKeys"
        wbs <- mapM get wbKeys
        return $ map (\(Right (Just w)) -> read (B.unpack w) :: ASWorkbook) wbs

setWorkbook :: Connection -> ASWorkbook -> IO () 
setWorkbook conn wb = do
    runRedis conn $ do
        let workbookKey = DU.getWorkbookKey . workbookName $ wb
        TxSuccess _ <- multiExec $ do
            set workbookKey (B.pack . show $ wb)  -- set the workbook as key-value
            sadd "workbookKeys" [workbookKey]  -- add the workbook key to the set of all sheets  
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
            srem "workbookKeys" [workbookKey]
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
                    srem "workbookKeys" [workbookKey] -- remove workbook from set
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
        Right sheetKeys <- smembers "sheetKeys"
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
            sadd "sheetKeys" [sheetKey]  -- add the sheet key to the set of all sheets  
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
            srem "sheetKeys" [sheetKey] -- remove the sheet key from the set of sheets
        return ()

----------------------------------------------------------------------------------------------------------------------
-- Volatile cell methods

getVolatileLocs :: Connection -> IO [ASLocation]
getVolatileLocs conn = do 
  runRedis conn $ do
      Right vl <- smembers "volatileLocs"
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

isPermissibleMessage :: ASUserId -> Connection -> ASClientMessage -> IO Bool
isPermissibleMessage uid conn (ClientMessage _ (PayloadC cell))      = canAccess conn uid (cellLocation cell)
isPermissibleMessage uid conn (ClientMessage _ (PayloadCL cells))    = canAccessAll conn uid (map cellLocation cells)
isPermissibleMessage uid conn (ClientMessage _ (PayloadL loc))       = canAccess conn uid loc
isPermissibleMessage uid conn (ClientMessage _ (PayloadLL locs))     = canAccessAll conn uid locs
isPermissibleMessage uid conn (ClientMessage _ (PayloadS sheet))     = canAccessSheet conn uid (sheetId sheet)
isPermissibleMessage uid conn (ClientMessage _ (PayloadW window))    = canAccessSheet conn uid (windowSheetId window)
isPermissibleMessage uid conn (ClientMessage _ (PayloadTags _ loc))  = canAccess conn uid loc
isPermissibleMessage _ _ _ = return True


----------------------------------------------------------------------------------------------------------------------
-- Users and Permissons TODO

