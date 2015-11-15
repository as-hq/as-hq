{-# LANGUAGE OverloadedStrings #-}

module AS.DB.API where

import Prelude

import AS.Types.Core hiding (location,expression,value,min)
import AS.Types.DB
import AS.Util as U
import qualified AS.DB.Util as DU
import AS.Parsing.Substitutions (getDependencies)
import AS.DB.Graph as G

import Data.List (zip4,head,partition,nub,intercalate)
import Data.Maybe (isNothing,fromJust,catMaybes)

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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Unsafe as BU
import Data.List.Split
-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
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
-- set key (DU.getSheetSetKey ASSheetId) members (ASIndexKey)
-- this set is updated automatically during setCells.
-- finally, a record of all sheetKeys is stored as a set with key "sheets" and members (DU.getSheetKey sheetid)

-- | Workbooks
-- stored identically to Sheets

-- | Volatile locs
-- stored as before, as a set with key volatileLocs

clear :: Connection -> IO ()
clear conn = runRedis conn $ flushall >> return ()

----------------------------------------------------------------------------------------------------------------------
-- Cells

getCell :: ASIndex -> IO (Maybe ASCell)
getCell loc = return . head =<< getCells [loc]

getPossiblyBlankCell :: ASIndex -> IO ASCell
getPossiblyBlankCell loc = return . head =<< getPossiblyBlankCells [loc]

getCells :: [ASIndex] -> IO [Maybe ASCell]
getCells [] = return []
getCells locs = DU.getCellsByMessage msg num
  where
    msg = DU.showB $ intercalate DU.msgPartDelimiter $ map show2 locs
    num = length locs

-- #incomplete LOL
getCellsInSheet :: ASSheetId -> IO [ASCell]
getCellsInSheet sid = do 
  cells <- getCellsByRange (Range sid ((1,1), (100,100)))
  return $ catMaybes cells

getPossiblyBlankCells :: [ASIndex] -> IO [ASCell]
getPossiblyBlankCells locs = do 
  cells <- getCells locs
  return $ map (\(l,c) -> case c of 
    Just c' -> c'
    Nothing -> Cell l (Expression "" Excel) NoValue []) (zip locs cells) -- someone hardcoded Excel ???

getCellsByRange :: ASRange -> IO [Maybe ASCell]
getCellsByRange rng = getCells (fromJust $ refToIndices $ RangeRef rng)

setCell :: ASCell -> IO ()
setCell c = setCells [c]

setCells :: [ASCell] -> IO ()
setCells [] = return ()
setCells cells = do 
  let str = intercalate DU.msgPartDelimiter $ (map (show2 . cellLocation) cells) ++ (map show2 cells)
      msg = DU.showB str
      num = length cells
  DU.setCellsByMessage msg num


deleteCells :: Connection -> [ASCell] -> IO ()
deleteCells _ [] = return ()
deleteCells conn cells = deleteLocs conn $ map cellLocation cells

deleteLocs :: Connection -> [ASIndex] -> IO ()
deleteLocs _ [] = return ()
deleteLocs conn locs = runRedis conn $ mapM_ DU.deleteLocRedis locs

locationsExist :: Connection -> [ASIndex] -> IO [Bool]
locationsExist conn locs = do
  runRedis conn $ do
    TxSuccess results <- multiExec $ do
      bools <- mapM (\l -> exists $ DU.getLocationKey l) locs
      return $ sequence bools
    return results

locationExists :: Connection -> ASIndex -> IO Bool
locationExists conn loc = runRedis conn $ do
  Right result <- exists $ DU.getLocationKey loc
  return result

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
  return $ DU.getUniquePrefixedName "Workbook" $ map workbookName wbs

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
createSheet conn (Sheet sid _ sperms) = do
    sid' <- T.pack <$> U.getUniqueId
    sname <- getUniqueSheetName conn
    let newSheet = Sheet sid' sname sperms
    setSheet conn newSheet
    return newSheet

getUniqueSheetName :: Connection -> IO String
getUniqueSheetName conn = do
  ss <- getAllSheets conn
  return $ DU.getUniquePrefixedName "Sheet" $ map sheetName ss

setSheet :: Connection -> ASSheet -> IO ()
setSheet conn sheet = do
    runRedis conn $ do
        let sheetKey = DU.getSheetKey . sheetId $ sheet
        TxSuccess _ <- multiExec $ do
            set sheetKey (B.pack . show $ sheet)  -- set the sheet as key-value
            sadd "sheetKeys" [sheetKey]  -- add the sheet key to the set of all sheets
        return ()

clearSheet :: ASSheetId -> IO ()
clearSheet = DU.deleteLocsInSheet

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

getVolatileLocs :: Connection -> IO [ASIndex]
getVolatileLocs conn = do
  runRedis conn $ do
      Right vl <- smembers "volatileLocs"
      return $ map DU.bStrToASIndex vl

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

canAccess :: Connection -> ASUserId -> ASIndex -> IO Bool
canAccess conn uid loc = canAccessSheet conn uid (locSheetId loc)

canAccessAll :: Connection -> ASUserId -> [ASIndex] -> IO Bool
canAccessAll conn uid locs = return . all id =<< mapM (canAccess conn uid) locs

isPermissibleMessage :: ASUserId -> Connection -> ASClientMessage -> IO Bool
isPermissibleMessage uid conn (ClientMessage _ payload) = case payload of 
  PayloadCL cells -> canAccessAll conn uid (map cellLocation cells)
  PayloadLL locs -> canAccessAll conn uid locs
  PayloadS sheet -> canAccessSheet conn uid (sheetId sheet)
  PayloadW window -> canAccessSheet conn uid (windowSheetId window)
  PayloadTag _ rng -> canAccessAll conn uid (rangeToIndices rng)
  _ -> return True
isPermissibleMessage uid conn _ = return True
