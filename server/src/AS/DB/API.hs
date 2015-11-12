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

-- | Commits
-- stored as before, as a list of commits

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
    Nothing -> Cell l (Expression "" Excel) NoValue []) (zip locs cells)

getCellsByRange :: ASRange -> IO [Maybe ASCell]
getCellsByRange rng = getCells $ rangeToIndices rng

setCell :: ASCell -> IO ()
setCell c = setCells [c]

setCells :: [ASCell] -> IO ()
setCells [] = return ()
setCells cells = do 
  let str = intercalate DU.msgPartDelimiter $ (map (show2 . cellLocation) cells) ++ (map show2 cells)
      msg = DU.showB str
      num = length cells
  DU.setCellsByMessage msg num

-- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of setting the cells. 
setCellsPropagated :: Connection -> [ASCell] -> IO ()
setCellsPropagated conn cells = do 
  setCells cells
  setCellsAncestorsForce $ filter (\c -> (not $ U.isListMember c) || DU.isListHead c) cells
  let listKeys = nub $ catMaybes $ map DU.getCellListKey cells
  mapM_ (recoupleList conn) listKeys

deleteCells :: Connection -> [ASCell] -> IO ()
deleteCells _ [] = return ()
deleteCells conn cells = deleteLocs conn $ map cellLocation cells

-- | Makes sure everything is synced -- the listKeys and ancestors in graph db should reflect 
-- the cell changes that happen as a result of deleting the cells. 
deleteCellsPropagated :: Connection -> [ASCell] -> IO ()
deleteCellsPropagated conn cells = do 
  deleteCells conn cells
  setCellsAncestorsForce $ U.blankCellsAt (map cellLocation cells)
  let listKeys = nub $ catMaybes $ map DU.getCellListKey cells
  mapM_ (decoupleList conn) listKeys

deleteLocs :: Connection -> [ASIndex] -> IO ()
deleteLocs _ [] = return ()
deleteLocs conn locs = runRedis conn $ do
  _ <- mapM_ DU.deleteLocRedis locs
  return ()

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

-- | Returns the listkeys of all the lists that are entirely contained in the range.  
getListsInRange :: Connection -> ASRange -> IO [ListKey]
getListsInRange conn rng = do
  let sid = rangeSheetId rng
  listKeys <- DU.getListKeysInSheet conn sid
  let rects = map DU.getRectFromListKey listKeys
      zipRects = zip listKeys rects
      zipRectsContained = filter (\(_,rect) -> U.rangeContainsRect rng rect) zipRects
  return $ map fst zipRectsContained

setListLocations :: Connection -> ListKey -> [ASIndex] -> IO ()
setListLocations conn listKey locs
  | null locs = return ()
  | otherwise = do 
    let locKeys       = map DU.getLocationKey locs
        listKey'      = B.pack listKey
        sheetListsKey = DU.getSheetListsKey . locSheetId $ head locs
    runRedis conn $ do
      liftIO $ printWithTime $ "setting list locations for key: " ++ listKey
      sadd listKey' locKeys
      sadd sheetListsKey [listKey']
      return ()

-- | Takes in a cell that's tied to a list. Decouples all the cells in that list from that
-- | returns: (cells before decoupling, cells after decoupling)
-- Note: this operation is O(n)
-- TODO move to C client because it's expensive
decoupleList :: Connection -> ListKey -> IO ([ASCell], [ASCell])
decoupleList conn listString = do
  let listKey = B.pack listString
  let sheetListsKey = DU.getSheetListsKey $ DU.getSheetIdFromListKey listString
  locs <- runRedis conn $ do
    TxSuccess result <- multiExec $ do
      result <- smembers listKey
      del [listKey]
      srem sheetListsKey [listKey]
      return result
    return result
  printWithTime $ "got coupled locs: " ++ (U.truncated $ show locs)
  case locs of
    [] -> return ([],[])
    ls -> do
      listCells <- U.filterNothing <$> DU.getCellsByKeys ls
      let decoupledCells = map DU.decoupleCell listCells
      return (listCells, decoupledCells)

recoupleList :: Connection -> ListKey -> IO ()
recoupleList conn key = do
  let locs = DU.getLocationsFromListKey key
  locsExist <- locationsExist conn locs
  let locs' = U.zipFilter $ zip locs locsExist
  setListLocations conn key locs'

----------------------------------------------------------------------------------------------------------------------
-- Commits

-- TODO: need to deal with large commit sizes and max number of commits
-- TODO: need to delete blank cells from the DB. (Otherwise e.g. if you delete a
-- a huge range, you're going to have all those cells in the DB doing nothing.)

-- | Deal with updating all DB-related things after an eval. 
updateAfterEval :: Connection -> ASTransaction -> EitherTExec [ASCell]
updateAfterEval conn (Transaction src@(sid, _) roots afterCells lists) = do
  let newListCells         = concat $ map snd lists
      afterCellsWithLists  = afterCells ++ newListCells
      cellLocs             = map cellLocation afterCellsWithLists
  beforeCells     <- lift $ catMaybes <$> getCells cellLocs
  listKeysChanged <- liftIO $ DU.getListIntersections conn sid cellLocs
  decoupleResult  <- lift $ mapM (decoupleList conn) listKeysChanged
  let (coupledCells, decoupledCells) = U.liftListTuple decoupleResult
      afterCells'                    = U.mergeCells afterCellsWithLists decoupledCells
      beforeCells'                   = U.mergeCells beforeCells coupledCells
  liftIO $ setCells afterCells'
  liftIO $ deleteCells conn (filter isEmptyCell afterCells')
  liftIO $ mapM_ (\(key, cells) -> setListLocations conn key (map cellLocation cells)) lists
  liftIO $ addCommit conn (beforeCells', afterCells') src
  liftIO $ printWithTime "added commits"
  right afterCells'

-- | Update the ancestor relationships in the DB based on the expressions and locations of the
-- cells passed in. (E.g. if a cell is passed in at A1 and its expression is "C1 + 1", C1->A1 is
-- added to the graph.)
setCellsAncestors :: [ASCell] -> EitherTExec [[ASReference]]
setCellsAncestors cells = G.setRelations relations >> return depSets
  where
    depSets = map (\(Cell l e _ _) -> getDependencies (locSheetId l) e) cells
    zipSets = zip cells depSets
    relations = map (\((Cell l _ _ _), depSet) -> (l, concat $ catMaybes $ map refToIndices depSet)) zipSets

-- | Should only be called when undoing or redoing commits, which should be guaranteed to not
-- introduce errors.
setCellsAncestorsForce :: [ASCell] -> IO ()
setCellsAncestorsForce cells = do
  runEitherT (setCellsAncestors cells)
  return ()

-- | Creates and pushes a commit to the DB
addCommit :: Connection -> ([ASCell], [ASCell]) -> CommitSource -> IO ()
addCommit conn (b,a) src = do
  time <- getASTime
  let commit = ASCommit b a time
  pushCommit conn commit src
  --putStrLn $ show commit

-- Slightly 
pushKey :: CommitSource -> BS.ByteString
pushKey (sid, uid) = B.pack $ (T.unpack sid) ++ '|':(T.unpack uid) ++ "pushed"

popKey :: CommitSource -> BS.ByteString
popKey (sid, uid)  = B.pack $ (T.unpack sid) ++ '|':(T.unpack uid) ++ "popped"

-- | Return a commit if possible (not possible if you undo past the beginning of time, etc)
-- | Update the DB so that there's always a source of truth (ie we will initEval undo to all relevant users)
undo :: Connection -> CommitSource -> IO (Maybe ASCommit)
undo conn src = do
  commit <- runRedis conn $ do
    TxSuccess justC <- multiExec $ do
      commit <- rpoplpush (pushKey src) (popKey src)
      return commit
    return $ DU.bStrToASCommit justC
  case commit of
    Nothing -> return Nothing
    Just c@(ASCommit b a t) -> do
      deleteCellsPropagated conn a
      setCellsPropagated conn b
      -- mapM_ (recoupleList conn) listKeys
      return $ Just c

redo :: Connection -> CommitSource -> IO (Maybe ASCommit)
redo conn src = do
  commit <- runRedis conn $ do
    Right result <- lpop (popKey src)
    case result of
      (Just commit) -> do
        rpush (pushKey src) [commit]
        return $ DU.bStrToASCommit (Just commit)
      _ -> return Nothing
  case commit of
    Nothing -> return Nothing
    Just c@(ASCommit b a t) -> do
      deleteCellsPropagated conn b
      setCellsPropagated conn a
      -- mapM_ (decoupleList conn) listKeys
      return $ Just c

pushCommit :: Connection -> ASCommit -> CommitSource -> IO ()
pushCommit conn c src = do
  let commit = (B.pack . show) c
  runRedis conn $ do
    TxSuccess _ <- multiExec $ do
      rpush (pushKey src) [commit]
      incrbyfloat "numCommits" 1
      del [popKey src]
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
isPermissibleMessage uid conn (ClientMessage _ (PayloadCL cells))  = canAccessAll conn uid (map cellLocation cells)
isPermissibleMessage uid conn (ClientMessage _ (PayloadLL locs))   = canAccessAll conn uid locs
isPermissibleMessage uid conn (ClientMessage _ (PayloadS sheet))   = canAccessSheet conn uid (sheetId sheet)
isPermissibleMessage uid conn (ClientMessage _ (PayloadW window))  = canAccessSheet conn uid (windowSheetId window)
isPermissibleMessage uid conn (ClientMessage _ (PayloadTag _ rng)) = canAccessAll conn uid (rangeToIndices rng)
isPermissibleMessage _ _ _ = return True


----------------------------------------------------------------------------------------------------------------------
-- Users and Permissons TODO