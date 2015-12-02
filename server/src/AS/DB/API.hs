{-# LANGUAGE OverloadedStrings #-}

module AS.DB.API where

import Prelude

import AS.Types.Cell
import AS.Types.Messages
import AS.Types.DB
import AS.Util as U
import qualified AS.DB.Util as DU
import AS.Parsing.Substitutions (getDependencies)
import AS.DB.Graph as G
import AS.Window

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
import Data.List as L
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
-- key is produced by cellLocation (see DU.makeLocationKey) and is unique
-- fields are "cellExpression", "cellValue", "cellTags" with corresponding stringified values

-- | DAG
-- same as before: a set of relations
-- access set with key "DAGLocSet"

-- | Sheets
-- stored as key (DU.makeSheetKey ASSheetId) value (stringified ASSheet)
-- additionally, the set of all locations belonging to a sheet are stored as
-- set key (DU.makeSheetSetKey ASSheetId) members (ASIndexKey)
-- this set is updated automatically during setCells.
-- finally, a record of all sheetKeys is stored as a set with key "sheets" and members (DU.makeSheetKey sheetid)

-- | Workbooks
-- stored identically to Sheets

-- | Volatile locs
-- stored as before, as a set with key volatileLocs

clear :: Connection -> IO ()
clear conn = runRedis conn $ flushall >> return ()

----------------------------------------------------------------------------------------------------------------------
-- Cells

getCell :: ASIndex -> IO (Maybe ASCell)
getCell loc = head <$> getCells [loc]

getPossiblyBlankCell :: ASIndex -> IO ASCell
getPossiblyBlankCell loc = head <$> getPossiblyBlankCells [loc]

-- expects all indices to be Index
getCells :: [ASIndex] -> IO [Maybe ASCell]
getCells [] = return []
getCells locs = DU.getCellsByMessage msg num
  where
    msg = DU.showB $ intercalate DU.msgPartDelimiter $ map show2 locs
    num = length locs

getCellsInSheet :: Connection -> ASSheetId -> IO [ASCell]
getCellsInSheet = DU.cellsInSheet

-- Gets the cells at the locations with expressions and values removed, but tags intact. 
getBlankedCellsAt :: [ASIndex] -> IO [ASCell]
getBlankedCellsAt locs = 
  let blank xp = case xp of 
            Expression _ lang -> Expression "" lang
            Coupled _ lang _ _ -> Expression "" lang
  in do 
    cells <- getPossiblyBlankCells locs
    return $ map (\(Cell l xp v ts) -> Cell l (blank xp) NoValue ts) cells

-- allows indices to be Pointer or Index
getCompositeCells :: Connection -> [ASIndex] -> IO [Maybe CompositeCell]
getCompositeCells _ [] = return []
getCompositeCells conn locs =
  let msg = DU.showB $ intercalate DU.msgPartDelimiter $ map (show2 . pointerToIndex) locs
      num = length locs
      expandPointerRefs (loc, ccell) = case loc of 
        Pointer sid coord -> case ccell of 
          Just (Cell l (Coupled xp lang dtype key) v ts) -> do
            -- if the cell was coupled but no range descriptor exists, something fucked up.
            (Just desc) <- getRangeDescriptor conn key
            let fatLocs = DU.rangeKeyToIndices key
            cells <- map fromJust <$> getCells fatLocs
            return . Just . Fat $ FatCell cells desc
          _ -> return Nothing
        _ -> return $ Single <$> ccell 
  in do 
    ccells <- DU.getCellsByMessage msg num
    mapM expandPointerRefs $ zip locs ccells

getPossiblyBlankCells :: [ASIndex] -> IO [ASCell]
getPossiblyBlankCells locs = do 
  cells <- getCells locs
  return $ map (\(l,c) -> case c of 
    Just c' -> c'
    Nothing -> blankCellAt l) (zip locs cells)

getPropsAt :: [ASIndex] -> IO [ASCellProps]
getPropsAt locs = do 
  cells <- getPossiblyBlankCells locs
  return $ map cellProps cells

setCell :: ASCell -> IO ()
setCell c = setCells [c]

setCells :: [ASCell] -> IO ()
setCells [] = return ()
setCells cells = DU.setCellsByMessage msg num
  where 
    str = intercalate DU.msgPartDelimiter $ (map (show2 . cellLocation) cells) ++ (map show2 cells)
    msg = DU.showB str
    num = length cells

deleteCells :: Connection -> [ASCell] -> IO ()
deleteCells _ [] = return ()
deleteCells conn cells = deleteLocs conn $ map cellLocation cells

deleteLocs :: Connection -> [ASIndex] -> IO ()
deleteLocs _ [] = return ()
deleteLocs conn locs = runRedis conn $ mapM_ DU.deleteLocRedis locs

----------------------------------------------------------------------------------------------------------------------
-- Locations

locationsExist :: Connection -> [ASIndex] -> IO [Bool]
locationsExist conn locs = runRedis conn $ map fromRight <$> mapM locExists locs
  where
    fromRight (Right a) = a
    locExists l         = exists $ DU.makeLocationKey l

locationExists :: Connection -> ASIndex -> IO Bool
locationExists conn loc = head <$> locationsExist conn [loc] 

-- | Returns the listkeys of all the lists that are entirely contained in the range.  
fatCellsInRange :: Connection -> ASRange -> IO [RangeKey]
fatCellsInRange conn rng = do
  let sid = rangeSheetId rng
  rangeKeys <- DU.makeRangeKeysInSheet conn sid
  let rects = map DU.rangeRect rangeKeys
      zipRects = zip rangeKeys rects
      zipRectsContained = filter (\(_,rect) -> rangeContainsRect rng rect) zipRects
  return $ map fst zipRectsContained

getRangeDescriptor :: Connection -> RangeKey -> IO (Maybe RangeDescriptor)
getRangeDescriptor conn key = runRedis conn $ do 
  Right desc <- get (B.pack key)
  return $ DU.bStrToRangeDescriptor desc

----------------------------------------------------------------------------------------------------------------------
-- WorkbookSheets (for frontend API)

matchSheets :: [ASWorkbook] -> [ASSheet] -> [WorkbookSheet]
matchSheets ws ss = [WorkbookSheet (workbookName w) (catMaybes $ lookUpSheets w ss) | w <- ws]
  where
    findSheet sid = L.find (\sh -> sid == sheetId sh) ss
    lookUpSheets workbook sheets = map findSheet (workbookSheets workbook)

getAllWorkbookSheets :: Connection -> IO [WorkbookSheet]
getAllWorkbookSheets conn = do
  ws <- getAllWorkbooks conn
  ss <- getAllSheets conn
  return $ matchSheets ws ss

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
    (Just wb) -> modifyWorkbookSheets conn deleteSheets (workbookName wb)
      where deleteSheets = filter $ \s -> not $ s `elem` delSheets
    Nothing -> return ()

modifyWorkbookSheets :: Connection -> ([ASSheetId] -> [ASSheetId]) -> String -> IO ()
modifyWorkbookSheets conn f wName = do
  (Just (Workbook wsName sheetIds)) <- getWorkbook conn wName
  let wbNew = Workbook wsName $ f sheetIds
  setWorkbook conn wbNew

----------------------------------------------------------------------------------------------------------------------
-- Ancestors

-- | Update the ancestor relationships in the DB based on the expressions and locations of the
-- cells passed in. (E.g. if a cell is passed in at A1 and its expression is "C1 + 1", C1->A1 is
-- added to the graph.)
setCellsAncestors :: [ASCell] -> EitherTExec [[ASReference]]
setCellsAncestors cells = G.setRelations relations >> return depSets
  where
    depSets = map (\(Cell l e _ _) -> getDependencies (locSheetId l) e) cells
    zipSets = zip cells depSets
    relations = map (\((Cell l _ _ _), depSet) -> (l, concat $ catMaybes $ map refToIndices depSet)) zipSets

-- | It'll parse no dependencies from the blank cells at these locations, so each location in the
-- graph DB gets all its ancestors removed. 
removeAncestorsAt :: [ASIndex] -> EitherTExec [[ASReference]]
removeAncestorsAt = setCellsAncestors . blankCellsAt

-- | Should only be called when undoing or redoing commits, which should be guaranteed to not
-- introduce errors. 
setCellsAncestorsForce :: [ASCell] -> IO ()
setCellsAncestorsForce cells = runEitherT (setCellsAncestors cells) >> return ()

removeAncestorsAtForced :: [ASIndex] -> IO ()
removeAncestorsAtForced locs = runEitherT (removeAncestorsAt locs) >> return ()

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
        mwb <- get $ DU.makeWorkbookKey name
        case mwb of
            Right wb -> return $ DU.bStrToWorkbook wb
            Left _   -> return Nothing

getAllWorkbooks :: Connection -> IO [ASWorkbook]
getAllWorkbooks conn = do
    runRedis conn $ do
        Right wbKeys <- smembers "workbookKeys"
        wbs <- mapM get wbKeys
        return $ map (\(Right (Just w)) -> read (B.unpack w) :: ASWorkbook) wbs

setWorkbook :: Connection -> ASWorkbook -> IO ()
setWorkbook conn wb = do
    runRedis conn $ do
        let workbookKey = DU.makeWorkbookKey . workbookName $ wb
        TxSuccess _ <- multiExec $ do
            set workbookKey (B.pack . show $ wb)  -- set the workbook as key-value
            sadd "workbookKeys" [workbookKey]  -- add the workbook key to the set of all sheets
        return ()

workbookExists :: Connection -> String -> IO Bool
workbookExists conn wName = do
  runRedis conn $ do
    Right result <- exists $ DU.makeWorkbookKey wName
    return result

-- only removes the workbook, not contained sheets
deleteWorkbook :: Connection -> String -> IO ()
deleteWorkbook conn name = do
    runRedis conn $ do
        let workbookKey = DU.makeWorkbookKey name
        multiExec $ do
          del [workbookKey]
          srem "workbookKeys" [workbookKey]
        return ()

-- note: this is an expensive operation
deleteWorkbookAndSheets :: Connection -> String -> IO ()
deleteWorkbookAndSheets conn name = do
    mwb <- getWorkbook conn name
    case mwb of
        Nothing -> return ()
        Just wb -> do
            mapM_ (deleteSheetUnsafe conn) (workbookSheets wb) -- remove sheets
            runRedis conn $ do
                let workbookKey = DU.makeWorkbookKey name
                TxSuccess _ <- multiExec $ do
                    del [workbookKey]   -- remove workbook from key-value
                    srem "workbookKeys" [workbookKey] -- remove workbook from set
                return ()

----------------------------------------------------------------------------------------------------------------------
-- Raw sheets

getSheet :: Connection -> ASSheetId -> IO (Maybe ASSheet)
getSheet conn sid = do
    runRedis conn $ do
        msheet <- get $ DU.makeSheetKey sid
        case msheet of
            Right sheet -> return $ DU.bStrToSheet sheet
            Left _      -> return Nothing

getAllSheets :: Connection -> IO [ASSheet]
getAllSheets conn = 
  let readSheet (Right (Just s)) = read (B.unpack s) :: ASSheet
  in runRedis conn $ do
    Right sheetKeys <- smembers "sheetKeys"
    sheets <- mapM get sheetKeys
    return $ map readSheet sheets

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
        let sheetKey = DU.makeSheetKey . sheetId $ sheet
        TxSuccess _ <- multiExec $ do
            set sheetKey (B.pack . show $ sheet)  -- set the sheet as key-value
            sadd "sheetKeys" [sheetKey]  -- add the sheet key to the set of all sheets
        return ()

clearSheet :: Connection -> ASSheetId -> IO ()
clearSheet conn sid = do
  keys <- map B.pack <$> DU.makeRangeKeysInSheet conn sid
  runRedis conn $ do
    del keys
    del [DU.makeSheetRangesKey sid]
    del [condFormattingRulesKey sid]
  DU.deleteLocsInSheet sid
  -- TODO: also clear undo, redo, and last message (for Ctrl+Y) (Alex 11/20)

-- deletes the sheet only, does not remove from any containing workbooks
deleteSheetUnsafe :: Connection -> ASSheetId -> IO ()
deleteSheetUnsafe conn sid = do
    runRedis conn $ do
        let setKey = DU.makeSheetSetKey sid
            sheetKey = DU.makeSheetKey sid

        mlocKeys <- smembers setKey
        TxSuccess _ <- multiExec $ do
            case mlocKeys of
                Right []      -> return () -- hedis can't delete empty list
                Right locKeys -> del locKeys >> return ()
                Left _        -> return ()
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
  let vLocs = map cellLocation $ filter ((hasProp VolatileProp) . cellProps) cells
  let locStrs = map (B.pack . show) vLocs
  sadd "volatileLocs" locStrs
  return ()

deleteChunkVolatileCells :: [ASCell] -> Redis ()
deleteChunkVolatileCells cells = do
  let vLocs = map cellLocation $ filter ((hasProp VolatileProp) . cellProps) cells
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
    Just someSheet -> return $ hasPermissions uid (sheetPermissions someSheet)

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
  PayloadProp _ rng -> canAccessAll conn uid (rangeToIndices rng)
  _ -> return True


----------------------------------------------------------------------------------------------------------------------------------------------
-- Repeat handlers

lastMessageKey :: CommitSource -> B.ByteString
lastMessageKey src = B.pack ("LASTMESSAGE" ++ show src)

storeLastMessage :: Connection -> ASClientMessage -> CommitSource -> IO () 
storeLastMessage conn msg src = case (clientAction msg) of 
  Repeat -> return ()
  _ -> runRedis conn (set (lastMessageKey src) (B.pack $ show msg)) >> return ()

getLastMessage :: Connection -> CommitSource -> IO ASClientMessage
getLastMessage conn src = runRedis conn $ do 
  msg <- get $ lastMessageKey src
  return $ case msg of 
    Right (Just msg') -> read (B.unpack msg')
    _ -> ClientMessage NoAction (PayloadN ())

----------------------------------------------------------------------------------------------------------------------------------------------
-- Conditional formatting handlers

condFormattingRulesKey :: ASSheetId -> B.ByteString
condFormattingRulesKey sid = B.pack ("CONDFORMATTINGRULES" ++ (show sid))

getCondFormattingRules :: Connection -> ASSheetId -> IO [CondFormatRule] 
getCondFormattingRules conn sid = runRedis conn $ do 
  msg <- get $ condFormattingRulesKey sid
  return $ case msg of 
    Right (Just msg') -> read (B.unpack msg')
    Right Nothing     -> []
    Left _            -> error "Failed to retrieve conditional formatting rules"

setCondFormattingRules :: Connection -> ASSheetId -> [CondFormatRule] -> IO ()
setCondFormattingRules conn sid rules = runRedis conn (set (condFormattingRulesKey sid) (B.pack $ show rules)) >> return ()