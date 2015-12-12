{-# LANGUAGE OverloadedStrings #-}

module AS.DB.API where

import Prelude
import AS.Types.Cell
import qualified AS.Types.RowColProps as RP
import AS.Types.Messages
import AS.Types.DB
import AS.Types.CellProps
import AS.Types.Errors
import AS.Types.Eval

import AS.Util as U
import qualified AS.DB.Internal as DI
import AS.Parsing.Substitutions (getDependencies)
import AS.Window
import AS.Logging

import Data.List (zip4,head,nub,intercalate)
import Data.Maybe 
import Data.List.Split
import qualified Data.Map as M

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
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.Serialize as S
import Data.List as L
import Data.Aeson hiding (Success)
import Data.ByteString.Unsafe as BU
import Data.List.Split

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
----------------------------------------------------------------------------------------------------------------------
-- Storage Documentation

-- | Cells
-- key-value hashes
-- key is produced by cellLocation (see makeLocationKey) and is unique
-- fields are "cellExpression", "cellValue", "cellTags" with corresponding stringified values

-- | DAG
-- same as before: a set of relations
-- access set with key "DAGLocSet"

-- | Sheets
-- stored as key (DI.makeSheetKey ASSheetId) value (stringified ASSheet)
-- additionally, the set of all locations belonging to a sheet are stored as
-- set key (DI.makeSheetSetKey ASSheetId) members (ASIndexKey)
-- this set is updated automatically during setCells.
-- finally, a record of all sheetKeys is stored as a set with key "sheets" and members (DI.makeSheetKey sheetid)

-- | Workbooks
-- stored identically to Sheets

-- | Volatile locs
-- stored as before, as a set with key volatileLocs

clear :: Connection -> IO ()
clear conn = runRedis conn $ flushall >> return ()

----------------------------------------------------------------------------------------------------------------------
-- Raw cells API
-- all of these functions are order-preserving unless noted otherwise.

getCells :: Connection -> [ASIndex] -> IO [Maybe ASCell]
getCells _ [] = return []
getCells conn locs = runRedis conn $ do
  sCells <- map fromRight <$> mapM (get . S.encode) locs
  return $ map deserializeResult sCells
    where 
      deserializeResult ms = case ms of
        Nothing -> Nothing
        Just s -> decodeMaybe s

setCells :: Connection -> [ASCell] -> IO ()
setCells _ [] = return ()
setCells conn cs = runRedis conn $ do
  mapM_ (\c -> set (S.encode . cellLocation $ c) (S.encode c)) cs
  let sheetIds = nub' $ map (locSheetId . cellLocation) cs
  sadd allSheetsKey $ map S.encode sheetIds
  return ()

deleteLocs :: Connection -> [ASIndex] -> IO ()
deleteLocs _ [] = return ()
deleteLocs conn locs = runRedis conn $ del (map S.encode locs) >> return ()

----------------------------------------------------------------------------------------------------------------------
-- Additional cell API methods

getCell :: Connection -> ASIndex -> IO (Maybe ASCell)
getCell conn loc = head <$> getCells conn [loc]

setCell :: Connection -> ASCell -> IO ()
setCell conn c = setCells conn [c]

getPossiblyBlankCell :: Connection -> ASIndex -> IO ASCell
getPossiblyBlankCell conn loc = head <$> getPossiblyBlankCells conn [loc]

getCellsInSheet :: Connection -> ASSheetId -> IO [ASCell]
getCellsInSheet conn sid = DI.getCellsByKeyPattern conn $ BC.pack $ "*" ++ (T.unpack sid) ++ "*"

getAllCells :: Connection -> IO [ASCell]
getAllCells conn = DI.getCellsByKeyPattern conn "*"

-- Gets the cells at the locations with expressions and values removed, but tags intact. 
-- this function is order-preserving
getBlankedCellsAt :: Connection -> [ASIndex] -> IO [ASCell]
getBlankedCellsAt conn locs = 
  let blank xp = case xp of 
            Expression _ lang -> Expression "" lang
            Coupled _ lang _ _ -> Expression "" lang
  in do 
    cells <- getPossiblyBlankCells conn locs
    return $ map (\(Cell l xp v ts) -> Cell l (blank xp) NoValue ts) cells

-- this function is order-preserving
getPossiblyBlankCells :: Connection -> [ASIndex] -> IO [ASCell]
getPossiblyBlankCells conn locs = do 
  cells <- getCells conn locs
  return $ map (\(l,c) -> case c of 
    Just c' -> c'
    Nothing -> blankCellAt l) (zip locs cells)

getPropsAt :: Connection -> [ASIndex] -> IO [ASCellProps]
getPropsAt conn locs = do 
  cells <- getPossiblyBlankCells conn locs
  return $ map cellProps cells

----------------------------------------------------------------------------------------------------------------------
-- Locations

locationsExist :: Connection -> [ASIndex] -> IO [Bool]
locationsExist conn locs = runRedis conn $ map fromRight <$> mapM (exists . S.encode) locs

locationExists :: Connection -> ASIndex -> IO Bool
locationExists conn loc = head <$> locationsExist conn [loc] 

----------------------------------------------------------------------------------------------------------------------
-- Fat cells

-- | Returns the listkeys of all the lists that are entirely contained in the range.  
fatCellsInRange :: Connection -> ASRange -> IO [RangeKey]
fatCellsInRange conn rng = do
  let sid = rangeSheetId rng
  rangeKeys <- DI.getRangeKeysInSheet conn sid
  let rects = map rangeRect rangeKeys
      zipRects = zip rangeKeys rects
      zipRectsContained = filter (\(_,rect) -> rangeContainsRect rng rect) zipRects
  return $ map fst zipRectsContained

getRangeDescriptor :: Connection -> RangeKey -> IO (Maybe RangeDescriptor)
getRangeDescriptor conn key = runRedis conn $ do 
  Right desc <- get (BC.pack . show2 $ key)
  return $ DI.bStrToRangeDescriptor desc

getRangeDescriptorsInSheet :: Connection -> ASSheetId -> IO [RangeDescriptor]
getRangeDescriptorsInSheet conn sid = do
  keys <- DI.getRangeKeysInSheet conn sid
  map fromJust <$> mapM (getRangeDescriptor conn) keys

getRangeDescriptorsInSheetWithContext :: Connection -> EvalContext -> ASSheetId -> IO [RangeDescriptor]
getRangeDescriptorsInSheetWithContext conn ctx@(EvalContext _ _ ddiff) sid = do
  printObj "removed descriptors in getRangeDescriptorsInSheetWithContext " $ removedDescriptors ddiff
  dbKeys <- DI.getRangeKeysInSheet conn sid
  let dbKeys' = dbKeys \\ (map descriptorKey $ removedDescriptors ddiff)
  dbDescriptors <- map fromJust <$> mapM (getRangeDescriptor conn) dbKeys' 
  return $ (addedDescriptors ddiff) ++ dbDescriptors

-- If the range descriptor associated with a range key is in the context, return it. Else, return Nothing. 
getRangeDescriptorUsingContext :: Connection -> EvalContext -> RangeKey -> IO (Maybe RangeDescriptor)
getRangeDescriptorUsingContext conn (EvalContext _ _ ddiff) rKey = if (isJust inRemoved)
  then return Nothing 
  else case inAdded of
    Nothing -> getRangeDescriptor conn rKey
    Just d -> return $ Just d
  where
    inRemoved = find (\d -> descriptorKey d == rKey) (removedDescriptors ddiff)
    inAdded = find (\d -> descriptorKey d == rKey) (addedDescriptors ddiff)

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
  return $ DI.getUniquePrefixedName "Workbook" $ map workbookName wbs

getWorkbook :: Connection -> String -> IO (Maybe ASWorkbook)
getWorkbook conn name = do
    runRedis conn $ do
        mwb <- get $ makeWorkbookKey name
        case mwb of
            Right wb -> return $ DI.bStrToWorkbook wb
            Left _   -> return Nothing

getAllWorkbooks :: Connection -> IO [ASWorkbook]
getAllWorkbooks conn = do
    runRedis conn $ do
        Right wbKeys <- smembers "workbookKeys"
        wbs <- mapM get wbKeys
        return $ map (\(Right (Just w)) -> read (BC.unpack w) :: ASWorkbook) wbs

setWorkbook :: Connection -> ASWorkbook -> IO ()
setWorkbook conn wb = do
    runRedis conn $ do
        let workbookKey = makeWorkbookKey . workbookName $ wb
        TxSuccess _ <- multiExec $ do
            set workbookKey (BC.pack . show $ wb)  -- set the workbook as key-value
            sadd "workbookKeys" [workbookKey]  -- add the workbook key to the set of all sheets
        return ()

workbookExists :: Connection -> String -> IO Bool
workbookExists conn wName = do
  runRedis conn $ do
    Right result <- exists $ makeWorkbookKey wName
    return result

-- only removes the workbook, not contained sheets
deleteWorkbook :: Connection -> String -> IO ()
deleteWorkbook conn name = do
    runRedis conn $ do
        let workbookKey = makeWorkbookKey name
        multiExec $ do
          del [workbookKey]
          srem "workbookKeys" [workbookKey]
        return ()

----------------------------------------------------------------------------------------------------------------------
-- Raw sheets

getSheet :: Connection -> ASSheetId -> IO (Maybe ASSheet)
getSheet conn sid = do
    runRedis conn $ do
        msheet <- get $ makeSheetKey sid
        case msheet of
            Right sheet -> return $ DI.bStrToSheet sheet
            Left _      -> return Nothing

getAllSheets :: Connection -> IO [ASSheet]
getAllSheets conn = 
  let readSheet (Right (Just s)) = read (BC.unpack s) :: ASSheet
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
  return $ DI.getUniquePrefixedName "Sheet" $ map sheetName ss

setSheet :: Connection -> ASSheet -> IO ()
setSheet conn sheet = do
    runRedis conn $ do
        let sheetKey = makeSheetKey . sheetId $ sheet
        TxSuccess _ <- multiExec $ do
            set sheetKey (BC.pack . show $ sheet)  -- set the sheet as key-value
            sadd "sheetKeys" [sheetKey]  -- add the sheet key to the set of all sheets
        return ()

clearSheet :: Connection -> ASSheetId -> IO ()
clearSheet conn sid = do
  let headerLangs = [Python, R] -- should REALLY put this in a Constants file
  keys <- map (BC.pack . show2) <$> DI.getRangeKeysInSheet conn sid
  runRedis conn $ do
    del keys
    del [makeSheetRangesKey sid]
    del [condFormattingRulesKey sid]
    mapM (\l -> del [makeEvalHeaderKey sid l]) headerLangs
  DI.deleteLocsInSheet sid
  -- TODO: also clear undo, redo, and last message (for Ctrl+Y) (Alex 11/20)

----------------------------------------------------------------------------------------------------------------------
-- Volatile cell methods

getVolatileLocs :: Connection -> IO [ASIndex]
getVolatileLocs conn = do
  runRedis conn $ do
      Right vl <- smembers "volatileLocs"
      return $ map DI.bStrToASIndex vl

-- TODO: some of the cells may change from volatile -> not volatile, but they're still in volLocs
setChunkVolatileCells :: [ASCell] -> Redis ()
setChunkVolatileCells cells = do
  let vLocs = map cellLocation $ filter ((hasPropType VolatileProp) . cellProps) cells
  let locStrs = map (BC.pack . show) vLocs
  sadd "volatileLocs" locStrs
  return ()

deleteChunkVolatileCells :: [ASCell] -> Redis ()
deleteChunkVolatileCells cells = do
  let vLocs = map cellLocation $ filter ((hasPropType VolatileProp) . cellProps) cells
  let locStrs = map (BC.pack . show) vLocs
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
lastMessageKey src = BC.pack ("LASTMESSAGE" ++ show src)

storeLastMessage :: Connection -> ASClientMessage -> CommitSource -> IO () 
storeLastMessage conn msg src = case (clientAction msg) of 
  Repeat -> return ()
  _ -> runRedis conn (set (lastMessageKey src) (BC.pack $ show msg)) >> return ()

getLastMessage :: Connection -> CommitSource -> IO ASClientMessage
getLastMessage conn src = runRedis conn $ do 
  msg <- get $ lastMessageKey src
  return $ case msg of 
    Right (Just msg') -> read (BC.unpack msg')
    _ -> ClientMessage NoAction (PayloadN ())

----------------------------------------------------------------------------------------------------------------------------------------------
-- Conditional formatting handlers

condFormattingRulesKey :: ASSheetId -> B.ByteString
condFormattingRulesKey sid = BC.pack ("CONDFORMATTINGRULES" ++ (show sid))

getCondFormattingRules :: Connection -> ASSheetId -> IO [CondFormatRule] 
getCondFormattingRules conn sid = runRedis conn $ do 
  msg <- get $ condFormattingRulesKey sid
  return $ case msg of 
    Right (Just msg') -> read (BC.unpack msg')
    Right Nothing     -> []
    Left _            -> error "Failed to retrieve conditional formatting rules"

setCondFormattingRules :: Connection -> ASSheetId -> [CondFormatRule] -> IO ()
setCondFormattingRules conn sid rules = runRedis conn (set (condFormattingRulesKey sid) (BC.pack $ show rules)) >> return ()

----------------------------------------------------------------------------------------------------------------------------------------------
-- Row/col getters/setters

-- #needsrefactor the keys here are horrendous. 

rowColPropsKey :: ASSheetId -> RP.RowColType -> Int -> B.ByteString
rowColPropsKey sid rct ind = BC.pack ((show rct) ++ "PROPS" ++ (show sid) ++ '`':(show ind)) 

getIndFromRowColPropsKey :: B.ByteString -> Int
getIndFromRowColPropsKey = read . last . (splitOn "`") . BC.unpack

-- TODO: eventually should extend to record generic row/col formats, like default font in the column, 
-- rather than just its width. 
getRowColProps :: Connection -> ASSheetId -> RP.RowColType -> Int -> IO (Maybe RP.ASRowColProps)
getRowColProps conn sid rct ind  = runRedis conn $ do 
  msg <- get $ rowColPropsKey sid rct ind
  return $ case msg of 
    Right (Just msg') -> Just $ read $ BC.unpack msg'
    Right Nothing     -> Nothing
    Left _            -> error "Failed to retrieve row or column props"

-- #needsrefactor the hard-coding, and the use of keys in hedis, are not great. 
getRowColsInSheet :: Connection -> ASSheetId -> IO [RP.RowCol]
getRowColsInSheet conn sid = do 
  mColKeys <- runRedis conn (keys $ BC.pack $ (show RP.ColumnType) ++ "PROPS" ++ (show sid) ++ "*")
  mRowKeys <- runRedis conn (keys $ BC.pack $ (show RP.RowType) ++ "PROPS" ++ (show sid) ++ "*")
  let colInds = either (error "Failed to retrieve eval header") (map getIndFromRowColPropsKey) mColKeys
      rowInds = either (error "Failed to retrieve eval header") (map getIndFromRowColPropsKey) mRowKeys
  colProps <- mapM (getRowColProps conn sid RP.ColumnType) colInds
  rowProps <- mapM (getRowColProps conn sid RP.RowType) rowInds
  let cols = map (\(x, Just y) -> RP.RowCol RP.ColumnType x y) $ filter (isJust . snd) $ zip colInds colProps
      rows = map (\(x, Just y) -> RP.RowCol RP.RowType x y) $ filter (isJust . snd) $ zip rowInds rowProps
  return $ union cols rows

deleteRowColsInSheet :: Connection -> ASSheetId -> IO ()
deleteRowColsInSheet conn sid = do
  mColKeys <- runRedis conn (keys $ BC.pack $ (show RP.ColumnType) ++ "PROPS" ++ (show sid) ++ "*")
  mRowKeys <- runRedis conn (keys $ BC.pack $ (show RP.RowType) ++ "PROPS" ++ (show sid) ++ "*")
  let colKeys = case mColKeys of
                     Right ck' -> ck'
                     Left _ -> error "Failed to retrieve row props in delete rowcols"
      rowKeys = case mRowKeys of
                     Right ck' -> ck'
                     Left _ -> error "Failed to retrieve row props in delete rowcols"
  runRedis conn $ del (colKeys ++ rowKeys)
  return()

setRowColProps :: Connection -> ASSheetId -> RP.RowCol -> IO ()
setRowColProps conn sid (RP.RowCol rct ind props) = do
  runRedis conn (set (rowColPropsKey sid rct ind) (BC.pack $ show props))
  return ()
