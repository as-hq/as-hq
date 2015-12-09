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
-- Cells

getCell :: ASIndex -> IO (Maybe ASCell)
getCell loc = head <$> getCells [loc]

getPossiblyBlankCell :: ASIndex -> IO ASCell
getPossiblyBlankCell loc = head <$> getPossiblyBlankCells [loc]

-- this function is order-preserving
getCells :: [ASIndex] -> IO [Maybe ASCell]
getCells [] = return []
getCells locs = DI.getCellsByMessage msg num
  where
    msg = DI.showB $ intercalate msgPartDelimiter $ map show2 locs
    num = length locs

-- looks up cells in the given context, then in the database, in that precedence order
-- this function is order-preserving
getCellsWithContext :: EvalContext -> [ASIndex] -> IO [Maybe ASCell]
getCellsWithContext (EvalContext mp _ _) locs = map replaceWithContext <$> zip locs <$> getCells locs
  where
    replaceWithContext (l, c) = case (M.lookup l mp) of 
      Just foundCell -> Just foundCell
      Nothing -> c

getCellsInSheet :: Connection -> ASSheetId -> IO [ASCell]
getCellsInSheet conn sid = DI.getCellsByKeyPattern conn $ "I/" ++ (T.unpack sid) ++ "/(*,*)"

getAllCells :: Connection -> IO [ASCell]
getAllCells conn = DI.getCellsByKeyPattern conn "I/*/(*,*)"

-- Gets the cells at the locations with expressions and values removed, but tags intact. 
-- this function is order-preserving
getBlankedCellsAt :: [ASIndex] -> IO [ASCell]
getBlankedCellsAt locs = 
  let blank xp = case xp of 
            Expression _ lang -> Expression "" lang
            Coupled _ lang _ _ -> Expression "" lang
  in do 
    cells <- getPossiblyBlankCells locs
    return $ map (\(Cell l xp v ts) -> Cell l (blank xp) NoValue ts) cells

-- this function is order-preserving
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
setCells cells = DI.setCellsByMessage msg num
  where 
    str = intercalate msgPartDelimiter $ (map (show2 . cellLocation) cells) ++ (map show2 cells)
    msg = DI.showB str
    num = length cells

deleteLocs :: Connection -> [ASIndex] -> IO ()
deleteLocs _ [] = return ()
deleteLocs conn locs = runRedis conn $ mapM_ DI.deleteLocRedis locs

refToIndices :: ASReference -> EitherTExec [ASIndex]
refToIndices (IndexRef i) = return [i]
refToIndices (RangeRef r) = return $ rangeToIndices r
refToIndices (PointerRef p) = do
  let index = pointerToIndex p 
  cell <- lift $ getCell index 
  case cell of
    Nothing -> left IndexOfPointerNonExistant
    Just cell' -> case (cellToRangeKey cell') of
        Nothing -> left PointerToNormalCell
        Just rKey -> return $ rangeKeyToIndices rKey

-- converts ref to indices using the evalContext, then the DB, in that order.
-- because our evalContext might contain information the DB doesn't (e.g. decoupling)
-- so in the pointer case, we need to check the evalContext first for changes that might have happened during eval
refToIndicesWithContextDuringEval :: EvalContext -> ASReference -> EitherTExec [ASIndex]
refToIndicesWithContextDuringEval _ (IndexRef i) = return [i]
refToIndicesWithContextDuringEval _ (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextDuringEval (EvalContext mp _ _) (PointerRef p) = do
  let index = pointerToIndex p
  case (M.lookup index mp) of 
    Just (Cell _ (Coupled _ _ _ rKey) _ _) -> return $ rangeKeyToIndices rKey
    Just (Cell _ (Expression _ _) _ _) -> left $ PointerToNormalCell
    Nothing -> do
      cell <- lift $ getCell index 
      case cell of
        Nothing -> left IndexOfPointerNonExistant
        Just cell' -> case (cellToRangeKey cell') of
            Nothing -> left PointerToNormalCell
            Just rKey -> return $ rangeKeyToIndices rKey

-- This is the function we use to convert ref to indices for updating the map PRIOR TO eval. There are some cases where we don't flip a shit. 
-- For example, if the map currently has A1 as a normal expression, and we have @A1 somewhere downstream, we won't flip a shit, and instead expect that
-- by the time the pointer is evalled, A1 will have a coupled expression due to toposort. We flip a shit if it's not the case then. 
refToIndicesWithContextBeforeEval :: EvalContext -> ASReference -> IO [ASIndex]
refToIndicesWithContextBeforeEval _ (IndexRef i) = return [i]
refToIndicesWithContextBeforeEval _ (RangeRef r) = return $ rangeToIndices r
refToIndicesWithContextBeforeEval (EvalContext mp _ _) (PointerRef p) = do
  let index = pointerToIndex p
  case (M.lookup index mp) of 
    Just (Cell _ (Coupled _ _ _ rKey) _ _) -> return $ rangeKeyToIndices rKey
    Just (Cell _ (Expression _ _) _ _) -> return $ []
    Nothing -> do
      cell <- getCell index 
      case cell of
        Nothing -> return $ []
        Just cell' -> case (cellToRangeKey cell') of
            Nothing -> return $ []
            Just rKey -> return $ rangeKeyToIndices rKey



----------------------------------------------------------------------------------------------------------------------
-- Locations

locationsExist :: Connection -> [ASIndex] -> IO [Bool]
locationsExist conn locs = runRedis conn $ map fromRight <$> mapM locExists locs
  where
    fromRight (Right a) = a
    locExists l         = exists $ makeLocationKey l

locationExists :: Connection -> ASIndex -> IO Bool
locationExists conn loc = head <$> locationsExist conn [loc] 

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
  Right desc <- get (B.pack . show2 $ key)
  return $ DI.bStrToRangeDescriptor desc

getRangeDescriptorsInSheet :: Connection -> ASSheetId -> IO [RangeDescriptor]
getRangeDescriptorsInSheet conn sid = do
  keys <- DI.getRangeKeysInSheet conn sid
  map fromJust <$> mapM (getRangeDescriptor conn) keys

getRangeDescriptorsInSheetWithContext :: Connection -> EvalContext -> ASSheetId -> IO [RangeDescriptor]
getRangeDescriptorsInSheetWithContext conn ctx@(EvalContext _ _ ddiff) sid = do
  putStrLn $ "removed descriptors in getRangeDescriptorsInSheetWithContext " ++ (show $ removedDescriptors ddiff)
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
        return $ map (\(Right (Just w)) -> read (B.unpack w) :: ASWorkbook) wbs

setWorkbook :: Connection -> ASWorkbook -> IO ()
setWorkbook conn wb = do
    runRedis conn $ do
        let workbookKey = makeWorkbookKey . workbookName $ wb
        TxSuccess _ <- multiExec $ do
            set workbookKey (B.pack . show $ wb)  -- set the workbook as key-value
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

-- note: this is an expensive operation
deleteWorkbookAndSheets :: Connection -> String -> IO ()
deleteWorkbookAndSheets conn name = do
    mwb <- getWorkbook conn name
    case mwb of
        Nothing -> return ()
        Just wb -> do
            mapM_ (deleteSheetUnsafe conn) (workbookSheets wb) -- remove sheets
            runRedis conn $ do
                let workbookKey = makeWorkbookKey name
                TxSuccess _ <- multiExec $ do
                    del [workbookKey]   -- remove workbook from key-value
                    srem "workbookKeys" [workbookKey] -- remove workbook from set
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
  return $ DI.getUniquePrefixedName "Sheet" $ map sheetName ss

setSheet :: Connection -> ASSheet -> IO ()
setSheet conn sheet = do
    runRedis conn $ do
        let sheetKey = makeSheetKey . sheetId $ sheet
        TxSuccess _ <- multiExec $ do
            set sheetKey (B.pack . show $ sheet)  -- set the sheet as key-value
            sadd "sheetKeys" [sheetKey]  -- add the sheet key to the set of all sheets
        return ()

clearSheet :: Connection -> ASSheetId -> IO ()
clearSheet conn sid = do
  let headerLangs = [Python, R] -- should REALLY put this in a Constants file
  keys <- map (B.pack . show2) <$> DI.getRangeKeysInSheet conn sid
  runRedis conn $ do
    del keys
    del [makeSheetRangesKey sid]
    del [condFormattingRulesKey sid]
    mapM (\l -> del [evalHeaderKey sid l]) headerLangs
  DI.deleteLocsInSheet sid
  -- TODO: also clear undo, redo, and last message (for Ctrl+Y) (Alex 11/20)

-- deletes the sheet only, does not remove from any containing workbooks
deleteSheetUnsafe :: Connection -> ASSheetId -> IO ()
deleteSheetUnsafe conn sid = do
    runRedis conn $ do
        let setKey = makeSheetSetKey sid
            sheetKey = makeSheetKey sid

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
      return $ map DI.bStrToASIndex vl

-- TODO: some of the cells may change from volatile -> not volatile, but they're still in volLocs
setChunkVolatileCells :: [ASCell] -> Redis ()
setChunkVolatileCells cells = do
  let vLocs = map cellLocation $ filter ((hasPropType VolatileProp) . cellProps) cells
  let locStrs = map (B.pack . show) vLocs
  sadd "volatileLocs" locStrs
  return ()

deleteChunkVolatileCells :: [ASCell] -> Redis ()
deleteChunkVolatileCells cells = do
  let vLocs = map cellLocation $ filter ((hasPropType VolatileProp) . cellProps) cells
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

----------------------------------------------------------------------------------------------------------------------------------------------
-- Header expressions handlers

evalHeaderKey :: ASSheetId -> ASLanguage -> B.ByteString
evalHeaderKey sid lang = B.pack ("EVALHEADER" ++ (show sid) ++ (show lang)) 

getEvalHeader :: Connection -> ASSheetId -> ASLanguage -> IO String
getEvalHeader conn sid lang = runRedis conn $ do 
  msg <- get $ evalHeaderKey sid lang
  return $ case msg of 
    Right (Just msg') -> B.unpack msg'
    Right Nothing -> ""
    Left _            -> error "Failed to retrieve eval header"

setEvalHeader :: Connection -> ASSheetId -> ASLanguage -> String -> IO ()
setEvalHeader conn sid lang xp = runRedis conn (set (evalHeaderKey sid lang) (B.pack xp)) >> return ()

----------------------------------------------------------------------------------------------------------------------------------------------
-- Row/col getters/setters

-- #needsrefactor the keys here are horrendous. 

rowColPropsKey :: ASSheetId -> RP.RowColType -> Int -> B.ByteString
rowColPropsKey sid rct ind = B.pack ((show rct) ++ "PROPS" ++ (show sid) ++ '`':(show ind)) 

getIndFromRowColPropsKey :: B.ByteString -> Int
getIndFromRowColPropsKey = read . last . (splitOn "`") . B.unpack

-- TODO: eventually should extend to record generic row/col formats, like default font in the column, 
-- rather than just its width. 
getRowColProps :: Connection -> ASSheetId -> RP.RowColType -> Int -> IO (Maybe RP.ASRowColProps)
getRowColProps conn sid rct ind  = runRedis conn $ do 
  msg <- get $ rowColPropsKey sid rct ind
  return $ case msg of 
    Right (Just msg') -> Just $ read $ B.unpack msg'
    Right Nothing     -> Nothing
    Left _            -> error "Failed to retrieve row or column props"

-- #needsrefactor the hard-coding, and the use of keys in hedis, are not great. 
getRowColsInSheet :: Connection -> ASSheetId -> IO [RP.RowCol]
getRowColsInSheet conn sid = do 
  mColKeys <- runRedis conn (keys $ B.pack $ (show RP.ColumnType) ++ "PROPS" ++ (show sid) ++ "*")
  mRowKeys <- runRedis conn (keys $ B.pack $ (show RP.RowType) ++ "PROPS" ++ (show sid) ++ "*")
  let colInds = either (error "Failed to retrieve eval header") (map getIndFromRowColPropsKey) mColKeys
      rowInds = either (error "Failed to retrieve eval header") (map getIndFromRowColPropsKey) mRowKeys
  colProps <- mapM (getRowColProps conn sid RP.ColumnType) colInds
  rowProps <- mapM (getRowColProps conn sid RP.RowType) rowInds
  let cols = map (\(x, Just y) -> RP.RowCol RP.ColumnType x y) $ filter (isJust . snd) $ zip colInds colProps
      rows = map (\(x, Just y) -> RP.RowCol RP.RowType x y) $ filter (isJust . snd) $ zip rowInds rowProps
  return $ union cols rows

deleteRowColsInSheet :: Connection -> ASSheetId -> IO ()
deleteRowColsInSheet conn sid = do
  mColKeys <- runRedis conn (keys $ B.pack $ (show RP.ColumnType) ++ "PROPS" ++ (show sid) ++ "*")
  mRowKeys <- runRedis conn (keys $ B.pack $ (show RP.RowType) ++ "PROPS" ++ (show sid) ++ "*")
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
  runRedis conn (set (rowColPropsKey sid rct ind) (B.pack $ show props))
  return ()
