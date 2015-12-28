{-# LANGUAGE OverloadedStrings, DataKinds, GADTs #-}

module AS.DB.API where

import Prelude
import AS.Types.Cell
import AS.Types.Bar
import AS.Types.BarProps (BarProp, ASBarProps) 
import AS.Types.Messages
import AS.Types.DB
import AS.Types.CellProps
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.CondFormat
import AS.Types.Updates

import qualified AS.Config.Settings as Settings
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
  return $ map (maybeDecode =<<) sCells

setCells :: Connection -> [ASCell] -> IO ()
setCells _ [] = return ()
setCells conn cs = runRedis conn $ mapM_ (\c -> set (S.encode . cellLocation $ c) (S.encode c)) cs

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
getCellsInSheet conn sid = getCellsByKeyPattern conn $ "*" ++ (T.unpack sid) ++ "*"

getAllCells :: Connection -> IO [ASCell]
getAllCells conn = getCellsByKeyPattern conn "*"

deleteLocsInSheet :: Connection -> ASSheetId -> IO ()
deleteLocsInSheet conn sid = runRedis conn $ do
  Right ks <- keys $ BC.pack $ "*" ++ (T.unpack sid) ++ "*"
  let locKeys = catMaybes $ map readLocKey ks
      readLocKey k = case (maybeDecode k) of 
        Just (Index _ _) -> Just k
        _ -> Nothing
  del locKeys
  return ()

getCellsByKeyPattern :: Connection -> String -> IO [ASCell]
getCellsByKeyPattern conn pattern = do
  ks <- DI.getKeysByPattern conn pattern
  let locs = catMaybes $ map readLoc ks
      readLoc k = case (maybeDecode k) of 
        Just l@(Index _ _) -> Just l
        _ -> Nothing
  return locs
  map fromJust <$> getCells conn locs

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

getPropsAt :: Connection -> ASIndex -> IO ASCellProps
getPropsAt conn ind = cellProps <$> getPossiblyBlankCell conn ind

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
  Right desc <- get . toRedisFormat $ RedisRangeKey key
  return $ maybeDecode =<< desc

getRangeDescriptorsInSheet :: Connection -> ASSheetId -> IO [RangeDescriptor]
getRangeDescriptorsInSheet conn sid = do
  keys <- DI.getRangeKeysInSheet conn sid
  map fromJust <$> mapM (getRangeDescriptor conn) keys

getRangeDescriptorsInSheetWithContext :: Connection -> EvalContext -> ASSheetId -> IO [RangeDescriptor]
getRangeDescriptorsInSheetWithContext conn ctx sid = do -- #lens
  dbKeys <- DI.getRangeKeysInSheet conn sid
  let dbKeys' = dbKeys \\ (oldRangeKeysInContext ctx)
  dbDescriptors <- map fromJust <$> mapM (getRangeDescriptor conn) dbKeys' 
  return $ (newRangeDescriptorsInContext ctx) ++ dbDescriptors

-- If the range descriptor associated with a range key is in the context, return it. Else, return Nothing. 
getRangeDescriptorUsingContext :: Connection -> EvalContext -> RangeKey -> IO (Maybe RangeDescriptor)
getRangeDescriptorUsingContext conn ctx rKey = if (isJust inRemoved) -- #lens
  then return Nothing 
  else case inAdded of
    Nothing -> getRangeDescriptor conn rKey
    Just d -> return $ Just d
  where
    inRemoved = find (\d -> d == rKey) (oldRangeKeysInContext ctx)
    inAdded = find (\d -> descriptorKey d == rKey) (newRangeDescriptorsInContext ctx)

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
    Just wb -> do
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

getUniqueWbName :: Connection -> IO WorkbookName
getUniqueWbName conn = do
  wbs <- getAllWorkbooks conn
  return $ DI.getUniquePrefixedName "Workbook" $ map workbookName wbs

getWorkbook :: Connection -> WorkbookName -> IO (Maybe ASWorkbook)
getWorkbook conn name = do
    runRedis conn $ do
        mwb <- get . toRedisFormat $ WorkbookKey name
        case mwb of
            Right wb -> return $ DI.bStrToWorkbook wb
            Left _   -> return Nothing

getAllWorkbooks :: Connection -> IO [ASWorkbook]
getAllWorkbooks conn = runRedis conn $ do
  Right wbKeys <- smembers . toRedisFormat $ AllWorkbooksKey
  wbs <- mapM get wbKeys
  return $ map (\(Right (Just w)) -> read (BC.unpack w) :: ASWorkbook) wbs

setWorkbook :: Connection -> ASWorkbook -> IO ()
setWorkbook conn wb = runRedis conn $ do
  let workbookKey = toRedisFormat . WorkbookKey . workbookName $ wb
  TxSuccess _ <- multiExec $ do
      set workbookKey (BC.pack . show $ wb)  -- set the workbook as key-value
      sadd (toRedisFormat AllWorkbooksKey) [workbookKey]  -- add the workbook key to the set of all sheets
  return ()

workbookExists :: Connection -> WorkbookName -> IO Bool
workbookExists conn wName = runRedis conn $ do
  Right result <- exists . toRedisFormat $ WorkbookKey wName
  return result

-- only removes the workbook, not contained sheets
deleteWorkbook :: Connection -> WorkbookName -> IO ()
deleteWorkbook conn name = runRedis conn $ do
  let workbookKey = toRedisFormat $ WorkbookKey name
  multiExec $ do
    del [workbookKey]
    srem (toRedisFormat AllWorkbooksKey) [workbookKey]
  return ()

----------------------------------------------------------------------------------------------------------------------
-- Raw sheets

getSheet :: Connection -> ASSheetId -> IO (Maybe ASSheet)
getSheet conn sid = runRedis conn $ do
  Right sheet <- get . toRedisFormat $ SheetKey sid
  return $ DI.bStrToSheet sheet

getAllSheets :: Connection -> IO [ASSheet]
getAllSheets conn = 
  let readSheet (Right (Just s)) = read (BC.unpack s) :: ASSheet
  in runRedis conn $ do
    Right sheetKeys <- smembers (toRedisFormat AllSheetsKey)
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
        let sheetKey = toRedisFormat . SheetKey . sheetId $ sheet
        TxSuccess _ <- multiExec $ do
            set sheetKey (BC.pack . show $ sheet)  -- set the sheet as key-value
            sadd (toRedisFormat AllSheetsKey) [sheetKey]  -- add the sheet key to the set of all sheets
        return ()

clearSheet :: Connection -> ASSheetId -> IO ()
clearSheet conn sid = 
  let sheetRangesKey = toRedisFormat $ SheetRangesKey sid
      evalHeaderKeys = map (toRedisFormat . (EvalHeaderKey sid)) Settings.headerLangs
      rangeKeys = map (toRedisFormat . RedisRangeKey) <$> DI.getRangeKeysInSheet conn sid
      pluralKeyTypes = [BarType2, PushCommitType, PopCommitType, TempCommitType, LastMessageType, CFRuleType]
      pluralKeys = (evalHeaderKeys ++) . concat <$> mapM (DI.getKeysInSheetByType conn sid) pluralKeyTypes
  in runRedis conn $ do
    pluralKeys <- liftIO pluralKeys
    del $ sheetRangesKey : pluralKeys
    liftIO $ deleteLocsInSheet conn sid

----------------------------------------------------------------------------------------------------------------------
-- Volatile cell methods

getVolatileLocs :: Connection -> IO [ASIndex]
getVolatileLocs conn = runRedis conn $ do
  vl <- (map maybeDecode) . fromRight <$> (smembers $ toRedisFormat VolatileLocsKey)
  if any isNothing vl 
    then error "Error decoding volatile locs!!!"
    else return $ map fromJust vl

-- TODO: some of the cells may change from volatile -> not volatile, but they're still in volLocs
setChunkVolatileCells :: [ASCell] -> Redis ()
setChunkVolatileCells cells = do
  let vLocs = map cellLocation $ filter ((hasPropType VolatileProp) . cellProps) cells
  sadd (toRedisFormat VolatileLocsKey) (map S.encode vLocs)
  return ()

deleteChunkVolatileCells :: [ASCell] -> Redis ()
deleteChunkVolatileCells cells = do
  let vLocs = map cellLocation $ filter ((hasPropType VolatileProp) . cellProps) cells
  srem (toRedisFormat VolatileLocsKey) (map S.encode vLocs)
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
canAccessAll conn uid locs = all id <$> mapM (canAccess conn uid) locs

isPermissibleMessage :: ASUserId -> Connection -> ServerMessage -> IO Bool
isPermissibleMessage uid conn _ = return True
-- (ServerMessage _ payload) = case payload of 
--   PayloadCL cells -> canAccessAll conn uid (map cellLocation cells)
--   PayloadLL locs -> canAccessAll conn uid locs
--   PayloadS sheet -> canAccessSheet conn uid (sheetId sheet)
--   PayloadW window -> canAccessSheet conn uid (windowSheetId window)
--   PayloadProp _ rng -> canAccessAll conn uid (rangeToIndices rng)
--   _ -> return True
-- commenting out 12/28 -- Alex


----------------------------------------------------------------------------------------------------------------------------------------------
-- Repeat handlers

storeLastMessage :: Connection -> ServerMessage -> CommitSource -> IO () 
storeLastMessage conn msg src = case (serverAction msg) of 
  Repeat _ -> return ()
  _ -> runRedis conn (set (toRedisFormat $ LastMessageKey src) (S.encode msg)) >> return ()

getLastMessage :: Connection -> CommitSource -> IO ServerMessage
getLastMessage conn src = error "Currently not implemented"
 -- runRedis conn $ do 
 --  msg <- fromRight <$> get (toRedisFormat $ LastMessageKey src)
 --  return $ case (maybeDecode =<< msg) of 
 --    Just m -> m
 --    -- _ -> ServerMessage NoAction (PayloadN ())

----------------------------------------------------------------------------------------------------------------------------------------------
-- Conditional formatting handlers

getCondFormattingRules :: Connection -> ASSheetId -> [CondFormatRuleId] -> IO [CondFormatRule] 
getCondFormattingRules conn sid cfids = do 
  let keys = map (toRedisFormat . CFRuleKey sid) cfids
  eitherMsg <- runRedis conn $ mget keys
  return $ either (const []) (mapMaybe maybeDecode . catMaybes) eitherMsg

-- some replication with the above...
getCondFormattingRulesInSheet :: Connection -> ASSheetId -> IO [CondFormatRule]
getCondFormattingRulesInSheet conn sid = do 
  keys <- DI.getKeysInSheetByType conn sid CFRuleType
  eitherMsg <- runRedis conn $ mget keys
  return $ either (const []) (mapMaybe maybeDecode . catMaybes) eitherMsg

deleteCondFormattingRules :: Connection -> ASSheetId -> [CondFormatRuleId] -> IO ()
deleteCondFormattingRules conn sid cfids = (runRedis conn $ del $ map (toRedisFormat . CFRuleKey sid) cfids) >> return ()

setCondFormattingRules :: Connection -> ASSheetId -> [CondFormatRule] -> IO ()
setCondFormattingRules conn sid rules = runRedis conn $ do
  let cfids        = map condFormatRuleId rules 
      makeKey      = toRedisFormat . CFRuleKey sid 
      keys         = map makeKey cfids
      encodedRules = map S.encode rules 
  mset $ zip keys encodedRules
  return ()

----------------------------------------------------------------------------------------------------------------------------------------------
-- Row/col getters/setters

-- #needsrefactor should be consistent about whether our getters and setters take in a single key or lists of keys.
getBar :: Connection -> BarIndex -> IO (Maybe Bar)
getBar conn bInd  = runRedis conn $ do 
  Right msg <- get . toRedisFormat $ BarKey bInd
  return $ maybeDecode =<< msg

setBar :: Connection -> Bar -> IO ()
setBar conn bar = do
  runRedis conn $ set (toRedisFormat $ BarKey (barIndex bar)) (S.encode bar)
  return ()

deleteBarAt :: Connection -> BarIndex -> IO ()
deleteBarAt conn bInd = do
  runRedis conn $ del [toRedisFormat $ BarKey bInd]
  return ()

replaceBars :: Connection -> [Bar] -> [Bar] -> IO()
replaceBars conn fromBars toBars = do
  mapM_ ((deleteBarAt conn) . barIndex) fromBars
  mapM_ (setBar conn) toBars

getBarsInSheet :: Connection -> ASSheetId -> IO [Bar]
getBarsInSheet conn sid = do 
  let readKey k = read2 (BC.unpack k) :: RedisKey BarType2
      extractInd :: RedisKey BarType2 -> BarIndex
      extractInd (BarKey ind) = ind
  inds <- map (extractInd . readKey) <$> DI.getKeysInSheetByType conn sid BarType2
  catMaybes <$> mapM (getBar conn) inds

deleteBarsInSheet :: Connection -> ASSheetId -> IO ()
deleteBarsInSheet conn sid = do
  colKeys <- fromRight <$> runRedis conn (keys $ BC.pack $ barPropsKeyPattern sid ColumnType)
  rowKeys <- fromRight <$> runRedis conn (keys $ BC.pack $ barPropsKeyPattern sid RowType)
  runRedis conn $ del (colKeys ++ rowKeys)
  return ()
