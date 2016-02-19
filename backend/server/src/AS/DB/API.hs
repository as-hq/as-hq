{-# LANGUAGE OverloadedStrings, DataKinds, GADTs #-}

module AS.DB.API where

import Prelude()
import AS.Prelude
import AS.Types.Cell
import AS.Types.Bar
import AS.Types.BarProps (BarProp, ASBarProps) 
import AS.Types.Messages
import AS.Types.DB
import AS.Types.EvalHeader
import AS.Types.CellProps
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.CondFormat
import AS.Types.Updates
import AS.Types.User
import AS.Types.Window

import qualified AS.Config.Settings as Settings
import qualified AS.DB.Internal as DI
import qualified AS.Serialize as S
import AS.Util as U
import AS.Parsing.Substitutions (getDependencies)
import AS.Logging

import Control.Arrow((&&&))

import Data.List (zip4,intercalate,find)
import Data.List.Split
import qualified Data.Map as M

import Data.Maybe hiding (fromJust)

import Data.SafeCopy (SafeCopy)

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Lens hiding (set)
import Control.Monad.Trans
import Data.Time
import Database.Redis hiding (decode)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.Aeson hiding (Success)
import Data.ByteString.Unsafe as BU

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

runRedis_ :: Connection -> Redis a -> IO ()
runRedis_ conn = void . runRedis conn

-- Generic getter method for RedisKey
lookUpRedisKeys :: (SafeCopy b) => Connection -> [RedisKey a] -> IO [Maybe b]
lookUpRedisKeys conn = lookUpKeys conn . map toRedisFormat

lookUpKeys :: (SafeCopy b) => Connection -> [B.ByteString] -> IO [Maybe b]
lookUpKeys conn byteKeys = runRedis conn $ do
  bs <- mget' byteKeys
  return $ map (S.maybeDecode =<<) bs

-- #NeedsRefactor: read is not great.
-- If a key can't be found, there will not be an error thrown. Timchu, 2/15/16.
mget' :: [B.ByteString] -> Redis [Maybe B.ByteString]
mget' [] = return []
mget' a = $fromRight <$> mget a

lookUpReadableKeys :: (Read a, RedisCtx Redis (Either Reply)) => [B.ByteString] -> Redis [a]
lookUpReadableKeys keys = do
  x <- mget' keys
  return $ mapMaybe (fmap $ $read . BC.unpack) x

----------------------------------------------------------------------------------------------------------------------
-- Raw cells API
-- all of these functions are order-preserving unless noted otherwise.

getCells :: Connection -> [ASIndex] -> IO [Maybe ASCell]
getCells _ [] = return []
getCells conn locs = (lookUpKeys conn . map S.encode) locs

setCells :: Connection -> [ASCell] -> IO ()
setCells _ [] = return ()
setCells conn cs = 
  runRedis_ conn $ mset $ map (S.encode . view cellLocation &&& S.encode) cs

deleteLocs :: Connection -> [ASIndex] -> IO ()
deleteLocs _ [] = return ()
deleteLocs conn locs = runRedis_ conn $ del $ map S.encode locs

----------------------------------------------------------------------------------------------------------------------
-- Additional cell API methods

getCell :: Connection -> ASIndex -> IO (Maybe ASCell)
getCell conn loc = $head <$> getCells conn [loc]

setCell :: Connection -> ASCell -> IO ()
setCell conn c = setCells conn [c]

getPossiblyBlankCell :: Connection -> ASIndex -> IO ASCell
getPossiblyBlankCell conn loc = $head <$> getPossiblyBlankCells conn [loc]

getCellsInSheet :: Connection -> ASSheetId -> IO [ASCell]
getCellsInSheet conn sid = getCellsByKeyPattern conn $ "*" ++ T.unpack sid ++ "*"

getAllCells :: Connection -> IO [ASCell]
getAllCells conn = getCellsByKeyPattern conn "*"

deleteLocsInSheet :: Connection -> ASSheetId -> IO ()
deleteLocsInSheet conn sid = runRedis_ conn $ do
  Right ks <- keys $ BC.pack $ "*" ++ T.unpack sid ++ "*"
  let locKeys = mapMaybe readLocKey ks
      readLocKey k = case S.maybeDecode k of 
        Just (Index _ _) -> Just k
        _ -> Nothing
  del locKeys

getCellsByKeyPattern :: Connection -> String -> IO [ASCell]
getCellsByKeyPattern conn pattern = do
  ks <- DI.getKeysByPattern conn pattern
  let locs = mapMaybe readLoc ks
      readLoc k = case S.maybeDecode k of 
        Just l@(Index _ _) -> Just l
        _ -> Nothing
  return locs
  map $fromJust <$> getCells conn locs

-- Gets the cells at the locations with expressions and values removed, but tags intact. 
-- this function is order-preserving
getBlankedCellsAt :: Connection -> [ASIndex] -> IO [ASCell]
getBlankedCellsAt conn locs = 
  let blankExpr = expression .~ "" -- replaces the expression in ASExpression with an empty string
      blankCell (Cell l xp _ ts _ _) = Cell l (blankExpr xp) NoValue ts Nothing Nothing
  in do 
    cells <- getPossiblyBlankCells conn locs
    return $ map blankCell cells -- #lens

-- this function is order-preserving
getPossiblyBlankCells :: Connection -> [ASIndex] -> IO [ASCell]
getPossiblyBlankCells conn locs = do 
  cells <- getCells conn locs
  return $ map (\(l,c) -> fromMaybe (blankCellAt l) c) (zip locs cells)

getPropsAt :: Connection -> ASIndex -> IO ASCellProps
getPropsAt conn ind = view cellProps <$> getPossiblyBlankCell conn ind

----------------------------------------------------------------------------------------------------------------------
-- Locations

locationsExist :: Connection -> [ASIndex] -> IO [Bool]
locationsExist conn locs = runRedis conn $ $fromRight <$> scriptExists (map S.encode locs)

locationExists :: Connection -> ASIndex -> IO Bool
locationExists conn loc = $head <$> locationsExist conn [loc] 

----------------------------------------------------------------------------------------------------------------------
-- Fat cells

-- | Returns the listkeys of all the lists that are entirely contained in the range.  
fatCellsInRange :: Connection -> ASRange -> IO [RangeKey]
fatCellsInRange conn rng = do
  let sid = rangeSheetId rng
  rangeKeys <- getRangeKeysInSheet conn sid
  let rects = map rangeRect rangeKeys
      zipRects = zip rangeKeys rects
      zipRectsContained = filter (\(_,rect) -> rangeContainsRect rng rect) zipRects
  return $ map fst zipRectsContained

getRangeKeysInSheet :: Connection -> ASSheetId -> IO [RangeKey]
getRangeKeysInSheet conn sid = runRedis conn $ do
  Right ks <- smembers . toRedisFormat $ SheetRangesKey sid
  liftIO $ printObj "GOT RANGEKEYS IN SHEET: " ks
  return $ map (unpackKey . fromRedis) ks
    where
      fromRedis k = read2 (BC.unpack k) :: RedisKey RangeType
      unpackKey :: RedisKey RangeType -> RangeKey
      unpackKey (RedisRangeKey k) = k

getRangeDescriptor :: Connection -> RangeKey -> IO (Maybe RangeDescriptor)
getRangeDescriptor conn key = runRedis conn $ do 
  Right desc <- get . toRedisFormat $ RedisRangeKey key
  return $ S.maybeDecode =<< desc

getRangeDescriptors :: Connection -> [RangeKey] -> IO [Maybe RangeDescriptor]
getRangeDescriptors conn = lookUpRedisKeys conn . map RedisRangeKey

getRangeDescriptorsInSheet :: Connection -> ASSheetId -> IO [RangeDescriptor]
getRangeDescriptorsInSheet conn sid = do
  keys <- getRangeKeysInSheet conn sid
  descs <- getRangeDescriptors conn keys
  return $ map $fromJust descs

----------------------------------------------------------------------------------------------------------------------
-- Volatile cell methods

getVolatileLocs :: Connection -> IO [ASIndex]
getVolatileLocs conn = runRedis conn $ do
  vl <- map S.maybeDecode . $fromRight <$> smembers (toRedisFormat VolatileLocsKey)
  if any isNothing vl 
    then $error "Error decoding volatile locs!!!"
    else return $ map $fromJust vl

-- TODO: some of the cells may change from volatile -> not volatile, but they're still in volLocs
setChunkVolatileCells :: [ASCell] -> Redis ()
setChunkVolatileCells cells = do
  let vLocs = mapCellLocation $ filter (hasPropType VolatileProp . view cellProps) cells
  sadd (toRedisFormat VolatileLocsKey) (map S.encode vLocs)
  return ()

-- #lens
deleteChunkVolatileCells :: [ASCell] -> Redis ()
deleteChunkVolatileCells cells = do
  let vLocs = mapCellLocation $ filter (hasPropType VolatileProp . view cellProps) cells 
  srem (toRedisFormat VolatileLocsKey) (map S.encode vLocs)
  return ()

----------------------------------------------------------------------------------------------------------------------
-- Permissions

--canAccessSheet :: Connection -> ASUserId -> ASSheetId -> IO Bool
--canAccessSheet conn uid sheetId = do
--  mSheet <- getSheet conn sheetId
--  maybe (return False) (return . hasPermissions uid . sheetPermissions) mSheet

--canAccess :: Connection -> ASUserId -> ASIndex -> IO Bool
--canAccess conn uid loc = canAccessSheet conn uid (loc^.locSheetId)

--canAccessAll :: Connection -> ASUserId -> [ASIndex] -> IO Bool
--canAccessAll conn uid locs = all id <$> mapM (canAccess conn uid) locs

isPermissibleMessage :: ASUserId -> Connection -> ServerMessage -> IO Bool
isPermissibleMessage uid conn _ = return True
-- (ServerMessage _ payload) = case payload of 
--   PayloadCL cells -> canAccessAll conn uid (mapCellLocation cells)
--   PayloadLL locs -> canAccessAll conn uid locs
--   PayloadS sheet -> canAccessSheet conn uid (sheetId sheet)
--   PayloadW window -> canAccessSheet conn uid (windowSheetId window)
--   PayloadProp _ rng -> canAccessAll conn uid (rangeToIndices rng)
--   _ -> return True
-- commenting out 12/28 -- Alex


----------------------------------------------------------------------------------------------------------------------------------------------
-- Repeat handlers

storeLastMessage :: Connection -> ServerMessage -> CommitSource -> IO () 
storeLastMessage conn msg src = case serverAction msg of 
  Repeat _ -> return ()
  _ -> runRedis_ conn (set (toRedisFormat $ LastMessageKey src) (S.encode msg))

getLastMessage :: Connection -> CommitSource -> IO ServerMessage
getLastMessage conn src = $error "Currently not implemented"
 -- runRedis conn $ do 
 --  msg <- $fromRight <$> get (toRedisFormat $ LastMessageKey src)
 --  return $ case (maybeDecode =<< msg) of 
 --    Just m -> m
 --    -- _ -> ServerMessage NoAction (PayloadN ())

----------------------------------------------------------------------------------------------------------------------------------------------
-- Conditional formatting handlers

getCondFormattingRules :: Connection -> ASSheetId -> [CondFormatRuleId] -> IO [CondFormatRule] 
getCondFormattingRules conn sid cfids = do
  let keys = map (CFRuleKey sid) cfids
  catMaybes <$> lookUpRedisKeys conn keys

-- some replication with the above...
getCondFormattingRulesInSheet :: Connection -> ASSheetId -> IO [CondFormatRule]
getCondFormattingRulesInSheet conn sid = do 
  keys <- DI.getKeysInSheetByType conn sid CFRuleType
  catMaybes <$> lookUpKeys conn keys

deleteCondFormattingRules :: Connection -> ASSheetId -> [CondFormatRuleId] -> IO ()
deleteCondFormattingRules conn sid cfids = runRedis_ conn (del $ map (toRedisFormat . CFRuleKey sid) cfids)

setCondFormattingRules :: Connection -> ASSheetId -> [CondFormatRule] -> IO ()
setCondFormattingRules conn sid rules = runRedis_ conn $ do
  let cfids        = map condFormatRuleId rules 
      makeKey      = toRedisFormat . CFRuleKey sid 
      keys         = map makeKey cfids
      encodedRules = map S.encode rules 
  mset $ zip keys encodedRules

----------------------------------------------------------------------------------------------------------------------------------------------
-- Row/col getters/setters

-- #needsrefactor should be consistent about whether our getters and setters take in a single key or lists of keys.
getBar :: Connection -> BarIndex -> IO (Maybe Bar)
getBar conn bInd  = runRedis conn $ do 
  Right msg <- get . toRedisFormat $ BarKey bInd
  return $ S.maybeDecode =<< msg

getBars :: Connection -> [BarIndex] -> IO [Maybe Bar] 
getBars conn = lookUpRedisKeys conn . map BarKey

setBar :: Connection -> Bar -> IO ()
setBar conn bar = runRedis_ conn $ set (toRedisFormat $ BarKey (barIndex bar)) (S.encode bar)

setBars :: Connection -> [Bar] -> IO ()
setBars conn bars = do
  let barKVPairs = map (toRedisFormat . BarKey . barIndex &&& S.encode) bars
  runRedis_ conn $ mset barKVPairs

deleteBarAt :: Connection -> BarIndex -> IO ()
deleteBarAt conn bInd = runRedis_ conn $ del [toRedisFormat $ BarKey bInd]

deleteBarsAt  :: Connection -> [BarIndex] -> IO()
deleteBarsAt conn = runRedis_ conn . del . map (toRedisFormat . BarKey)

replaceBars :: Connection -> [Bar] -> [Bar] -> IO ()
replaceBars conn fromBars toBars = do
  deleteBarsAt conn $ map barIndex fromBars
  setBars conn toBars

getBarsInSheet :: Connection -> ASSheetId -> IO [Bar]
getBarsInSheet conn sid = do 
  let readKey k = read2 (BC.unpack k) :: RedisKey BarType2
      extractInd :: RedisKey BarType2 -> BarIndex
      extractInd (BarKey ind) = ind
  inds <- map (extractInd . readKey) <$> DI.getKeysInSheetByType conn sid BarType2
  catMaybes <$> getBars conn inds

deleteBarsInSheet :: Connection -> ASSheetId -> IO ()
deleteBarsInSheet conn sid = do
  colKeys <- $fromRight <$> runRedis conn (keys $ BC.pack $ barPropsKeyPattern sid ColumnType)
  rowKeys <- $fromRight <$> runRedis conn (keys $ BC.pack $ barPropsKeyPattern sid RowType)
  runRedis_ conn $ del (colKeys ++ rowKeys)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Header expressions handlers

getEvalHeader :: Connection -> ASSheetId -> ASLanguage -> IO EvalHeader
getEvalHeader conn sid lang = runRedis conn $ do 
  msg <- get . toRedisFormat $ EvalHeaderKey sid lang
  return $ EvalHeader sid lang $ case msg of 
    Right (Just msg') -> BC.unpack msg'
    Right Nothing -> ""
    Left _            -> $error "Failed to retrieve eval header"

setEvalHeader :: Connection -> EvalHeader -> IO ()
setEvalHeader conn evalHeader = runRedis_ conn $ do
  let sid  = evalHeader^.evalHeaderSheetId
      lang = evalHeader^.evalHeaderLang
      xp   = evalHeader^.evalHeaderExpr
  set (toRedisFormat $ EvalHeaderKey sid lang) (BC.pack xp)
