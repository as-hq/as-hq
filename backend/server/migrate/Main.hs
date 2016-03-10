{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Data.List (stripPrefix)
import qualified Data.Text as T
import Database.Redis
import Data.Maybe
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Control.Exception
import Control.Monad
import Control.Lens (view)
import qualified Data.Set as Set

import Prelude()
import AS.Prelude
import qualified AS.Serialize as S
import qualified AS.DB.Internal as DI
import AS.Config.Settings as CS
import AS.Types.DB 
import AS.Types.Locations
import AS.Types.RangeDescriptor
import AS.Types.User

import Conv
import Types
import DB

main :: IO ()
main = alphaMain $ do  
  -- perform migration
  conn <- DI.connectRedis 
  -- Sheets weren't stored correctly in AllSheetsType or as key-value pairs before. 
  -- We remedy that for the first migration (this is preparation work for the migration)
  when isFirstMigration $ 
    runRedis conn $ do 
      --  On the first migration, delete some keys that don't parse that we don't care about (bad keys)
      del ["foo", 
           "AllWorkbooksType~", 
           "WorkbookType~Workbook1", 
           "CFRuleType~ericmillerNk8B5xD2l",
           "TempCommitType~salvatier?TEST_USER_ID",
           "PushCommitType~salvatier?TEST_USER_ID"]
      Right bs <- keys "SheetRangesType~*"
      let sheetNames = mapMaybe (stripPrefix "SheetRangesType~" . BC.unpack) bs
      let sheetKeys = map (\name -> "SheetType~" ++ name)  sheetNames
      let sheets = map (\i -> Sheet (T.pack i) i) sheetNames
      let sheetVals = map (\s -> init (show s) ++ ", sheetPermissions = Blacklist []}") sheets
      sadd (BC.pack "AllSheetsType~") $ map BC.pack sheetKeys
      mset $ zip (map BC.pack sheetKeys) (map BC.pack sheetVals)
      return ()
  migrateDBE conn 

migrateDBE :: Connection -> IO ()
migrateDBE conn = catch (migrateDB conn) (\e -> print (e :: MigrateError)) 

-- This function does the migration by 
-- (1) Getting all keys from the DB
-- (2) Trying to parse all of them into RedisQuery types, and exiting if any errors here
-- (3) Getting the values for each key, and converting them to the new serialization, while
-- exiting if there's any errors here
-- (4) Do the DB transformation (delete old keys, set new keys)
-- Some sort of streaming would be more performant, but this is fine for now. 
-- We're building up DBSetterObjects in memory so that we don't modify the DB if any errors.
migrateDB :: Connection -> IO ()
migrateDB conn = do 
  bStrKeys <- runRedis conn $ handleErr $ keys "*"
  let maybeKeys = (map toDBKey bStrKeys) :: [Maybe DBKey]
  let rawKeys = zip [1..] bStrKeys
  -- These are the (Int, ByteString) undecodeable key pairs
  let badRawKeys = map fst $ filter (isNothing . snd) $ zip rawKeys maybeKeys
  if null badRawKeys
    then do 
      let keys = catMaybes maybeKeys
      let rawKeys = zip [1..] bStrKeys
      let pairs = zip keys rawKeys
      dbObjects <- mapM (\(key, rawKey) -> getDBSetterObject key rawKey conn) pairs
      mapM_ (applyDBSetterObject conn) dbObjects

      -- On the first migration, add all sheets to a default user
      let sheets = extractSheets $ concatMap setterVals dbObjects
      when (isFirstMigration && not (null sheets)) $ do
        let uid = T.pack "alphasheetsdemo@gmail.com"
        let sids = map sheetId sheets 
        let user = User (Set.fromList sids) uid ($head sids)
        print user
        setV conn (UserKey uid) (UserValue user)

    else throwIO $ CouldNotDecodeKeys $ take 5 badRawKeys

-- Given a DBKey, obtain a Getter and Setter DB function (list, set, value)
keyToDBFuncs :: DBKey -> (Getter, Setter)
keyToDBFuncs (SheetKey _) = vPair
keyToDBFuncs (EvalHeaderKey _ _) = vPair 
keyToDBFuncs (TempCommitKey _) = vPair
keyToDBFuncs (PushCommitKey _) = lPair
keyToDBFuncs (PopCommitKey _) = lPair
keyToDBFuncs (LastMessageKey _) = vPair
keyToDBFuncs (AllSheetsKey) = sPair
keyToDBFuncs (RedisRangeKey _) = vPair
keyToDBFuncs (IndexKey _) = vPair
keyToDBFuncs (BarKey _) = vPair
keyToDBFuncs (UserKey _) = vPair
keyToDBFuncs (SheetGroupKey (SheetRangesKey _)) = sPair
keyToDBFuncs (SheetGroupKey (SheetTempCommitsKey _)) = sPair
keyToDBFuncs (SheetGroupKey (SheetLastMessagesKey _)) = sPair
keyToDBFuncs (SheetGroupKey (SheetCFRulesKey _)) = sPair
keyToDBFuncs (SheetGroupKey (SheetBarsKey _)) = sPair
keyToDBFuncs (SheetGroupKey (SheetLocsKey _)) = sPair

-- Given a DBKey, (Int, ByteString) rawKey, and connection, get the values associated 
-- with the key, and try to decode them. If there's any decoding errors, exit. Otherwise, 
-- return a DBSetterObject with the old bytestring, the DBKey, and the list of values
-- associated with this key. We return a dbSetterObject and build them up in memory so that
-- migrations are atomic, although this sacrifices some memory/performance.
getDBSetterObject :: DBKey -> RawKey -> Connection -> IO DBSetterObject
getDBSetterObject key rawKey@(num, bs) conn = do 
  let (getter, _) = keyToDBFuncs key
  bsVals <- getter conn bs 
  let maybeNewValsBS = map (newValueBS key) bsVals
  if any isNothing maybeNewValsBS
    then do 
      let badVals = map fst $ filter (isNothing . snd) $ zip bsVals maybeNewValsBS
      throwIO $ CouldNotDecodeValues rawKey key badVals
    else do 
      let newValsBS = catMaybes maybeNewValsBS
      return $ DBSetterObject bs key newValsBS

-- Do the actual setting/modification of the DB given a DBSetterObject
applyDBSetterObject :: Connection -> DBSetterObject -> IO ()
applyDBSetterObject conn (DBSetterObject bs key newValsBS) = do 
  let (_, setter) = keyToDBFuncs key
  runRedis conn $ handleErr $ del [bs]
  setter conn (S.encode key) newValsBS

  case key of 
    IndexKey i -> when isFirstMigration $ do
      -- on the first migration, add index keys to their sheet-sets
      let sid = view locSheetId i
      addS conn (SheetGroupKey $ SheetLocsKey sid) (KeyValue key)
    RedisRangeKey rk -> when isFirstMigration $ do
      -- on the first migration, add range keys to their sheet sets
      let sid =  view locSheetId (keyIndex rk)
      let vals = (map S.maybeDecode newValsBS) :: [Maybe DBValue]
      addS conn (SheetGroupKey $ SheetRangesKey sid) (KeyValue key)
    _ -> return ()

-- Get the bytestring values which represent sheets
extractSheets :: [ByteString] -> [ASSheet]
extractSheets = mapMaybe convSheet . mapMaybe S.maybeDecode
  where
    convSheet (SheetValue s) = Just s
    convSheet _ = Nothing

