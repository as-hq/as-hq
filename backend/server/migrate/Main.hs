{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import Control.Exception
import qualified Data.Set as Set

import Prelude()
import AS.Prelude
import AS.Mock.DB

import qualified AS.DB.API as DB
import AS.Config.Settings
import qualified AS.LanguageDefs as LD
import AS.Config.Settings as CS
import AS.Types.Locations
import AS.Types.RangeDescriptor
import AS.Types.User
import AS.Types.EvalHeader

import Data.SafeCopy

import Conv
import Types
import DB

main :: IO ()
main = alphaMain $ do
  conn <- connectRedis
  (Right bs) <- runRedis conn $ keys "*"
  let ks = map ($fromJust . toDBKey) bs :: [DBKey]

  -- users
  let f (UserKey _) = True
      f _ = False
  let userKeyPairs = filter (\(b, k) -> f k) $ zip bs ks
  users <- forM userKeyPairs $ \(ub, uk) -> do
    (UserValue user) <- runRedis conn $ do
      Right (Just val) <- get ub
      return . $fromJust $ toDBValue val :: Redis DBValue
    putStrLn $ "found user: " ++ show (user^.userId)
    return user

  dbActions <- forM users $ \user -> do
    -- the bytestring migration of the `user` type simply shoves all of the 
    -- sheetIds into the `workbookIds` record. 
    let sids = Set.toList $ user^.workbookIds
    -- lastOpenSheet has been shoved into lastOpenWorkbook by the bytestring migration
    let openedSheet = user^.lastOpenWorkbook

    -- sheets
    let onSheetExc _ = putStrLn "[FAIL]" >> return Nothing
    msheets <- forM sids $ \sid -> handleAny onSheetExc $ do
      putStrLn $ "[GETTING...] user " ++ (T.unpack $ user^.userId) ++ " getting sheet: " ++ show sid
      let skey = encode $ SheetKey0 sid
      (SheetValue sheet) <- runRedis conn $ do
        Right (Just sval) <- get skey
        return . $fromJust $ maybeDecode sval :: Redis DBValue
      putStrLn $ "[SUCCESS] user " ++ (T.unpack $ user^.userId) ++ " got sheet: " ++ (sheet^.sheetName)
      return $ Just sheet
    let sheets = catMaybes msheets

    -- first workbook
    wid <- T.pack <$> getUUID
    let wb = Workbook wid "Workbook1" (Set.fromList sids) (user^.userId) openedSheet

      -- headers
    let defaultHeaderSet = map (\lang -> EvalHeader wid lang "") headerLangs
    let processHeaders [] hs = return hs
        processHeaders (s:ss) hs = do
          let sid = s^.sheetId
          headers <- runRedis conn $ do
            forM headerLangs $ \lang -> do
              let hkey = encode $ EvalHeaderKey0 sid lang
              Right mhval <- get hkey
              return $ EvalHeader wid lang $ 
                maybe "" BC.unpack mhval
          let headers' = map (& evalHeaderWorkbookId .~ wid) headers
          let hs' = flip map (zip hs headers') $ \(h, h') -> 
                      let lang = h^.evalHeaderLang
                          body = unlines  [ h^.evalHeaderExpr
                                          , LD.commented lang "-----------------------------------"
                                          , LD.commented lang $ "SHEET: " ++ (s^.sheetName)
                                          , LD.commented lang "-----------------------------------"
                                          -- just don't even ask
                                          , drop 17 $ h'^.evalHeaderExpr
                                          ]
                      in EvalHeader wid lang body
          processHeaders ss hs'
    newHeaders <- processHeaders sheets defaultHeaderSet

    -- user
    let newUser = User  { _userId = user^.userId
                        , _workbookIds = Set.singleton wid
                        , _lastOpenWorkbook = wid
                        }

    return $ \_ -> do
      -- delete old data
      runRedis conn $ do
        del $ map (encode . SheetKey0) sids
        del $ map fst userKeyPairs
        del $ concat $ map (\sid -> map (encode . EvalHeaderKey0 sid) headerLangs) sids
      -- set all data
      mapM_ (DB.setSheet conn) sheets
      mapM_ (DB.setEvalHeader conn) newHeaders 
      DB.setWorkbook conn wb
      DB.setUser conn newUser

  -- perform DB actions
  mapM_ ($ ()) dbActions

main' :: IO ()
main' = alphaMain $ do  
  -- perform migration
  conn <- connectRedis 
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
        -- we don't do first migrations anymore. (anand 5/9)
        $undefined
        --let uid = T.pack "alphasheetsdemo@gmail.com"
        --let sids = map sheetId sheets 
        --let user = User (Set.fromList sids) Set.empty uid ($head sids)
        --print user
        --setV conn (UserKey uid) (UserValue user)

    else throwIO $ CouldNotDecodeKeys $ take 5 badRawKeys

-- Given a DBKey, obtain a Getter and Setter DB function (list, set, value)
keyToDBFuncs :: DBKey -> (Getter, Setter)
keyToDBFuncs NullKey = sPair
keyToDBFuncs (SheetKey _) = vPair
keyToDBFuncs (WorkbookKey _) = vPair
keyToDBFuncs (EvalHeaderKey _ _) = vPair 
keyToDBFuncs (TempCommitKey _) = vPair
keyToDBFuncs (PushCommitKey _) = lPair
keyToDBFuncs (PopCommitKey _) = lPair
keyToDBFuncs (LastMessageKey _) = vPair
keyToDBFuncs (CFRuleKey _ _) = vPair
keyToDBFuncs (BarKey _) = vPair
keyToDBFuncs (UserKey _) = vPair
keyToDBFuncs (IndexKey _) = vPair
keyToDBFuncs (RedisRangeKey _) = vPair
keyToDBFuncs (LogKey _) = lPair
keyToDBFuncs (AllSheetsKey) = sPair
keyToDBFuncs (AllWorkbooksKey) = sPair
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
  setter conn (encode key) newValsBS

  case key of 
    IndexKey i -> when isFirstMigration $ do
      -- on the first migration, add index keys to their sheet-sets
      let sid = view locSheetId i
      addS conn (SheetGroupKey $ SheetLocsKey sid) (KeyValue key)
    RedisRangeKey rk -> when isFirstMigration $ do
      -- on the first migration, add range keys to their sheet sets
      let sid =  view locSheetId (keyIndex rk)
      let vals = (map maybeDecode newValsBS) :: [Maybe DBValue]
      addS conn (SheetGroupKey $ SheetRangesKey sid) (KeyValue key)
    _ -> return ()

-- Get the bytestring values which represent sheets
extractSheets :: [ByteString] -> [Sheet]
extractSheets = mapMaybe convSheet . mapMaybe maybeDecode
  where
    convSheet (SheetValue s) = Just s
    convSheet _ = Nothing



