{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString        as B
import qualified Data.ByteString.Search as BS
import qualified Data.ByteString.Lazy   as BL
import Data.ByteString (ByteString)
import Control.Exception
import qualified Data.Set as Set

import Prelude()
import AS.Prelude
import AS.Mock.DB

import Data.List.Split

import qualified AS.DB.API as DB
import qualified AS.DB.Export as DB
import qualified AS.DB.Clear as DB
import qualified AS.DB.Transaction as DB
import qualified AS.LanguageDefs as LD

import AS.Config.Settings

import AS.Types.Locations
import AS.Types.RangeDescriptor
import AS.Types.User
import AS.Types.Eval
import AS.Types.EvalHeader

import Control.Concurrent.Async

import qualified AS.Serialize as Serial

import Data.SafeCopy

import System.Directory

import Conv
import Types
import DB

main :: IO ()
main = alphaMain $ do
  conn <- connectRedis

  -- perform workbook migration
  putStrLn "\n\nperforming workbook migration..."
  migrateWorkbooks conn
  putStrLn "done."

  -- perform strict migration of all other types
  putStrLn "\n\nperforming strict migration..."
  migrateDB conn
  putStrLn "done."

  -- add onboarding sheets
  putStrLn "\n\nadding onboarding sheets..."
  onboard conn
  putStrLn "done"

migrateWorkbooks :: Connection -> IO ()
migrateWorkbooks conn = do
  (Right bs) <- runRedis conn $ keys "*"
  let ks = map (fromJust . Serial.maybeDecode) bs :: [DBKey]

  -- users
  let f (UserKey _) = True
      f _ = False
  let userKeyPairs = filter (\(b, k) -> f k) $ zip bs ks
  users <- forM userKeyPairs $ \(ub, uk) -> do
    (UserValue user) <- runRedis conn $ do
      Right (Just val) <- get ub
      return . fromJust $ toDBValue val :: Redis DBValue
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
        return . fromJust $ maybeDecode sval :: Redis DBValue
      putStrLn $ "[SUCCESS] user " ++ (T.unpack $ user^.userId) ++ " got sheet: " ++ (sheet^.sheetName)
      return $ Just sheet
    let sheets = catMaybes msheets

    -- first workbook
    wid <- T.pack <$> getUUID
    let wb = Workbook wid "Workbook1" (Set.fromList sids) (user^.userId) openedSheet

      -- headers
    oldSheetHeaders <- forM sheets $ \sheet -> do
      let sid = sheet^.sheetId
      runRedis conn $ do
        forM headerLangs $ \lang -> do
          let hkey = encode $ EvalHeaderKey0 sid lang
          Right mhval <- get hkey
          let hval = maybeDecode =<< mhval :: Maybe DBValue
          return $ EvalHeader0 "some_sheet_id" lang $ --doesn't matter what the sheetId is
            maybe "" (\(HeaderValue h) -> BC.unpack h) hval
                                    
    let oldHeaderSets = zip oldSheetHeaders $ map (view sheetName) sheets
    let newWorkbookHeaders = collateOldHeaders wid oldHeaderSets 

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
      mapM_ (DB.setEvalHeader conn) newWorkbookHeaders
      DB.setWorkbook conn wb
      DB.setUser conn newUser

  -- perform DB actions
  mapM_ ($ ()) dbActions

collateOldHeaders :: WorkbookID -> [([EvalHeader0], SheetName)] -> [EvalHeader]
collateOldHeaders wid oldhs = processHeaders oldhs defaultHeaderSet
  where
    defaultHeaderSet = map (\lang -> EvalHeader wid lang "") headerLangs
    processHeaders [] hs = for hs $ \h -> 
      h & evalHeaderExpr %~ ((defaultHeaderText (h^.evalHeaderLang) ++ "\n") ++)
    processHeaders ((headers, sname):ss) hs = processHeaders ss hs'
      where
        hs' = for (zip hs headers) $ \(h, h') -> 
                LD.mergeHeaders ("SHEET: " ++ sname) h (migrate h')

onboard :: Connection -> IO ()
onboard conn = do
  -- load onboarding data into new workbook from disk
  let origUid = "alphasheetsdemo@gmail.com"
  let importOldSheets :: [(SheetID, SheetName)] -> SheetID -> WorkbookID -> WorkbookName -> IO ()
      importOldSheets sheets openedSheet wid wname = do
        let wb = Workbook wid wname (Set.fromList $ map fst sheets) origUid openedSheet
        DB.setWorkbook conn wb
        oldHeaders <- forM sheets $ \(sid, sname) -> do
          ex <- fromJust . Serial.maybeDecode <$> B.readFile ("sheets/" ++ sname ++ ".as") :: IO ExportData0
          putStrLn $ "[SUCCESS] decoded sheet: " ++ sname
          DB.setSheet conn $ Sheet sid sname origUid False
          DB.clearSheet conn sid 
          DB.setCellsPropagated conn origUid $ ex^.exportCells0
          mapM_ (DB.setBar conn) $ ex^.exportBars0
          mapM_ (DB.setDescriptor conn) $ ex^.exportDescriptors0
          DB.setCondFormattingRules conn sid $ ex^.exportCondFormatRules0
          return (ex^.exportHeaders0, sname)
        let newHeaders = collateOldHeaders wid oldHeaders 
        mapM_ (DB.setEvalHeader conn) newHeaders

  importOldSheets tutorialSheets tutorialOpen tutorialWorkbookId tutorialWorkbookName
  importOldSheets templateSheets templateOpen templateWorkbookId templateWorkbookName
  putStrLn "finished importing onboarding sheets."
      
tutorialSheets :: [(SheetID, SheetName)]
tutorialSheets = [
    ("d814c688-2115-11e6-8001-0401dbcded01", "python_tutorial")
  , ("d847bbea-2117-11e6-8001-0401dbcded01", "python_tutorial_advanced")
  , ("ddca8692-2117-11e6-8001-0401dbcded01", "r_tutorial")
  , ("e355f92a-2117-11e6-8001-0401dbcded01", "sql_tutorial")
  ]

tutorialOpen :: SheetID
tutorialOpen = "d814c688-2115-11e6-8001-0401dbcded01"

templateSheets :: [(SheetID, SheetName)]
templateSheets = [
    ("eae1cf2a-2117-11e6-8001-0401dbcded01", "moving_averages")
  , ("ff8f4f4c-2117-11e6-8001-0401dbcded01", "task_times")
  ]

templateOpen :: SheetID
templateOpen = "eae1cf2a-2117-11e6-8001-0401dbcded01"


main' :: IO ()
main' = alphaMain $ do  
  -- perform migration
  conn <- connectRedis 
  migrateDB conn 

-- This function does the migration by 
-- (1) Getting all keys from the DB
-- (2) Trying to parse all of them into RedisQuery types, and exiting if any errors here
-- (3) Getting the values for each key, and converting them to the new serialization, while
-- exiting if there's any errors here
-- (4) Do the DB transformation (delete old keys, set new keys)
-- streams.
migrateDB :: Connection -> IO ()
migrateDB conn = do 
  Right bs <- runRedis conn $ keys "*"
  _ <- flip mapConcurrently (zip [1..] $ chunksOf 20000 $ zip [1..] bs) $ \(chunkI, chunk) -> do
    putStrLn $ "forked chunk: " ++ show chunkI
    forM_ chunk $ \(i, b) -> do
      when (i `mod` 1000 == 0) $ putStrLn $ "chunk " ++ show chunkI ++ " processed: " ++ show i
      let k = fromJust $ toDBKey b :: DBKey
      getDBSetterObject k b conn >>= applyDBSetterObject conn
  return ()
  --handleIgnored $ removeFile "keys.dump"
  --keysH <- openFile "keys.dump" WriteMode
  --let rep = "\\n" :: B.ByteString
  --forM_ ks $ B.hPutStrLn keysH . BL.toStrict. BS.replace "\n" rep
  --hClose keysH
  --putStrLn "...dumped the keys"

  --keysH <- openFile "keys.dump" ReadMode
  --let go !i = do
  --              putStrLn $ show i
  --              --handleAny (const $ putStrLn "finished strict migration") $ do
  --              b <- B.hGetLine keysH
  --              putStrLn $ show b
  --              let k = fromJust $ toDBKey b :: DBKey
  --              getDBSetterObject k b conn >>= applyDBSetterObject conn
  --              go (i+1)
  --go 1

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
getDBSetterObject :: DBKey -> B.ByteString -> Connection -> IO DBSetterObject
getDBSetterObject key bkey conn = do 
  let (getter, _) = keyToDBFuncs key
  bsVals <- getter conn bkey 
  let maybeNewValsBS = map (newValueBS key) bsVals
  if any isNothing maybeNewValsBS
    then do 
      let badVals = map fst $ filter (isNothing . snd) $ zip bsVals maybeNewValsBS
      error "could not decode value"
    else do 
      let newValsBS = catMaybes maybeNewValsBS
      return $ DBSetterObject bkey key newValsBS

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



