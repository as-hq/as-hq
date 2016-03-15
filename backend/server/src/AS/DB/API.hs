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
import AS.Types.Commits

import qualified AS.Config.Settings
import qualified AS.DB.Internal as DI
import qualified AS.Serialize as S
import AS.DB.Internal
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
import Data.Either as DE

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

------------------------------------------------------------------------------------------------------------------------
-- Raw cells API
-- all of these functions are order-preserving unless noted otherwise.

getCell :: Connection -> ASIndex -> IO (Maybe ASCell)
getCell conn loc = $head <$> getCells conn [loc]

-- ::ALEX:: these are kind of unsafe
setCell :: Connection -> ASCell -> IO ()
setCell conn c = setCells conn [c]

getCells :: Connection -> [ASIndex] -> IO [Maybe ASCell]
getCells = multiGet IndexKey dbValToCell

-- ::ALEX:: these are kind of unsafe
setCells :: Connection -> [ASCell] -> IO ()
setCells = setWithSheetFunc (SheetLocsKey . view locSheetId) IndexKey CellDBValue (view cellLocation)

-- ::ALEX:: these are kind of unsafe
deleteLocs :: Connection -> [ASIndex] -> IO ()
deleteLocs = delWithSheetFunc (SheetLocsKey . view locSheetId) IndexKey

------------------------------------------------------------------------------------------------------------------------
-- Additional cell API methods

getPossiblyBlankCell :: Connection -> ASIndex -> IO ASCell
getPossiblyBlankCell conn loc = $head <$> getPossiblyBlankCells conn [loc]

getCellsInSheet :: Connection -> ASSheetId -> IO [ASCell]
getCellsInSheet = getInSheet SheetLocsKey dbValToCell

getAllCells :: Connection -> IO [ASCell]
getAllCells conn = getCellsByKeyPattern conn "*"

deleteLocsInSheet :: Connection -> ASSheetId -> IO ()
deleteLocsInSheet = delInSheet SheetLocsKey

getCellsByKeyPattern :: Connection -> String -> IO [ASCell]
getCellsByKeyPattern conn pattern = do
  ks <- getKeysByPattern conn pattern
  let locs = mapMaybe readLoc ks
      readLoc k = case (S.maybeDecode k :: Maybe DBKey) of 
        Just (IndexKey i) -> Just i
        _ -> Nothing
  return locs
  map $fromJust <$> getCells conn locs

getKeysByPattern :: Connection -> String -> IO [B.ByteString]
getKeysByPattern conn pattern = runRedis conn $ $fromRight <$> keys (BC.pack pattern)

-- Gets the cells at the locations with expressions and values removed, but tags intact. 
getBlankedCellsAt :: Connection -> [ASIndex] -> IO [ASCell]
getBlankedCellsAt conn locs = 
  let blankCell (Cell l xp _ ts _ _) = Cell l (xp & expression .~ "") NoValue ts Nothing Nothing
  in do 
    cells <- getPossiblyBlankCells conn locs
    return $ map blankCell cells 

getPossiblyBlankCells :: Connection -> [ASIndex] -> IO [ASCell]
getPossiblyBlankCells conn locs = do 
  cells <- getCells conn locs
  return $ map (\(l,c) -> fromMaybe (blankCellAt l) c) (zip locs cells)

getPropsAt :: Connection -> ASIndex -> IO ASCellProps
getPropsAt conn ind = view cellProps <$> getPossiblyBlankCell conn ind

-- looks up cells in the given context, then in the database, in that precedence order
-- this function is order-preserving
getCellsWithContext :: Connection -> EvalContext -> [ASIndex] -> IO [Maybe ASCell]
getCellsWithContext conn ctx locs = map replaceWithContext <$> zip locs <$> getCells conn locs
  where
    replaceWithContext (l, c) = maybe c Just $ M.lookup l (ctx^.virtualCellsMap)

------------------------------------------------------------------------------------------------------------------------
-- Range descriptors and keys

getRangeKeysInSheet :: Connection -> ASSheetId -> IO [RangeKey]
getRangeKeysInSheet conn sid = map descriptorKey <$> getRangeDescriptorsInSheet conn sid

getRangeDescriptor :: Connection -> RangeKey -> IO (Maybe RangeDescriptor)
getRangeDescriptor conn rk = $head <$> getRangeDescriptors conn [rk]

getRangeDescriptors :: Connection -> [RangeKey] -> IO [Maybe RangeDescriptor]
getRangeDescriptors = multiGet RedisRangeKey dbValToRDesc

setDescriptor :: Connection -> RangeDescriptor -> IO ()
setDescriptor conn rd = setDescriptors conn [rd]

setDescriptors :: Connection -> [RangeDescriptor] -> IO ()
setDescriptors = setWithSheetFunc (SheetRangesKey . rangeKeyToSheetId) RedisRangeKey RangeDescriptorValue descriptorKey

deleteDescriptor :: Connection -> RangeKey -> IO ()
deleteDescriptor conn rk = deleteDescriptors conn [rk]

deleteDescriptors :: Connection -> [RangeKey] -> IO ()
deleteDescriptors = delWithSheetFunc (SheetRangesKey . rangeKeyToSheetId) RedisRangeKey 

getRangeDescriptorsInSheet :: Connection -> ASSheetId -> IO [RangeDescriptor]
getRangeDescriptorsInSheet = getInSheet SheetRangesKey dbValToRDesc

-- | Returns the listkeys of all the lists that are entirely contained in the range.  
fatCellsInRange :: Connection -> ASRange -> IO [RangeKey]
fatCellsInRange conn rng = do
  let sid = rangeSheetId rng
  rangeKeys <- getRangeKeysInSheet conn sid
  let rects = map rangeRect rangeKeys
      zipRects = zip rangeKeys rects
      zipRectsContained = filter (\(_,rect) -> rangeContainsRect rng rect) zipRects
  return $ map fst zipRectsContained

------------------------------------------------------------------------------------------------------------------------
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
--   PayloadProp _ rng -> canAccessAll conn uid (finiteRangeToIndices rng)
--   _ -> return True
-- commenting out 12/28 -- Alex

------------------------------------------------------------------------------------------------------------------------
-- Sheets 

getAllSheets :: Connection -> IO [ASSheet]
getAllSheets conn = do 
  keys <- getS AllSheetsKey dbValToKey conn
  catMaybes <$> multiGet id dbValToSheet conn keys

-- creates a sheet with unique id
createSheet :: Connection -> ASUserId -> String -> IO ASSheet
createSheet conn uid name = do
    sid <- T.pack <$> getUniqueId
    let sheet = Sheet sid name uid
    setSheet conn sheet
    return sheet

setSheet :: Connection -> ASSheet -> IO ()
setSheet conn sheet = do 
  let dbKey = SheetKey $ sheetId sheet
  setV conn dbKey (SheetValue sheet)
  addS conn AllSheetsKey (KeyValue dbKey)

------------------------------------------------------------------------------------------------------------------------
-- Repeat handlers

storeLastMessage :: Connection -> ServerMessage -> CommitSource -> IO () 
storeLastMessage conn msg src = case serverAction msg of 
  Repeat _ -> return ()
  _ -> do 
    let sheetKey = SheetLastMessagesKey $ srcSheetId src
    let dbKeys = [LastMessageKey src]
    let dbVals = [ServerMessageValue msg]
    setWithSheet conn sheetKey dbKeys dbVals

getLastMessage :: Connection -> CommitSource -> IO ServerMessage
getLastMessage conn src = $error "Currently not implemented"

------------------------------------------------------------------------------------------------------------------------
-- Conditional formatting handlers

getCondFormattingRules :: Connection -> ASSheetId ->  [CondFormatRuleId] -> IO [CondFormatRule]
getCondFormattingRules conn sid cfids = do
  let keys = map (CFRuleKey sid) cfids
  catMaybes <$> multiGet id dbValToCFRule conn keys

getCondFormattingRulesInSheet :: Connection -> ASSheetId -> IO [CondFormatRule]
getCondFormattingRulesInSheet = getInSheet SheetCFRulesKey dbValToCFRule
 
setCondFormattingRules :: Connection -> ASSheetId -> [CondFormatRule] -> IO ()
setCondFormattingRules conn sid rules = do 
  let sheetKey = SheetCFRulesKey sid 
  let dbKeys = map (CFRuleKey sid) $ map condFormatRuleId rules
  let dbVals = map CFValue rules
  setWithSheet conn sheetKey dbKeys dbVals
 
deleteCondFormattingRules :: Connection -> ASSheetId -> [CondFormatRuleId] -> IO ()
deleteCondFormattingRules conn sid cfids = do 
  let sheetKey = SheetCFRulesKey sid 
  let dbKeys = map (CFRuleKey sid) cfids 
  delWithSheet conn sheetKey dbKeys

------------------------------------------------------------------------------------------------------------------------
-- Row/col getters/setters

getBar :: Connection -> BarIndex -> IO (Maybe Bar)
getBar conn bInd  = $head <$> getBars conn [bInd]

getBars :: Connection -> [BarIndex] -> IO [Maybe Bar]
getBars = multiGet BarKey dbValToBar 

getBarsInSheet :: Connection -> ASSheetId -> IO [Bar]
getBarsInSheet = getInSheet SheetBarsKey dbValToBar 
 
setBars :: Connection -> [Bar] -> IO ()
setBars = setWithSheetFunc (SheetBarsKey . barSheetId) BarKey BarValue barIndex
 
setBar :: Connection -> Bar -> IO ()
setBar conn bar = setBars conn [bar]

deleteBarAt :: Connection -> BarIndex -> IO()
deleteBarAt conn bInd = deleteBarsAt conn [bInd]

deleteBarsAt :: Connection -> [BarIndex] -> IO ()
deleteBarsAt = delWithSheetFunc (SheetBarsKey . barSheetId) BarKey

deleteBarsInSheet :: Connection -> ASSheetId -> IO ()
deleteBarsInSheet = delInSheet SheetBarsKey

replaceBars :: Connection -> [Bar] -> [Bar] -> IO ()
replaceBars conn fromBars toBars = do
  deleteBarsAt conn $ map barIndex fromBars
  setBars conn toBars

------------------------------------------------------------------------------------------------------------------------
-- Header expressions handlers

getEvalHeader :: Connection -> ASSheetId -> ASLanguage -> IO EvalHeader
getEvalHeader conn sid lang = do 
  maybeEH <- getV conn (EvalHeaderKey sid lang) dbValToEvalHeaderBStr
  case maybeEH of 
    Nothing -> return $ EvalHeader sid lang (defaultHeaderText lang)
    Just msg -> return $ EvalHeader sid lang (BC.unpack msg)

setEvalHeader :: Connection -> EvalHeader -> IO ()
setEvalHeader conn evalHeader = do
  let sid  = evalHeader^.evalHeaderSheetId
      lang = evalHeader^.evalHeaderLang
      xp   = evalHeader^.evalHeaderExpr
  setV conn (EvalHeaderKey sid lang) (HeaderValue (BC.pack xp))

------------------------------------------------------------------------------------------------------------------------

-- Each commit source has a temp commit, used for decouple warnings
-- Key: commitSource + "tempcommit", value: ASCommit bytestring
getTempCommit :: Connection -> CommitSource -> IO (Maybe ASCommit)
getTempCommit conn src = getV conn (TempCommitKey src) dbValToCommit
  
setTempCommit :: Connection  -> CommitSource -> ASCommit -> IO ()
setTempCommit conn src c = do 
  let sheetKey = SheetTempCommitsKey $ srcSheetId src 
  let dbKeys = [TempCommitKey src]
  let dbVals = [CommitValue c]
  setWithSheet conn sheetKey dbKeys dbVals 
