{-# LANGUAGE OverloadedStrings, DataKinds, GADTs #-}

module AS.DB.API where

import Control.Arrow((&&&))
import Data.List (zip4,intercalate,find)
import Data.List.Split
import Data.Maybe hiding (fromJust)
import Data.SafeCopy (SafeCopy)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Time
import Database.Redis hiding (decode)
import Data.Aeson hiding (Success)
import Data.Either as DE
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

import qualified AS.LanguageDefs as LD

import AS.Prelude
import AS.Types.Cell
import AS.Types.Bar
import AS.Types.BarProps
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
import AS.Types.Network as N

import AS.Config.Constants
import AS.Config.Settings
import AS.DB.Internal
import AS.Util as U
import AS.Logging

------------------------------------------------------------------------------------------------------------------------
-- Raw cells API
-- all of these functions are order-preserving unless noted otherwise.

getCell :: Connection -> ASIndex -> IO (Maybe ASCell)
getCell conn loc = headMaybes <$> getCells conn [loc]

setCell :: Connection -> ASCell -> IO ()
setCell conn c = setCells conn [c]

getCells :: Connection -> [ASIndex] -> IO [Maybe ASCell]
getCells = multiGet IndexKey dbValToCell

setCells :: Connection -> [ASCell] -> IO ()
setCells = setWithSheetFunc (SheetLocsKey . view locSheetId) IndexKey CellDBValue (view cellLocation)

deleteLocs :: Connection -> [ASIndex] -> IO ()
deleteLocs = delWithSheetFunc (SheetLocsKey . view locSheetId) IndexKey

------------------------------------------------------------------------------------------------------------------------
-- Additional cell API methods

getPossiblyBlankCell :: Connection -> ASIndex -> IO (Maybe ASCell)
getPossiblyBlankCell conn loc = headMay <$> getPossiblyBlankCells conn [loc]

getCellsInSheet :: Connection -> SheetID -> IO [ASCell]
getCellsInSheet = getInSheet SheetLocsKey dbValToCell

deleteLocsInSheet :: Connection -> SheetID -> IO ()
deleteLocsInSheet = delInSheet SheetLocsKey

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

getPropsAt :: Connection -> ASIndex -> IO (Maybe ASCellProps)
getPropsAt conn ind = (view cellProps <$>) <$> getPossiblyBlankCell conn ind

------------------------------------------------------------------------------------------------------------------------
-- Range descriptors and keys

getRangeKeysInSheet :: Connection -> SheetID -> IO [RangeKey]
getRangeKeysInSheet conn sid = map descriptorKey <$> getRangeDescriptorsInSheet conn sid

getRangeDescriptor :: Connection -> RangeKey -> IO (Maybe RangeDescriptor)
getRangeDescriptor conn rk = headMaybes <$> getRangeDescriptors conn [rk]

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

getRangeDescriptorsInSheet :: Connection -> SheetID -> IO [RangeDescriptor]
getRangeDescriptorsInSheet = getInSheet SheetRangesKey dbValToRDesc

getRangeDescriptorsInWorkbook :: Connection
                              -> WorkbookID
                              -> IO [RangeDescriptor]
getRangeDescriptorsInWorkbook conn wid = do 
  wkbook <- getWorkbook conn wid 
  case wkbook of
    Nothing  -> return []
    Just wkb -> do
      let sids = Set.toList $ wkb^.workbookSheetIds
      concat <$> mapM (getRangeDescriptorsInSheet conn) sids

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

--canAccessSheet :: Connection -> UserID -> SheetID -> IO Bool
--canAccessSheet conn uid sheetId = do
--  mSheet <- getSheet conn sheetId
--  maybe (return False) (return . hasPermissions uid . sheetPermissions) mSheet

--canAccess :: Connection -> UserID -> ASIndex -> IO Bool
--canAccess conn uid loc = canAccessSheet conn uid (loc^.locSheetId)

--canAccessAll :: Connection -> UserID -> [ASIndex] -> IO Bool
--canAccessAll conn uid locs = all id <$> mapM (canAccess conn uid) locs

isPermissibleMessage :: UserID -> Connection -> ServerMessage -> IO Bool
isPermissibleMessage uid conn _ = return True
-- (ServerMessage _ payload) = case payload of 
--   PayloadCL cells -> canAccessAll conn uid (mapCellLocation cells)
--   PayloadLL locs -> canAccessAll conn uid locs
--   PayloadS sheet -> canAccessSheet conn uid (sheetId sheet)
--   PayloadW Window -> canAccessSheet conn uid (windowSheetId window)
--   PayloadProp _ rng -> canAccessAll conn uid (finiteRangeToIndices rng)
--   _ -> return True
-- commenting out 12/28 -- Alex

------------------------------------------------------------------------------------------------------------------------
-- Sheets 

getSheet :: Connection -> SheetID -> IO (Maybe Sheet)
getSheet conn sid = getV conn (SheetKey sid) dbValToSheet

getAllSheets :: Connection -> IO [Sheet]
getAllSheets conn = do 
  keys <- getS AllSheetsKey dbValToKey conn
  catMaybes <$> multiGet id dbValToSheet conn keys

-- | Creates a sheet with unique id
createSheet :: Connection -> UserID -> SheetName -> IO Sheet
createSheet conn uid name = do
  sid <- T.pack <$> getUUID
  let sheet = Sheet sid name uid False
  setSheet conn sheet
  return sheet

setSheet :: Connection -> Sheet -> IO ()
setSheet conn sheet = do 
  let dbKey = SheetKey $ sheet^.sheetId
  setV conn dbKey (SheetValue sheet)
  addS conn AllSheetsKey (KeyValue dbKey)

deleteSheet :: Connection -> SheetID -> IO ()
deleteSheet conn sid = do
  multiDel SheetKey conn [sid]
  remS conn AllSheetsKey (KeyValue $ SheetKey sid)

getContainingWorkbook :: Connection -> SheetID -> IO WorkbookID
getContainingWorkbook conn sid = do
  sheet <- fromJust <$> getSheet conn sid
  user <- fromJust <$> getUser conn (sheet^.sheetOwner)
  let wids = Set.toList $ user^.workbookIds
  sidSets <- mapM (getWorkbookSheetIds conn) wids
  let sidSets' = zip wids sidSets
  let [(containerWid, _)] = filter (\(_,sids) -> sid `elem` sids) sidSets'
  return containerWid

--------------------------------------------------------------------------------
-- Workbooks

getWorkbook :: Connection -> WorkbookID -> IO (Maybe Workbook)
getWorkbook conn wid = getV conn (WorkbookKey wid) dbValToWorkbook

getOpenedWorkbook :: Connection -> WorkbookID -> IO (Maybe OpenedWorkbook)
getOpenedWorkbook conn wid = do
  mwb <- getWorkbook conn wid
  case mwb of 
    Nothing -> return Nothing
    Just wb -> do
      sheets <- catMaybes <$> mapM (getSheet conn) (Set.toList $ wb^.workbookSheetIds)
      return . Just $ OpenedWorkbook
        { openedWorkbookId = wb^.workbookId
        , openedWorkbookName = wb^.workbookName
        , openedWorkbookOwner = wb^.workbookOwner
        , openedWorkbookSheets = sheets
        , openedSheet = wb^.lastOpenSheet
        }

getWorkbooks :: Connection -> [WorkbookID] -> IO [Maybe Workbook]
getWorkbooks = multiGet WorkbookKey dbValToWorkbook

getWorkbookSheetIds :: Connection -> WorkbookID -> IO [SheetID]
getWorkbookSheetIds conn wid = 
  maybeM 
    (getWorkbook conn wid) 
    (return []) 
    $ \wb -> 
      return $ Set.toList . view workbookSheetIds $ wb 

createWorkbook :: Connection -> UserID -> WorkbookName -> IO Workbook
createWorkbook conn uid wname = do 
  sheet <- createSheet conn uid new_sheet_name
  let sid = sheet^.sheetId
  wid <- T.pack <$> getUUID
  let workbook = Workbook wid wname (Set.singleton sid) uid sid
  setWorkbook conn workbook
  return workbook

setWorkbook :: Connection -> Workbook -> IO ()
setWorkbook conn workbook = do 
  let dbKey = WorkbookKey $ workbook^.workbookId
  setV conn dbKey (WorkbookValue workbook)
  addS conn AllWorkbooksKey (KeyValue dbKey)

modifyWorkbook :: Connection -> WorkbookID -> (Workbook -> Workbook) -> IO Workbook
modifyWorkbook conn wid f = 
  getWorkbook conn wid >>= return . f . fromJust >>= \w -> setWorkbook conn w >> return w

getAllWorkbooks :: Connection -> IO [Workbook]
getAllWorkbooks conn = do 
  keys <- getS AllWorkbooksKey dbValToKey conn
  catMaybes <$> multiGet id dbValToWorkbook conn keys

--------------------------------------------------------------------------------
-- Users

getUser :: Connection -> UserID -> IO (Maybe User)
getUser conn uid = getV conn (UserKey uid) dbValToUser

setUser :: Connection -> User -> IO ()
setUser conn user = setV conn (UserKey $ view AS.Types.User.userId $ user) (UserValue user)

deleteUser :: Connection -> UserID -> IO ()
deleteUser conn uid = delV conn (UserKey uid)

modifyUser :: Connection -> UserID -> (User -> User) -> IO User
modifyUser conn uid f = 
  getUser conn uid >>= return . f . fromJust >>= \u -> setUser conn u >> return u

-- | Get all sheets in the user's currently open workbook.
getOpenedSheets :: Connection -> UserID -> IO [Sheet]
getOpenedSheets conn uid = do
  maybeM 
    (getUser conn uid)
    (return [])
    $ \user -> do
      wb <- fromJust <$> getWorkbook conn (user^.lastOpenWorkbook)
      let sids = Set.toList $ wb^.workbookSheetIds
      map fromJust <$> mapM (getSheet conn) sids

getUserWorkbookRefs :: Connection -> UserID -> IO [WorkbookRef]
getUserWorkbookRefs conn uid = do
  user <- fromJust <$> getUser conn uid
  let wids = Set.toList $ user^.workbookIds
  wbs <- forM wids $ fmap fromJust . getWorkbook conn
  return $ map (\wb -> WorkbookRef (wb^.workbookId) (wb^.workbookName) (wb^.workbookOwner)) wbs


--------------------------------------------------------------------------------
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
getLastMessage conn src = error "Currently not implemented"

------------------------------------------------------------------------------------------------------------------------
-- Conditional formatting handlers

getCondFormattingRules :: Connection -> SheetID ->  [CondFormatRuleId] -> IO [CondFormatRule]
getCondFormattingRules conn sid cfids = do
  let keys = map (CFRuleKey sid) cfids
  catMaybes <$> multiGet id dbValToCFRule conn keys

getCondFormattingRulesInSheet :: Connection -> SheetID -> IO [CondFormatRule]
getCondFormattingRulesInSheet = getInSheet SheetCFRulesKey dbValToCFRule
 
setCondFormattingRules :: Connection -> SheetID -> [CondFormatRule] -> IO ()
setCondFormattingRules conn sid rules = do 
  let sheetKey = SheetCFRulesKey sid 
  let dbKeys = map (CFRuleKey sid) $ map condFormatRuleId rules
  let dbVals = map CFValue rules
  setWithSheet conn sheetKey dbKeys dbVals
 
deleteCondFormattingRules :: Connection -> SheetID -> [CondFormatRuleId] -> IO ()
deleteCondFormattingRules conn sid cfids = do 
  let sheetKey = SheetCFRulesKey sid 
  let dbKeys = map (CFRuleKey sid) cfids 
  delWithSheet conn sheetKey dbKeys

------------------------------------------------------------------------------------------------------------------------
-- Row/col getters/setters

getBar :: Connection -> BarIndex -> IO (Maybe Bar)
getBar conn bInd = headMaybes <$> getBars conn [bInd]

getBars :: Connection -> [BarIndex] -> IO [Maybe Bar]
getBars = multiGet BarKey dbValToBar 

getBarsInSheet :: Connection -> SheetID -> IO [Bar]
getBarsInSheet = getInSheet SheetBarsKey dbValToBar 
 
setBars :: Connection -> [Bar] -> IO ()
setBars = setWithSheetFunc (SheetBarsKey . barSheetId) BarKey BarValue barIndex
 
setBar :: Connection -> Bar -> IO ()
setBar conn bar = setBars conn [bar]

deleteBarAt :: Connection -> BarIndex -> IO()
deleteBarAt conn bInd = deleteBarsAt conn [bInd]

deleteBarsAt :: Connection -> [BarIndex] -> IO ()
deleteBarsAt = delWithSheetFunc (SheetBarsKey . barSheetId) BarKey

deleteBarsInSheet :: Connection -> SheetID -> IO ()
deleteBarsInSheet = delInSheet SheetBarsKey

replaceBars :: Connection -> [Bar] -> [Bar] -> IO ()
replaceBars conn fromBars toBars = do
  deleteBarsAt conn $ map barIndex fromBars
  setBars conn toBars

------------------------------------------------------------------------------------------------------------------------
-- Header expressions handlers

getEvalHeader :: Connection -> WorkbookID -> ASLanguage -> IO EvalHeader
getEvalHeader conn wid lang = do 
  maybeEH <- getV conn (EvalHeaderKey wid lang) dbValToEvalHeaderBStr
  case maybeEH of 
    Nothing -> return $ EvalHeader wid lang (defaultHeaderText lang)
    Just msg -> return $ EvalHeader wid lang (BC.unpack msg)

getEvalHeaders :: Connection -> WorkbookID -> IO [EvalHeader]
getEvalHeaders conn wid = forM headerLangs $ getEvalHeader conn wid

setEvalHeader :: Connection -> EvalHeader -> IO ()
setEvalHeader conn evalHeader = do
  let wid  = evalHeader^.evalHeaderWorkbookId
      lang = evalHeader^.evalHeaderLang
      xp   = evalHeader^.evalHeaderExpr
  setV conn (EvalHeaderKey wid lang) (HeaderValue (BC.pack xp))

deleteEvalHeaders :: Connection -> WorkbookID -> IO ()
deleteEvalHeaders conn wid = forM_ headerLangs $ \lang -> 
  delV conn (EvalHeaderKey wid lang)

getAllHeaders :: Connection -> ASLanguage -> IO [EvalHeader]
getAllHeaders conn lang = do
  wids <- map (view workbookId) <$> getAllWorkbooks conn
  mapM (\wid -> getEvalHeader conn wid lang) wids

-- | Invoked when "acquiring" a sheet from another user. EvalHeaders are 
-- workbook-specific, so the rather messy solution now is to just concatenate
-- the other user's headers to your own. anand 5/25
acquireHeaders :: Connection -> WorkbookID -> SheetID -> IO [EvalHeader]
acquireHeaders conn toWid fromSid = do
  (Just acquiredWb) <- getWorkbook conn =<< getContainingWorkbook conn fromSid
  headers <- getEvalHeaders conn toWid
  acquiredHeaders <- getEvalHeaders conn $ acquiredWb^.workbookId
  let wname = acquiredWb^.workbookName
  let uid = acquiredWb^.workbookOwner
  let title = "WORKBOOK: (owned by " ++ show uid ++ ") " ++ wname
  let headers' = zipWith (LD.mergeHeaders title) headers acquiredHeaders 
  mapM_ (setEvalHeader conn) headers'
  return headers'

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
