{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module AS.Types.DB where

import AS.Prelude
import Data.Maybe
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString               as B
import Data.ByteString (ByteString)
import Data.SafeCopy
import Control.Monad
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Database.Redis 

import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Locations
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.CellProps
import AS.Types.Bar
import AS.Types.CondFormat
import AS.Types.User
import AS.Types.Messages
import AS.Types.BarProps
import AS.Types.Formats
import AS.Types.Updates
import AS.Types.Window
import AS.Types.Selection
import AS.Types.Mutate
import AS.Serialize as S 

data DBSheetGroupKey = 
    SheetRangesKey       SheetID
  | SheetTempCommitsKey  SheetID  
  | SheetLastMessagesKey SheetID 
  | SheetLocsKey         SheetID
  | SheetCFRulesKey      SheetID 
  | SheetBarsKey         SheetID
  deriving (Generic, Show)
deriveSafeCopy 1 'base ''DBSheetGroupKey

data DBKey = 
    NullKey -- basically /dev/null for the database, when doing migrations send keys which should be deleted here
  | SheetKey             SheetID     
  | WorkbookKey          WorkbookID        
  | EvalHeaderKey        WorkbookID ASLanguage        
  | TempCommitKey        CommitSource        
  | PushCommitKey        CommitSource        
  | PopCommitKey         CommitSource         
  | LastMessageKey       CommitSource       
  | SheetGroupKey        DBSheetGroupKey
  | CFRuleKey            SheetID CondFormatRuleId
  | BarKey               BarIndex             
  | UserKey              UserID
  | IndexKey             ASIndex  
  | RedisRangeKey        RangeKey  
  | LogKey               LogSource 
  | AllSheetsKey
  | AllWorkbooksKey
  deriving (Generic, Show)
deriveSafeCopy 2 'extension ''DBKey

data DBValue = 
    KeyValue DBKey
  | UserValue User
  | SheetValue Sheet
  | WorkbookValue Workbook
  | HeaderValue ByteString
  | CommitValue ASCommit 
  | CellDBValue ASCell 
  | ServerMessageValue ServerMessage
  | BarValue Bar
  | CFValue CondFormatRule
  | IndexValue ASIndex
  | RangeDescriptorValue RangeDescriptor 
  | LogValue LogData
  deriving (Generic, Show)
deriveSafeCopy 1 'base ''DBValue


---------------------------------------------------------------------------------------------------------------
-- DBKey/DBValue migrations


data DBKey0 = 
    SheetKey0             SheetID             
  | EvalHeaderKey0        SheetID ASLanguage        
  | TempCommitKey0        CommitSource        
  | PushCommitKey0        CommitSource        
  | PopCommitKey0         CommitSource         
  | LastMessageKey0       CommitSource       
  | SheetGroupKey0        DBSheetGroupKey
  | CFRuleKey0            SheetID CondFormatRuleId
  | BarKey0               BarIndex             
  | UserKey0              UserID
  | IndexKey0             ASIndex  
  | WorkbookKey0          WorkbookName
  | RedisRangeKey0        RangeKey  
  | LogKey0               LogSource 
  | AllSheetsKey0
  deriving (Generic, Show)
deriveSafeCopy 1 'base ''DBKey0

instance Migrate DBKey where
  type MigrateFrom DBKey = DBKey0
  migrate (SheetKey0 s) = SheetKey s
  migrate (EvalHeaderKey0 s l) = EvalHeaderKey s l
  migrate (TempCommitKey0 c) = TempCommitKey c
  migrate (PushCommitKey0 c) = PushCommitKey c
  migrate (PopCommitKey0 c) = PopCommitKey c
  migrate (LastMessageKey0 c) = LastMessageKey c
  migrate (SheetGroupKey0 g) = SheetGroupKey g
  migrate (CFRuleKey0 s c) = CFRuleKey s c
  migrate (BarKey0 b) = BarKey b 
  migrate (UserKey0 u) = UserKey u
  migrate (IndexKey0 i) = IndexKey i
  migrate (WorkbookKey0 _) = NullKey
  migrate (RedisRangeKey0 r) = RedisRangeKey r
  migrate (LogKey0 l) = LogKey l
  migrate AllSheetsKey0 = AllSheetsKey

---------------------------------------------------------------------------------------------------------------
-- Exporting

data ExportData = ExportData { _exportCells           :: [ASCell]
                             , _exportBars            :: [Bar]
                             , _exportDescriptors     :: [RangeDescriptor]
                             , _exportCondFormatRules :: [CondFormatRule]
                             , _exportHeaders         :: [EvalHeader] } deriving (Show, Read, Eq, Generic)

makeLenses ''ExportData
deriveSafeCopy 1 'base ''ExportData

-- #incomplete Assumes the export has at least one cell. 
exportDataSheetId :: ExportData -> SheetID
exportDataSheetId = (view (cellLocation.locSheetId)) . $head . view exportCells

cloneData :: SheetID -> ExportData -> ExportData
cloneData sid ex = 
    (& exportCells %~ map moveCell)
  . (& exportBars %~ map moveBar)
  . (& exportDescriptors %~ map moveDescriptor)
  . (& exportCondFormatRules %~ map moveRule)
  . (& exportHeaders %~ map moveHeader)
  $ ex
  where
    moveCell = (& cellLocation.locSheetId .~ sid)
    moveBar b = b {barIndex = (barIndex b) {barSheetId = sid}}
    moveDescriptor d = d 
      { descriptorKey = 
        let key = descriptorKey d
        in key {keyIndex = (keyIndex key) & locSheetId .~ sid}
      }
    moveRule r = r {cellLocs = map moveRange (cellLocs r)} 
    moveRange r = r {rangeSheetId = sid}
    moveHeader = (& evalHeaderWorkbookId .~ sid)
  
---------------------------------------------------------------------------------------------------------------
-- Conversion from DBValue 

dbValToKey :: DBValue -> Maybe DBKey 
dbValToKey (KeyValue k) = Just k
dbValToKey _ = Nothing

dbValToCell :: DBValue -> Maybe ASCell 
dbValToCell (CellDBValue c) = Just c 
dbValToCell _ = Nothing

dbValToUser :: DBValue -> Maybe User
dbValToUser (UserValue u) = Just u 
dbValToUser _ = Nothing

dbValToCFRule :: DBValue -> Maybe CondFormatRule 
dbValToCFRule (CFValue c) = Just c 
dbValToCFRule _ =  Nothing

dbValToRDesc :: DBValue -> Maybe RangeDescriptor 
dbValToRDesc (RangeDescriptorValue rd) = Just rd 
dbValToRDesc _ = Nothing

dbValToSheet :: DBValue -> Maybe Sheet 
dbValToSheet (SheetValue s) = Just s 
dbValToSheet _ = Nothing 

dbValToWorkbook :: DBValue -> Maybe Workbook 
dbValToWorkbook (WorkbookValue w) = Just w
dbValToWorkbook _ = Nothing

dbValToBar :: DBValue -> Maybe Bar 
dbValToBar (BarValue b) = Just b 
dbValToBar _ = Nothing

dbValToEvalHeaderBStr :: DBValue -> Maybe ByteString 
dbValToEvalHeaderBStr (HeaderValue b) = Just b 
dbValToEvalHeaderBStr _ = Nothing

dbValToCommit :: DBValue -> Maybe ASCommit 
dbValToCommit (CommitValue c) = Just c 
dbValToCommit _ = Nothing

dbValToLogData :: DBValue -> Maybe LogData
dbValToLogData (LogValue l) = Just l 
dbValToLogData _ = Nothing

---------------------------------------------------------------------------------------------------------------
-- Conversion helpers 

fromBS :: (SafeCopy a) => (a -> Maybe b) -> [Maybe ByteString] -> [Maybe b]
fromBS conv = map (\b -> b >>=  S.maybeDecode >>= conv)

fromBS' :: (SafeCopy a) => (a -> Maybe b) -> [ByteString] -> [b]
fromBS' conv = mapMaybe (\b -> S.maybeDecode b >>= conv)

---------------------------------------------------------------------------------------------------------------

-- Given a function producing a SheetGroupKey from a SheetId, and a value decoder, get all X in a sheet
-- First gets all of the keys in the set associated with the SheetGroupKey
-- Then does an mget on those keys to get the associated values in this sheet, and decodes them. 
getInSheet :: (SheetID -> DBSheetGroupKey) -> (DBValue -> Maybe b) -> Connection -> SheetID -> IO [b]
getInSheet toSheetGroupKey fromDBValue conn sid = runRedis conn $ do
  -- get all the dbKeys associated with the sheetGroupKey
  Right dbValKeys <- smembers (S.encode $ SheetGroupKey $ toSheetGroupKey sid)
  if dbValKeys == []
    then return []
    else do 
      let dbKeys = fromBS' dbValToKey dbValKeys
      Right vals <- mget $ map S.encode dbKeys 
      return $ catMaybes $ fromBS fromDBValue vals

-- Given a SheetGroupKey, a list of keys, and a list of vals, 
-- (1) mset the list of keys and values
-- (2) add all of the encoded keys to the set of keys associated with the SheetGroupKey
setWithSheet :: Connection -> DBSheetGroupKey -> [DBKey] -> [DBValue] -> IO ()
setWithSheet conn sheetKey keys vals = runRedis conn $ do 
  let encKeys = map S.encode keys
  let encVals = map S.encode vals
  let encKeyValues = map (S.encode . KeyValue) keys
  mset $ zip encKeys encVals
  sadd (S.encode (SheetGroupKey sheetKey)) encKeyValues
  return ()

-- Similar as above, but takes some function helpers
-- We want to set a list of b's in the DB, while also preserving sheet groupings
setWithSheetFunc :: (a -> DBSheetGroupKey) -> (a -> DBKey) -> (b -> DBValue) -> (b -> a) -> Connection -> [b] -> IO ()
setWithSheetFunc toSidKey toKey toValue valToKey conn vals = do 
  multiSet toKey toValue valToKey conn vals 
  runRedis conn $ do 
    let encSheetKey = S.encode . SheetGroupKey . toSidKey . valToKey
    let encKeyValue =  S.encode . KeyValue . toKey . valToKey
    let addSet x = sadd (encSheetKey x) [encKeyValue x]
    mapM addSet vals
  return ()

-- Delete all keys in the provided list of keys, and all keys associated with the SheetGroupKey
delWithSheet :: Connection -> DBSheetGroupKey -> [DBKey] -> IO ()
delWithSheet conn sheetKey keys = runRedis conn $ do 
  let encKeys = map S.encode keys
  let encKeyValues = map (S.encode . KeyValue) keys
  del encKeys
  srem (S.encode (SheetGroupKey sheetKey)) encKeyValues 
  return ()

-- Similar to above, but takes in some function helpers
delWithSheetFunc :: (a -> DBSheetGroupKey) -> (a -> DBKey) -> Connection -> [a] -> IO ()
delWithSheetFunc toSidKey toKey conn keys = do 
  multiDel toKey conn keys 
  runRedis conn $ do 
    let encSheetKey = S.encode . SheetGroupKey . toSidKey 
    let encKeyValue = S.encode . KeyValue . toKey 
    let remSet x = srem (encSheetKey x) [encKeyValue x]
    mapM remSet keys
  return ()

-- Delete everything associated with a SheetGroupKey in a given sheet
-- Get all of the keys in the set associated with the SheetGroupKey and delete them
delInSheet :: (SheetID -> DBSheetGroupKey) -> Connection -> SheetID -> IO ()
delInSheet toKey conn sid = runRedis conn $ do  
  let sheetKey = S.encode $ SheetGroupKey $ toKey sid
  Right keyStrings <- smembers sheetKey
  let keys = map S.encode $ fromBS' dbValToKey keyStrings
  del $ sheetKey:keys
  return ()

---------------------------------------------------------------------------------------------------------------

-- Given a key encoder and value decoder, get a bunch of objects from the db that are direct key-value pairs
multiGet :: (a -> DBKey) -> (DBValue -> Maybe b) -> Connection -> [a] -> IO [Maybe b]
multiGet _ _ _ [] = return []
multiGet toDBKey fromDBValue conn [x] = runRedis conn $ do 
  Right val <- get $ S.encode $ toDBKey x
  return $ fromBS fromDBValue [val] 
multiGet toDBKey fromDBValue conn xs = runRedis conn $ do 
  Right vals <- mget $ map (S.encode . toDBKey) xs
  return $ fromBS fromDBValue vals

-- Given a key encoder, a value decoder, and a function to produce a key, set a bunch of objects in the DB as
-- direct key-value pairs
multiSet :: (a -> DBKey) -> (b -> DBValue) -> (b -> a) -> Connection -> [b] -> IO ()
multiSet _ _ _ _ [] = return ()
multiSet toDBKey toDBValue valToKey conn [x] = runRedis conn $ do 
  set (S.encode $ toDBKey $ valToKey x) (S.encode $ toDBValue x)
  return ()
multiSet toDBKey toDBValue valToKey conn xs = runRedis conn $ do 
  let keys = map (S.encode . toDBKey . valToKey) xs
  let vals = map (S.encode . toDBValue) xs
  mset $ zip keys vals
  return ()

-- Delete multiple keys at once
multiDel :: (a -> DBKey) -> Connection -> [a] -> IO ()
multiDel _ _ [] = return ()
multiDel toDBKey conn xs = void $ runRedis conn $ del $ map (S.encode . toDBKey) xs

-- Given a key and a value decoder, simply get a key-value pair
getV :: Connection -> DBKey -> (DBValue -> Maybe b) -> IO (Maybe b)
getV conn key fromDBValue = runRedis conn $ do 
  Right val <- get $ S.encode key
  return $ val >>= S.maybeDecode >>= fromDBValue

-- Given a dbkey and dbvalue, just set that pair in the DB 
setV :: Connection -> DBKey -> DBValue -> IO ()
setV conn key val = void $ runRedis conn $ set (S.encode key) (S.encode val) 

delV :: Connection -> DBKey -> IO ()
delV conn key = void $ runRedis conn $ del [S.encode key]

-- Given a key and a value decoder, get all of the values in the set associated with this key
getS :: DBKey -> (DBValue -> Maybe b) -> Connection -> IO [b]
getS key fromDBValue conn = runRedis conn $ do 
  Right vals <- smembers (S.encode key)
  let dbVals = map S.maybeDecode vals
  return $ mapMaybe (fromDBValue =<<) dbVals

-- Add a value to a set associated with the key
addS :: Connection -> DBKey -> DBValue -> IO ()
addS conn key val = void $ runRedis conn $ sadd (S.encode key) [S.encode val]

-- Remove a value from a set associated with the key
remS :: Connection -> DBKey -> DBValue -> IO ()
remS conn key val = void $ runRedis conn $ srem (S.encode key) [S.encode val] 

-- Add a value to the right end of a list at key
addL :: Connection -> DBKey -> DBValue -> IO ()
addL conn key val = void $ runRedis conn $ rpush (S.encode key) [S.encode val]

-- Get all members of a list at key, given a value decoder
getL :: Connection -> DBKey -> (DBValue -> Maybe a) -> IO [a]
getL conn key fromDBValue = runRedis conn $ do 
  Right vals <- lrange (S.encode key) 0 (-1)
  return $ fromBS' fromDBValue vals

---------------------------------------------------------------------------------------------------------------
-- Instances 

deriveSafeCopy 1 'base ''CommitSource
deriveSafeCopy 1 'base ''Workbook

instance NFData DBSheetGroupKey where rnf = genericRnf
instance NFData DBKey     where rnf = genericRnf
instance NFData DBValue   where rnf = genericRnf
instance NFData CommitSource     where rnf = genericRnf
instance NFData ASLanguage     where rnf = genericRnf
instance NFData BarIndex     where rnf = genericRnf
instance NFData RangeKey     where rnf = genericRnf
instance NFData BarCoord     where rnf = genericRnf
instance NFData Dimensions     where rnf = genericRnf
instance NFData User     where rnf = genericRnf
instance NFData RangeDescriptor      where rnf = genericRnf
instance NFData ASCell      where rnf = genericRnf
instance NFData JSONField      where rnf = genericRnf
instance NFData Bar      where rnf = genericRnf
instance NFData ExpandingType      where rnf = genericRnf
instance NFData ASCellProps      where rnf = genericRnf
instance NFData JSONValue      where rnf = genericRnf
instance NFData CellPropType      where rnf = genericRnf
instance NFData ASBarProps      where rnf = genericRnf
instance NFData BarPropType      where rnf = genericRnf
instance NFData CellProp      where rnf = genericRnf
instance NFData FormatMapConstructor      where rnf = genericRnf
instance NFData BoolCondition      where rnf = genericRnf
instance NFData ASExpression      where rnf = genericRnf
instance NFData BarProp      where rnf = genericRnf
instance NFData ASCommit      where rnf = genericRnf
instance NFData Format      where rnf = genericRnf
instance NFData ServerMessage      where rnf = genericRnf
instance NFData TwoExprBoolCondType      where rnf = genericRnf
instance NFData VAlignType      where rnf = genericRnf
instance NFData FormatType      where rnf = genericRnf
instance (NFData a) => NFData (Diff a)      where rnf = genericRnf
instance (NFData a, NFData b) => NFData (Update a b) where rnf = genericRnf
instance NFData Stream      where rnf = genericRnf
instance NFData OneExprBoolCondType      where rnf = genericRnf
instance NFData NoExprBoolCondType      where rnf = genericRnf
instance NFData ServerAction      where rnf = genericRnf
instance NFData StreamSource      where rnf = genericRnf
instance NFData HAlignType      where rnf = genericRnf
instance NFData ASTime      where rnf = genericRnf
instance NFData CondFormatRule      where rnf = genericRnf
instance NFData EvalHeader      where rnf = genericRnf
instance NFData Bloomberg      where rnf = genericRnf
instance NFData Window      where rnf = genericRnf
instance NFData Selection      where rnf = genericRnf
instance NFData Mutate where rnf = genericRnf
instance (NFData a) => NFData (MutateTypeNew a) where rnf = genericRnf
instance NFData EvalInstruction      where rnf = genericRnf
instance NFData LogSource where rnf = genericRnf
instance NFData LogData where rnf = genericRnf
