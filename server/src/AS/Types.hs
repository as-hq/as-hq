{-# LANGUAGE DeriveGeneric #-}

module AS.Types where

import Prelude
import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Text hiding (foldr)
import qualified Network.WebSockets as WS
import qualified Database.Redis as R

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Sheets

type ASSheetId = Text
data ASSheet = Sheet {sheetId :: ASSheetId, sheetName :: String, sheetPermissions :: ASPermissions} deriving (Show, Read, Eq, Generic)
data ASWorkbook = Workbook {workbookName :: String, workbookSheets :: [ASSheetId]}  deriving (Show, Read, Eq, Generic)

data WorkbookSheet = WorkbookSheet {wsName :: String, wsSheets :: [ASSheet]} deriving (Show, Read, Eq, Generic)
----------------------------------------------------------------------------------------------------------------------------------------------
-- | Core cell types

-- -1 in "row" for Index signifies an entire column
data ASLocation = Index {locSheetId :: ASSheetId, index :: (Int, Int)} | 
                  Range {locSheetId :: ASSheetId, range :: ((Int, Int), (Int, Int))} |
                  Column {locSheetId :: ASSheetId, column :: Int}
                  deriving (Show, Read, Eq, Generic, Ord)

data ASValue =
  NoValue |
  ValueNaN () |
  ValueS String |
  ValueI Int |
  ValueD Double | 
  ValueB Bool |
  ValueL [ASValue] |
  ExcelSheet { locs :: ASValue, exprs :: ASValue, vals :: ASValue} |
  Rickshaw {rickshawData :: ASValue} |
  ValueError { error :: String, err_type :: String, file :: String, position :: Int } | 
  ValueImage { imagePath :: String} |
  StockChart { stockPrices :: ASValue, stockName :: String } |
  ObjectValue { objectType :: String, jsonRepresentation :: String } |
  StyledValue { style :: String, value :: ASValue } |
  DisplayValue { displayValue :: String, actualValue :: ASValue }|
  ValueE ASEvalError
  deriving (Show, Read, Eq, Generic)

data ASLangValue = LangValue {langValue :: ASValue, lang :: ASLanguage} deriving (Show, Read, Eq, Generic)

type ASEvalError = String

data ASLanguage = R | Python | OCaml | CPP | Java | SQL | Excel deriving (Show, Read, Eq, Generic)

-- TODO consider migration to exLocs record
data ASExpression =
  Expression { expression :: String, language :: ASLanguage } | 
  Reference { location :: ASLocation, referenceIndex :: (Int, Int) }
  deriving (Show, Read, Eq, Generic)

data ASCellTag = 
  Color String |
  Size Int |
  Money |
  Percentage |
  StreamTag Stream |
  Tracking |
  Volatile |
  ReadOnly [ASUserId]
  deriving (Show, Read, Eq, Generic)

data ASCell = Cell {cellLocation :: ASLocation, 
					cellExpression :: ASExpression,
					cellValue :: ASValue,
          cellTags :: [ASCellTag]} deriving (Show, Read, Eq, Generic)


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel

-- TODO fix recursion
data ExLoc = ExSheet {name :: String, sheetLoc :: ExLoc} |
             ExRange {first :: ExLoc, second :: ExLoc}     |
             ExIndex {d1 :: String, col :: String, d2 :: String, row :: String} 
             deriving (Show,Read,Eq,Ord)

data ExcelAction = 
  Lookup |
  CheckIndirectRef |
  LookupSheets |
  LookupWorkbooks |
  CurrentLocation 
  deriving (Show,Read)

data ExcelPayload = 
  NoPayload 
  deriving (Show,Read)
  
----------------------------------------------------------------------------------------------------------------------------------------------
-- | Streaming
-- | Stream sources
data Bloomberg = Bloomberg {url :: String, key :: String} deriving (Show, Read, Eq, Generic)
data StreamSource = StreamB Bloomberg | NoSource deriving (Show, Read, Eq, Generic)

-- | A stream just needs a source and a frequency
data Stream = Stream {streamSource :: StreamSource, streamFreq :: Int} deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Message Types

data ASMessage = Message {
  messageUserId :: ASUserId,
  action :: ASAction,
  result :: ASResult,
  payload :: ASPayload
} deriving (Show, Read, Eq, Generic)

data ASAction = 
  NoAction |
  Acknowledge |
  New | Import | 
  Open | Close |
  Evaluate | EvaluateRepl |
  Update | 
  Get | Delete |
  Copy | CopyForced |
  Undo | Redo |
  Clear | 
  UpdateWindow |
  AddTags | RemoveTags
  deriving (Show, Read, Eq, Generic)

data ASResult = Success | Failure {failDesc :: String} | NoResult deriving (Show, Read, Eq, Generic)

-- for open, close dialogs
data QueryList = 
  Sheets |
  Workbooks |
  WorkbookSheets
  deriving (Show, Read, Eq, Generic)

data ASPayload = 
  PayloadN () |
  PayloadInit ASInitConnection |
  PayloadDaemonInit ASInitDaemonConnection |
  PayloadC ASCell | 
  PayloadCL [ASCell] | 
  PayloadL ASLocation |
  PayloadLL [ASLocation] |
  PayloadS ASSheet |
  PayloadSS [ASSheet] |
  PayloadWB ASWorkbook |
  PayloadWBS [ASWorkbook] |
  PayloadWorkbookSheets [WorkbookSheet] |
  PayloadW ASWindow |
  PayloadU ASUserId |
  PayloadE ASExecError |
  PayloadCommit ASCommit |
  PayloadTags {tags :: [ASCellTag], tagsLoc :: ASLocation} |
  PayloadXp ASExpression |
  PayloadLangValue ASLangValue |
  PayloadList QueryList 
  deriving (Show, Read, Eq, Generic)


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Eval Types

data ASExecError = 
  Timeout | 
  EvaluationError {evalErrorDesc :: String} |
  DependenciesLocked {lockUserId :: ASUserId} | 
  DBNothingException {badLocs :: [ASLocation]} |
  DBGraphUnreachable | 
  NetworkDown | 
  ResourceLimitReached |
  InsufficientPermissions |
  NonUniqueIdentifier |
  CopyNonexistentDependencies
  deriving (Show, Read, Eq, Generic)

type EitherCells = Either ASExecError [ASCell] 

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Websocket types

data ASInitConnection = ASInitConnection {connUserId :: ASUserId} deriving (Show,Read,Eq,Generic)
data ASInitDaemonConnection = ASInitDaemonConnection {parentUserId :: ASUserId, initDaemonLoc :: ASLocation} deriving (Show,Read,Eq,Generic)

data ASDaemon = ASDaemon {daemonLoc :: ASLocation, daemonConn :: WS.Connection}

instance Eq ASDaemon where 
  c1 == c2 = (daemonLoc c1) == (daemonLoc c2)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Database and state

data ServerState = State {userList :: [(ASUser,[ASDaemon])], dbConn :: R.Connection} 

data GraphQuery = 
  GetDescendants |
  GetImmediateAncestors |
  SetRelations 
  deriving (Show)
----------------------------------------------------------------------------------------------------------------------------------------------
-- | Users

data ASWindow = Window {windowSheetId :: ASSheetId, topLeft :: (Int, Int), bottomRight :: (Int, Int)} deriving (Show,Read,Eq,Generic)
type ASUserId = Text 
data ASUser = User {userId :: ASUserId, userConn :: WS.Connection, userWindows :: [ASWindow]} 

instance Eq ASUser where 
  c1 == c2 = (userId c1) == (userId c2)

data ASUserGroup = Group {groupMembers :: [ASUserId], groupAdmins :: [ASUserId], groupName :: Text} deriving (Show, Read, Eq, Generic)
data ASEntity = EntityGroup ASUserGroup|
                EntityUser ASUserId
                deriving (Show, Read, Eq, Generic)

data ASPermissions = Blacklist [ASEntity] |
                     Whitelist [ASEntity]
                      deriving (Show, Read, Eq, Generic)


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Version Control

data ASTime = Time {day :: String, hour :: Int, min :: Int, sec :: Int} deriving (Show,Read,Eq,Generic)
data ASCommit = ASCommit {commitUserId :: ASUserId, before :: [ASCell], after :: [ASCell], time :: ASTime} deriving (Show,Read,Eq,Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Convenience methods

lst :: ASValue -> [ASValue]
lst (ValueL l) = l
lst other = [other]

str :: ASValue -> String
str (ValueS s) = s

dbl :: ASValue -> Double
dbl (ValueD d) = d

failureMessage :: String -> ASMessage
failureMessage s = Message genericText NoAction (Failure s) (PayloadN ())

initialViewingWindow :: ASWindow
initialViewingWindow = Window "testSheetId" (0, 0) (100, 100)
-- TODO generate Unique sheet id

-- | When sending data from server to client, the server doesn't have a userId 
genericText :: Text
genericText = pack ""

openPermissions :: ASPermissions
openPermissions = Blacklist []

----------------------------------------------------------------------------------------------------------------------------------------------
-- | read/show compressed

class Show2 a where
  show2 :: a -> String

instance Show2 ASCell where
  show2 (Cell l e v ts) = (show2 l) ++ ('|':(show2 e)) ++ ('|':(show2 v)) ++ ('|':(show ts))

instance (Show2 ASLocation) where
  show2 (Index sid a) = "I/" ++ (unpack sid) ++ ('/':(show a))
  show2 (Range sid a) = "R/" ++ (unpack sid) ++ ('/':(show a))

instance (Show2 ASExpression) where
  show2 (Expression xp lang) = "E/" ++ xp ++ ('/':(show lang))
  show2 (Reference loc ref) = "R/" ++ (show2 loc) ++ ('/':(show ref))

instance (Show2 ASValue) where
  show2 = show -- TODO optimize

splitBy delimiter = foldr f [[]] 
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs

class Read2 a where
  read2 :: (Show2 a) => String -> a

instance (Read2 ASCell) where
  read2 str = Cell l xp v ts
    where
      [locstr, xpstr, valstr, tagstr] = splitBy '|' str
      l = read2 locstr :: ASLocation
      xp = read2 xpstr :: ASExpression
      v = read2 valstr :: ASValue
      ts = read tagstr :: [ASCellTag]

instance (Read2 ASLocation) 
  where
    read2 str = loc
      where
        [tag, sid, locstr] = splitBy '/' str
        loc = case tag of 
          "I" -> Index (pack sid) (read locstr :: (Int, Int))
          "R" -> Range (pack sid) (read locstr :: ((Int, Int), (Int, Int)))

instance (Read2 ASExpression)
  where
    read2 str = xp
      where
        [tag, xpstr, laststr] = splitBy '/' str
        xp = case tag of 
          "E" -> Expression xpstr (read laststr :: ASLanguage)
          "R" -> Reference (read2 xpstr :: ASLocation) (read laststr :: (Int, Int))

instance (Read2 ASValue)
  where 
    read2 = read -- TODO optimize

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Generic From/To JSON instances

instance ToJSON ASLocation
instance FromJSON ASLocation
instance ToJSON ASValue
instance FromJSON ASValue
instance ToJSON ASLanguage
instance FromJSON ASLanguage
instance ToJSON ASExpression
instance FromJSON ASExpression
instance ToJSON ASCell
instance FromJSON ASCell
instance ToJSON ASAction
instance FromJSON ASAction
instance ToJSON ASResult
instance FromJSON ASResult
instance ToJSON ASPayload
instance FromJSON ASPayload
instance ToJSON ASMessage
instance FromJSON ASMessage
instance ToJSON ASInitConnection
instance FromJSON ASInitConnection 
instance ToJSON ASExecError
instance FromJSON ASExecError
instance FromJSON ASTime
instance ToJSON ASTime
instance ToJSON ASCommit 
instance FromJSON ASCommit
instance ToJSON ASCellTag
instance FromJSON ASCellTag
instance ToJSON ASWindow
instance FromJSON ASWindow
instance ToJSON ASSheet
instance FromJSON ASSheet
instance ToJSON StreamSource
instance FromJSON StreamSource
instance ToJSON Stream
instance FromJSON Stream
instance ToJSON Bloomberg
instance FromJSON Bloomberg
instance FromJSON ASInitDaemonConnection
instance ToJSON ASInitDaemonConnection
instance FromJSON ASEntity
instance ToJSON ASEntity
instance FromJSON ASUserGroup
instance ToJSON ASUserGroup
instance FromJSON ASPermissions
instance ToJSON ASPermissions
instance FromJSON QueryList
instance ToJSON QueryList
instance FromJSON ASWorkbook
instance ToJSON ASWorkbook
instance FromJSON WorkbookSheet
instance ToJSON WorkbookSheet
instance FromJSON ASLangValue
instance ToJSON ASLangValue
