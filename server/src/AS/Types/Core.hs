{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Core where

import Prelude
import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Aeson.Types (defaultOptions)
import Data.Text hiding (foldr, map)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BC
import qualified Network.WebSockets as WS
import qualified Database.Redis as R
import Control.Concurrent (MVar)
import Control.Applicative

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------------------------------
-- Sheets

type ASSheetId = Text
data ASSheet = Sheet {sheetId :: ASSheetId, sheetName :: String, sheetPermissions :: ASPermissions} deriving (Show, Read, Eq, Generic)
data ASWorkbook = Workbook {workbookName :: String, workbookSheets :: [ASSheetId]} deriving (Show, Read, Eq, Generic)

data WorkbookSheet = WorkbookSheet {wsName :: String, wsSheets :: [ASSheet]} deriving (Show, Read, Eq, Generic)
----------------------------------------------------------------------------------------------------------------------------------------------
-- Core cell types

data ASIndex = Index {locSheetId :: ASSheetId, index :: (Int, Int)} deriving (Show, Read, Eq, Generic, Ord)
data ASRange = Range {rangeSheetId :: ASSheetId, range :: ((Int, Int), (Int, Int))} deriving (Show, Read, Eq, Generic, Ord)
data ASReference = IndexRef ASIndex | RangeRef ASRange deriving (Show, Read, Eq, Generic, Ord)

refSheetId :: ASReference -> ASSheetId
refSheetId loc = case loc of 
  IndexRef i -> locSheetId i 
  RangeRef r -> rangeSheetId r

data ASValue =
    NoValue
  | ValueS String
  | ValueI Int
  | ValueD Double 
  | ValueB Bool
  | ValueL [ASValue]
  | ValueT (ASValue, ASValue)
  | ValueImage { imagePath :: String}
  | ValueObject { objectType :: String, jsonRepresentation :: String }
  | ValueStyled { style :: String, value :: ASValue }
  | ValueError { error :: String, err_type :: String, file :: String, position :: Int } 
  | ValueE ASEvalError
  | ExcelSheet { locs :: ASValue, exprs :: ASValue, vals :: ASValue}
  | Rickshaw {rickshawData :: ASValue}
  | StockChart { stockPrices :: ASValue, stockName :: String }
  | DisplayValue { displayValue :: String, actualValue :: ASValue }
  deriving (Show, Read, Eq, Generic)

data ASReplValue = ReplValue {replValue :: ASValue, replLang :: ASLanguage} deriving (Show, Read, Eq, Generic)

type ASEvalError = String

data ASLanguage = R | Python | OCaml | CPP | Java | SQL | Excel deriving (Show, Read, Eq, Generic)

-- TODO consider migration to exLocs record
data ASExpression =
  Expression { expression :: String, language :: ASLanguage } | 
  Reference { location :: ASReference, referenceIndex :: (Int, Int) }
  deriving (Show, Read, Eq, Generic)

emptyExpression = ""

data ASCellTag = 
    Color String
  | Size Int
  | Money
  | Percentage
  | StreamTag Stream
  | Tracking
  | Volatile
  | ReadOnly [ASUserId]
  | ListMember {listKey :: String}
  deriving (Show, Read, Eq, Generic)

data ASCell = Cell {cellLocation :: ASIndex, 
					cellExpression :: ASExpression,
					cellValue :: ASValue,
          cellTags :: [ASCellTag]} deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Streaming

-- Stream sources
data Bloomberg = Bloomberg {url :: String, key :: String} deriving (Show, Read, Eq, Generic)
data StreamSource = StreamB Bloomberg | NoSource deriving (Show, Read, Eq, Generic)

-- A stream just needs a source and a frequency
data Stream = Stream {streamSource :: StreamSource, streamFreq :: Int} deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Message Types

data ASClientMessage = ClientMessage {
  clientAction :: ASAction,
  clientPayload :: ASPayload
} deriving (Show, Read, Eq, Generic)

data ASServerMessage = ServerMessage { 
  serverAction :: ASAction,
  serverResult :: ASResult,
  serverPayload :: ASPayload
} deriving (Show, Read, Eq, Generic)

data ASAction = 
    NoAction
  | Acknowledge
  | SetInitialSheet
  | New | Import 
  | Open | Close
  | Evaluate | EvaluateRepl
  | Update
  | Get | Delete
  | Copy | CopyForced
  | Undo | Redo
  | Clear
  | UpdateWindow
  | AddTags | RemoveTags
  deriving (Show, Read, Eq, Generic)

data ASResult = Success | Failure {failDesc :: String} | NoResult deriving (Show, Read, Eq, Generic)

-- for open, close dialogs
data QueryList = 
  Sheets |
  Workbooks |
  WorkbookSheets
  deriving (Show, Read, Eq, Generic)

data ASPayload = 
    PayloadN ()
  | PayloadInit ASInitConnection
  | PayloadDaemonInit ASInitDaemonConnection
  | PayloadC ASCell
  | PayloadCL [ASCell]
  | PayloadL ASIndex
  | PayloadLL [ASIndex]
  | PayloadR ASRange
  | PayloadS ASSheet
  | PayloadSS [ASSheet]
  | PayloadWB ASWorkbook
  | PayloadWBS [ASWorkbook]
  | PayloadWorkbookSheets [WorkbookSheet]
  | PayloadW ASWindow
  | PayloadU ASUserId
  | PayloadE ASExecError
  | PayloadCommit ASCommit
  | PayloadCopy {copyRange :: ASRange, copyTo :: ASIndex}
  | PayloadTags {tags :: [ASCellTag], tagsLoc :: ASIndex}
  | PayloadXp ASExpression
  | PayloadReplValue ASReplValue
  | PayloadList QueryList 
  deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Version Control

data ASTime = Time {day :: String, hour :: Int, min :: Int, sec :: Int} deriving (Show,Read,Eq,Generic)
data ASCommit = ASCommit {commitUserId :: ASUserId, before :: [ASCell], after :: [ASCell], time :: ASTime} deriving (Show,Read,Eq,Generic)


----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval Types

data ASExecError = 
    Timeout
  | EvaluationError {evalErrorDesc :: String}
  | DependenciesLocked {lockUserId :: ASUserId} 
  | DBNothingException {badLocs :: [ASIndex]}
  | DBGraphUnreachable 
  | NetworkDown
  | ResourceLimitReached
  | InsufficientPermissions
  | NonUniqueIdentifier
  | CopyNonexistentDependencies
  | ParseError
  | ExpressionNotEvaluable
  | ExecError
  | ExcelSyntaxError {excelErr :: String}
  | SyntaxError
  deriving (Show, Read, Eq, Generic)

type EitherCells = Either ASExecError [ASCell] 
type EitherTExec = EitherT ASExecError IO
 
----------------------------------------------------------------------------------------------------------------------------------------------
-- Websocket types

data ASInitConnection = ASInitConnection {connUserId :: ASUserId} deriving (Show,Read,Eq,Generic)
data ASInitDaemonConnection = ASInitDaemonConnection {parentUserId :: ASUserId, initDaemonLoc :: ASIndex} deriving (Show,Read,Eq,Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- State

data ServerState = State {userClients :: [ASUserClient], daemonClients :: [ASDaemonClient], dbConn :: R.Connection} 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Clients

type ClientId = Text

class Client c where
  conn :: c -> WS.Connection
  clientId :: c -> ClientId
  ownerName :: c -> ASUserId
  addClient :: c -> ServerState -> ServerState
  removeClient :: c -> ServerState -> ServerState
  handleClientMessage :: c -> MVar ServerState -> ASClientMessage -> IO ()

data ASRecipients = Original | All | Custom [ASUserClient]

----------------------------------------------------------------------------------------------------------------------------------------------
-- Users

data ASWindow = Window {windowSheetId :: ASSheetId, topLeft :: (Int, Int), bottomRight :: (Int, Int)} deriving (Show,Read,Eq,Generic)
type ASUserId = Text 
data ASUserClient = UserClient {userId :: ASUserId, userConn :: WS.Connection, windows :: [ASWindow], sessionId :: ClientId} 

instance Eq ASUserClient where 
  c1 == c2 = (sessionId c1) == (sessionId c2)

data ASUserGroup = Group {groupMembers :: [ASUserId], groupAdmins :: [ASUserId], groupName :: Text} deriving (Show, Read, Eq, Generic)
data ASEntity = EntityGroup ASUserGroup|
                EntityUser ASUserId
                deriving (Show, Read, Eq, Generic)

data ASPermissions = Blacklist [ASEntity] |
                     Whitelist [ASEntity]
                      deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Daemons

data ASDaemonClient = DaemonClient {daemonLoc :: ASIndex, daemonConn :: WS.Connection, daemonOwner :: ASUserId}

instance Eq ASDaemonClient where 
  c1 == c2 = (daemonLoc c1) == (daemonLoc c2)


----------------------------------------------------------------------------------------------------------------------------------------------
-- Convenience methods

toList :: ASValue -> [ASValue]
toList (ValueL l) = l
toList other = [other]

str :: ASValue -> String
str (ValueS s) = s

dbl :: ASValue -> Double
dbl (ValueD d) = d

failureMessage :: String -> ASServerMessage
failureMessage s = ServerMessage NoAction (Failure s) (PayloadN ())

initialViewingWindow :: ASWindow
initialViewingWindow = Window "testSheetId" (0, 0) (100, 100)
-- TODO generate Unique sheet id

openPermissions :: ASPermissions
openPermissions = Blacklist []


----------------------------------------------------------------------------------------------------------------------------------------------
-- JSON
instance ToJSON ASReference
instance FromJSON ASReference
instance ToJSON ASIndex
instance FromJSON ASIndex
instance ToJSON ASRange
instance FromJSON ASRange
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
instance ToJSON ASInitConnection
instance FromJSON ASInitConnection 
instance ToJSON ASExecError
instance FromJSON ASExecError
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
instance FromJSON ASReplValue
instance ToJSON ASReplValue
instance FromJSON ASTime
instance ToJSON ASTime
instance FromJSON ASCommit
instance ToJSON ASCommit

-- The format Frontend uses for both client->server and server->client is 
-- { messageUserId: blah, action: blah, result: blah, payload: blah }
instance ToJSON ASClientMessage where 
  toJSON (ClientMessage action payload) = object ["action" .= action, "payload" .= payload]
instance FromJSON ASClientMessage where 
  parseJSON (Object v) = ClientMessage <$>
                           v .: "action" <*>
                           v .: "payload"
  parseJSON _          = fail "client message JSON attributes missing"
instance ToJSON ASServerMessage where 
  toJSON (ServerMessage action result payload) = object ["action" .= action, "result" .= result, "payload" .= payload]
instance FromJSON ASServerMessage where 
  parseJSON (Object v) = ServerMessage <$>
                           v .: "action" <*>
                           v .: "result" <*>
                           v .: "payload"
  parseJSON _          = fail "server message JSON attributes missing"

instance FromJSON ASCellTag
instance ToJSON ASCellTag
--instance ToJSON ASCellTag where
--  toJSON (ListMember k) = object ["listKey" .= (BC.unpack k)]
--  toJSON a = genericToJSON defaultOptions a
--instance FromJSON ASCellTag where
--  parseJSON obj@(Object v) = do
--    listField <- v .:? "listKey"
--    case listField of 
--      Nothing -> genericParseJSON defaultOptions obj
--      (Just k) -> return . ListMember $ BC.pack k