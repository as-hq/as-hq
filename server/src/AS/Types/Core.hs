{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Core where

import Prelude
import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Aeson.Types (defaultOptions)
import Data.Text hiding (foldr, map)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Network.WebSockets as WS
import qualified Database.Redis as R
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Concurrent (MVar)
import Control.Applicative
import Control.Monad (liftM, ap)

-- memory
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------------------------------
-- Sheets

type WorkbookName = String
type SheetName = String
type ASSheetId = Text

data ASSheet = Sheet {sheetId :: ASSheetId, sheetName :: SheetName, sheetPermissions :: ASPermissions} deriving (Show, Read, Eq, Generic)

-- should probably be a list of ASSheet's rather than ASSheetId's. 
data ASWorkbook = Workbook {workbookName :: WorkbookName, workbookSheets :: [ASSheetId]} deriving (Show, Read, Eq, Generic)

-- this type needs to be refactored away. It's used in a frontend API in basically exactly the
-- same way that ASWorkbook is supposed to be used. (Alex 11/3) 
data WorkbookSheet = WorkbookSheet {wsName :: WorkbookName, wsSheets :: [ASSheet]} deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Core cell types

type Col = Int
type Row = Int
type Coord = (Col, Row)
type Dimensions = (Int, Int)
type Offset = (Int, Int)

data ASIndex = Index {locSheetId :: ASSheetId, index :: Coord} deriving (Show, Read, Eq, Generic, Ord)
data ASRange = Range {rangeSheetId :: ASSheetId, range :: (Coord, Coord)} deriving (Show, Read, Eq, Generic, Ord)
data ASReference = IndexRef ASIndex | RangeRef ASRange | OutOfBounds deriving (Show, Read, Eq, Generic, Ord)

refSheetId :: ASReference -> ASSheetId
refSheetId loc = case loc of
  IndexRef i -> locSheetId i
  RangeRef r -> rangeSheetId r


-- | TODO: create custom show instance that takes REF/NA/VALUE etc into account
data EError =
  ExcelSyntaxError |
  EmptyMatrix String |
  NotFunction String |
  TooManyArgs String |
  ArrayConstantDim |
  CannotIntersectRefs |
  CannotScalarizeArrConst |
  EmptyArrayConstant |
  CannotNormalizeDimensions |
  ScalarizeIntersectionError ASReference ASReference |
  CannotConvertToExcelValue ASReference |
  InvalidArrayConstEntity |
  ArrayFormulaUnMappable | -- one of the arguments returned a reference or matrix, not a value
  NumArgs {fNameNA :: String, expectedNumArgs :: Int, actualNumArgs :: Int} |
  RequiredArgMissing {fNameRAM :: String, argNumRAM :: Int} |
  ArgType {fNameAT :: String, argNumAT :: Int, expectedType :: String, actualType :: String} |
  Default String |
  VAL String |
  REF String |
  NA String  |
  NUM String |
  DIV0 | 
  NegExpBaseWithFloatingExp | 
  ZeroToTheZero
  deriving (Show, Read, Eq, Ord, Generic)

data ASValue =
    NoValue
  | ValueNull
  | ValueS String
  | ValueI Int
  | ValueD Double
  | ValueB Bool
  | ValueL [ASValue]
  | ValueImage { imagePath :: String }
  | ValueObject { displayValue :: String, objectType :: String, jsonRepresentation :: String }
  | ValueError { errMsg :: String, errType :: String, file :: String, position :: Int }
  | ValueExcelError EError -- #needsrefactor: should be a part of ValueError
  | RList [(RListKey, ASValue)]
  | RDataFrame [ASValue]
  deriving (Show, Read, Eq, Generic)

type Percent = Double

type RListKey = String

type EvalCode = String

data ASReplValue = ReplValue {replValue :: ASValue, replLang :: ASLanguage} deriving (Show, Read, Eq, Generic)

data ASLanguage = R | Python | OCaml | CPP | Java | SQL | Excel deriving (Show, Read, Eq, Generic)

-- TODO consider migration to exLocs record
data ASExpression =
  Expression { expression :: String, language :: ASLanguage }
  deriving (Show, Read, Eq, Generic)


data ASCellTag =
    Color String
  | Size Int
  | Bold | Italic | Underline
  | StreamTag Stream
  | Tracking
  | Volatile
  | ReadOnly [ASUserId]
  | ListMember {listKey :: String}
  | Format FormatType
  | DFMember
  deriving (Show, Read, Eq, Generic)

data ASCell = Cell {cellLocation :: ASIndex,
          cellExpression :: ASExpression,
          cellValue :: ASValue,
          cellTags :: [ASCellTag]} deriving (Show, Read, Eq, Generic)

type ListKey = String
type ASList = (ListKey, [ASCell])
type Rect = (Coord, Coord)



data FormatType = NoFormat | Money | Percentage | Date deriving (Show, Read, Eq, Generic)
data Formatted a = Formatted { orig :: a, format :: Maybe FormatType }

instance Functor Formatted where
  fmap = liftM

instance Applicative Formatted where
  pure  = return
  (<*>) = ap

-- Always retain the format of the first argument, unless there was none
instance Monad Formatted where 
  return x                   = Formatted x Nothing
  Formatted x Nothing >>= f  = f x
  Formatted x y >>= f        = (f x) { format = y }

instance (Eq a) => Eq (Formatted a) where 
  (==) (Formatted x _) (Formatted y _)  = x==y

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
  | Evaluate | EvaluateRepl | EvaluateHeader
  | Update
  | Get | Delete
  | Copy | Cut | CopyForced
  | Undo | Redo
  | Clear
  | UpdateWindow
  | SetTag | ToggleTag
  | Repeat
  | BugReport
  | JumpSelect
  | MutateSheet
  | Drag
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
  | PayloadCL [ASCell]
  | PayloadLL [ASIndex]
  | PayloadR ASRange
  | PayloadS ASSheet
  | PayloadSelection {selectionRange :: ASRange, selectionOrigin :: ASIndex}
  | PayloadJump {jumpRange :: ASRange, jumpOrigin :: ASIndex, isShifted :: Bool, jumpDirection :: Direction}
  | PayloadSS [ASSheet]
  | PayloadWB ASWorkbook
  | PayloadWBS [ASWorkbook]
  | PayloadWorkbookSheets [WorkbookSheet]
  | PayloadW ASWindow
  | PayloadU ASUserId
  | PayloadE ASExecError
  | PayloadCommit ASCommit
  | PayloadDelete ASRange [ASCell]
  | PayloadPaste {copyRange :: ASRange, copyTo :: ASRange}
  | PayloadTag {cellTag :: ASCellTag, tagRange :: ASRange}
  | PayloadXp ASExpression
  | PayloadReplValue ASReplValue
  | PayloadValue ASValue
  | PayloadList QueryList
  | PayloadText {text :: String}
  | PayloadMutate MutateType
  | PayloadDrag {initialRange :: ASRange, dragRange :: ASRange}
  deriving (Show, Read, Eq, Generic)

data Direction = DUp | DDown | DLeft | DRight deriving (Show, Read, Eq, Generic)
data MutateType = InsertCol {insertColNum :: Int} | InsertRow { insertRowNum :: Int } |
                  DeleteCol { deleteColNum :: Int } | DeleteRow { deleteRowNum :: Int } |
                  DragCol { oldColNum :: Int, newColNum :: Int } | DragRow { oldRowNum :: Int, newRowNum :: Int } 
                  deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Version Control

data ASTime = Time {day :: String, hour :: Int, minute :: Int, sec :: Int} deriving (Show,Read,Eq,Generic)

type ASRelation = (ASIndex, [ASIndex]) -- for representing ancestry relationships

data ASCommit = ASCommit {before :: [ASCell],
                          after :: [ASCell],
                          time :: ASTime}
                          deriving (Show,Read,Eq,Generic)

type CommitSource = (ASSheetId, ASUserId)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval Types

data ASExecError =
    Timeout
  | EvaluationError {evalErrorDesc :: String}
  | DependenciesLocked {lockUserId :: ASUserId}
  | DBNothingException {badLocs :: [ASIndex]}
  | DBGraphUnreachable -- failed to connect
  | CircularDepError {badLoc :: ASIndex}
  | NetworkDown
  | RuntimeEvalException
  | ResourceLimitReached
  | InsufficientPermissions
  | NonUniqueIdentifier
  | ParseError
  | ExpressionNotEvaluable
  | ExecError
  | SyntaxError
  | HighDimensionalValue
  | APIError
  deriving (Show, Read, Eq, Generic)

type EitherCells = Either ASExecError [ASCell]
type EitherTExec = EitherT ASExecError IO

type IndValMap = M.Map ASIndex ASValue
type FormattedIndValMap = M.Map ASIndex (Formatted ASValue)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Websocket types

data ASInitConnection = ASInitConnection {connUserId :: ASUserId, connSheetId :: ASSheetId} deriving (Show,Read,Eq,Generic)
data ASInitDaemonConnection = ASInitDaemonConnection {parentUserId :: ASUserId, initDaemonLoc :: ASIndex} deriving (Show,Read,Eq,Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- State

data ServerState = State {userClients :: [ASUserClient], daemonClients :: [ASDaemonClient], dbConn :: R.Connection, appPort :: Port}

type Port = Int
----------------------------------------------------------------------------------------------------------------------------------------------
-- Clients

type ClientId = Text

class Client c where
  conn :: c -> WS.Connection
  clientId :: c -> ClientId
  clientSheetId :: c -> ASSheetId
  ownerName :: c -> ASUserId
  addClient :: c -> ServerState -> ServerState
  removeClient :: c -> ServerState -> ServerState
  clientCommitSource :: c -> CommitSource
  handleClientMessage :: c -> MVar ServerState -> ASClientMessage -> IO ()

data ASRecipients = Original | All | Custom [ASUserClient]

----------------------------------------------------------------------------------------------------------------------------------------------
-- Users

data ASWindow = Window {windowSheetId :: ASSheetId, topLeft :: Coord, bottomRight :: Coord} deriving (Show,Read,Eq,Generic)
type ASUserId = Text
data ASUserClient = UserClient {userId :: ASUserId, userConn :: WS.Connection, userWindow :: ASWindow, sessionId :: ClientId}

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

row :: Coord -> Int
row = snd

col :: Coord -> Int
col = snd

emptyExpression = ""

toList :: ASValue -> [ASValue]
toList (ValueL l) = l
toList other = [other]

str :: ASValue -> String
str (ValueS s) = s

dbl :: ASValue -> Double
dbl (ValueD d) = d

failureMessage :: String -> ASServerMessage
failureMessage s = ServerMessage NoAction (Failure s) (PayloadN ())

openPermissions :: ASPermissions
openPermissions = Blacklist []

isColocated :: ASCell -> ASCell -> Bool
isColocated c1 c2 = (cellLocation c1) == (cellLocation c2)

----------------------------------------------------------------------------------------------------------------------------------------------
-- JSON
instance FromJSON ASReference
-- instance ToJSON EvalErrorType
-- instance FromJSON EvalErrorType
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
--instance ToJSON ASPayload
instance FromJSON ASPayload
instance ToJSON ASInitConnection
instance FromJSON ASInitConnection
instance ToJSON ASExecError
instance FromJSON ASExecError
instance FromJSON EError
instance ToJSON EError
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
  toJSON (ServerMessage action result payload) = object ["action" .= action,
                                                        "result" .= result,
                                                        "payload" .= payload]

instance FromJSON ASServerMessage where
  parseJSON (Object v) = ServerMessage <$>
                           v .: "action" <*>
                           v .: "result" <*>
                           v .: "payload"
  parseJSON _          = fail "server message JSON attributes missing"

instance ToJSON ASPayload where
  toJSON (PayloadWorkbookSheets wbs) = object ["tag" .= ("PayloadWorkbookSheets" :: String),
                                               "contents" .= fields]
    where fields = object $ map (\wb -> (T.pack $ wsName wb) .= wb) wbs
  toJSON a = genericToJSON defaultOptions a

instance FromJSON ASCellTag
instance ToJSON ASCellTag

instance ToJSON ASIndex where
  toJSON (Index sid (c,r)) = object ["tag"     .= ("index" :: String),
                                     "sheetId" .= sid,
                                     "index"   .= object ["row" .= r, 
                                                          "col" .= c]]

instance FromJSON ASIndex where
  parseJSON (Object v) = do
    loc <- v .: "index"
    sid <- v .: "sheetId"
    idx <- (,) <$> loc .: "col" <*> loc .: "row"
    return $ Index sid idx
  parseJSON _          = fail "client message JSON attributes missing"

instance ToJSON ASRange where
  toJSON (Range sid ((c,r),(c2,r2))) = object ["tag" .= ("range" :: String),
                                               "sheetId" .= sid,
                                               "range" .= object [ 
                                                  "tl" .= object [ "row"  .= r, 
                                                                   "col"  .= c],
                                                  "br" .= object [ "row"  .= r2, 
                                                                   "col"  .= c2]]]
instance FromJSON ASRange where
  parseJSON (Object v) = do
    rng <- v .: "range" 
    (tl, br) <- (,) <$> rng .: "tl" <*> rng .: "br"
    tl' <- (,) <$> tl .: "col" <*> tl .: "row"
    br' <- (,) <$> br .: "col" <*> br .: "row"
    sid <- v .: "sheetId"
    return $ Range sid (tl', br')
  parseJSON _          = fail "client message JSON attributes missing"

instance ToJSON ASReference where
  toJSON (IndexRef idx) = toJSON idx
  toJSON (RangeRef rng) = toJSON rng

instance ToJSON ASWindow where
  toJSON (Window sid (c,r) (c2, r2)) = object ["tag" .= ("window" :: String),
                                               "sheetId" .= sid,
                                               "range" .= object [ 
                                                  "tl" .= object [ "row"  .= r, 
                                                                   "col"  .= c],
                                                  "br" .= object [ "row"  .= r2, 
                                                                   "col"  .= c2]]]
                                               

instance FromJSON ASWindow where
  parseJSON (Object v) = do
    rng <- v .: "window" 
    (tl, br) <- (,) <$> rng .: "tl" <*> rng .: "br"
    tl' <- (,) <$> tl .: "col" <*> tl .: "row"
    br' <- (,) <$> br .: "col" <*> br .: "row"
    sid <- v .: "sheetId"
    return $ Window sid tl' br'
  parseJSON _          = fail "client message JSON attributes missing"

instance ToJSON Direction 
instance FromJSON Direction

instance ToJSON MutateType
instance FromJSON MutateType

instance ToJSON FormatType
instance FromJSON FormatType

-- memory region exposure instances for R value unboxing
instance NFData ASValue       where rnf = genericRnf
instance NFData EError        where rnf = genericRnf
instance NFData ASReference   where rnf = genericRnf
instance NFData ASRange       where rnf = genericRnf
instance NFData ASIndex       where rnf = genericRnf
