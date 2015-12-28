{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Messages where

import AS.Window

import AS.Types.Commits
import AS.Types.Cell
import AS.Types.Bar
import AS.Types.BarProps
import AS.Types.Sheets
import AS.Types.Excel (indexToExcel)
import AS.Types.Locations
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.CellProps
import AS.Types.CondFormat
import AS.Types.Updates 

import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Aeson.Types (defaultOptions)
import Data.Serialize (Serialize)
import qualified Data.Text as T


data ASClientMessage = ClientMessage {
  clientAction :: ASAction,
  clientPayload :: ASPayload
} deriving (Show, Read, Generic)

data ASServerMessage = ServerMessage {
  serverAction :: ASAction,
  serverResult :: ASResult,
  serverPayload :: ASPayload
} deriving (Show, Read, Generic)

-- maybe move to Util?
failureMessage :: String -> ASServerMessage
failureMessage s = ServerMessage NoAction (Failure s) (PayloadN ())

data ASAction =
    NoAction
  | Acknowledge
  | SetInitialSheet
  | New
  | Import | Export | ImportCSV
  | Open | Close
  | Evaluate | EvaluateRepl | EvaluateHeader
  | UpdateSheet
  | Get | Delete
  | Copy | Cut | CopyForced
  | Undo | Redo
  | Clear
  | UpdateWindow
  | SetProp | ToggleProp
  | SetBarProp
  | Repeat
  | BugReport
  | JumpSelect
  | MutateSheet
  | Drag
  | UpdateCondFormatRules
  | Decouple
  deriving (Show, Read, Eq, Generic)

data ASResult = Success | Failure {failDesc :: String} | NoResult | DecoupleDuringEval deriving (Show, Read, Eq, Generic)

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
  | PayloadPaste {copyRange :: ASRange, copyTo :: ASRange}
  | PayloadProp {prop :: CellProp, tagRange :: ASRange}
  | PayloadXp ASExpression
  | PayloadOpen {initHeaderExpressions :: [ASExpression], initSheetUpdate :: SheetUpdate}
  | PayloadValue CompositeValue ASLanguage
  | PayloadList QueryList
  | PayloadText {text :: String}
  | PayloadMutate MutateType
  | PayloadDrag {initialRange :: ASRange, dragRange :: ASRange}
  | PayloadCondFormatUpdate CondFormatRuleUpdate
  | PayloadSetBarProp BarIndex BarProp
  | PayloadCSV {csvIndex :: ASIndex, csvLang :: ASLanguage, csvFileName :: String}
  | PayloadSheetUpdate SheetUpdate
  deriving (Show, Read, Generic)

data ASReplValue = ReplValue {replValue :: ASValue, replLang :: ASLanguage} deriving (Show, Read, Eq, Generic)

data ASInitConnection = ASInitConnection {connUserId :: ASUserId, connSheetId :: ASSheetId} deriving (Show,Read,Eq,Generic)

data ASInitDaemonConnection = ASInitDaemonConnection {parentUserId :: ASUserId, initDaemonLoc :: ASIndex} deriving (Show,Read,Eq,Generic)

data MutateType = InsertCol { insertColNum :: Int } | InsertRow { insertRowNum :: Int } |
                  DeleteCol { deleteColNum :: Int } | DeleteRow { deleteRowNum :: Int } |
                  DragCol { oldColNum :: Int, newColNum :: Int } | DragRow { oldRowNum :: Int, newRowNum :: Int }
                  deriving (Show, Read, Eq, Generic)

-- should get renamed
data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Show, Read, Eq, Generic)


--------------------------------------------------------------------------------------------------------------
-- Instances

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
instance FromJSON ASPayload

instance ToJSON ASResult
instance FromJSON ASResult

instance ToJSON ASAction
instance FromJSON ASAction

instance ToJSON ASInitConnection
instance FromJSON ASInitConnection

instance FromJSON ASInitDaemonConnection
instance ToJSON ASInitDaemonConnection

instance ToJSON MutateType
instance FromJSON MutateType

instance ToJSON Direction
instance FromJSON Direction

instance FromJSON ASReplValue
instance ToJSON ASReplValue

instance FromJSON QueryList
instance ToJSON QueryList

instance Serialize ASClientMessage
instance Serialize ASPayload
instance Serialize ASAction
instance Serialize WorkbookSheet
instance Serialize ASSheet
instance Serialize ASWorkbook
instance Serialize QueryList
instance Serialize MutateType
instance Serialize Direction
instance Serialize ASReplValue
instance Serialize ASInitConnection
instance Serialize ASInitDaemonConnection
-- are legit.
--------------------------------------------------------------------------------------------------------------
-- Helpers


-- #incomplete #needsrefactor incorrectly located
generateErrorMessage :: ASExecError -> String
generateErrorMessage e = case e of
  CircularDepError circDepLoc -> "Circular dependecy detected in cell " ++ (indexToExcel circDepLoc)
  DBNothingException locs     -> "Unable to fetch cells from database."
  ExpressionNotEvaluable      -> "Expression not does not contain evaluable statement."
  ExecError                   -> "Error while evaluating expression."
  SyntaxError                 -> "Syntax error."
  _                           -> show e


-- | Creates a server message from an ASExecError. Used in makeReplyMessageFromCells and  makeDeleteMessage.
-- #needsrefactor when makeCondFormatMessage goes, this can probably go too
makeErrorMessage :: ASExecError -> ASAction -> ASServerMessage
makeErrorMessage DecoupleAttempt a = ServerMessage a DecoupleDuringEval (PayloadN ())
makeErrorMessage e a = ServerMessage a (Failure (generateErrorMessage e)) (PayloadN ())

makeReplyMessageFromUpdate :: SheetUpdate -> ASServerMessage
makeReplyMessageFromUpdate update = ServerMessage UpdateSheet Success $ PayloadSheetUpdate update

makeReplyMessageFromCommit :: ASCommit -> ASServerMessage
makeReplyMessageFromCommit = makeReplyMessageFromUpdate . sheetUpdateFromCommit

makeReplyMessageFromErrOrUpdate :: Either ASExecError SheetUpdate -> ASServerMessage
makeReplyMessageFromErrOrUpdate (Left err) = makeErrorMessage err UpdateSheet
makeReplyMessageFromErrOrUpdate (Right update) = makeReplyMessageFromUpdate update

makeReplyMessageFromErrOrCommit :: Either ASExecError ASCommit -> ASServerMessage
makeReplyMessageFromErrOrCommit = makeReplyMessageFromErrOrUpdate . (fmap sheetUpdateFromCommit)

-- | Pass in a list of cells and an action, and this function constructs the message for updating those cells. 
makeReplyMessageFromCells :: ASAction -> [ASCell] -> ASServerMessage
makeReplyMessageFromCells action cells = ServerMessage action Success $ PayloadSheetUpdate $ SheetUpdate (Update cells []) emptyUpdate emptyUpdate emptyUpdate