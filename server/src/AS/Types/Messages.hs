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
  | Update
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
  | SetCondFormatRules
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
  | PayloadCommit ASCommit
  | PayloadDelete ASRange [ASCell]
  | PayloadPaste {copyRange :: ASRange, copyTo :: ASRange}
  | PayloadProp {prop :: CellProp, tagRange :: ASRange}
  | PayloadXp ASExpression
  | PayloadOpen {initHeaderExpressions :: [ASExpression], initCondFormatRules :: [CondFormatRule], initBars :: [Bar]}
  | PayloadReplValue ASReplValue
  | PayloadValue CompositeValue
  | PayloadList QueryList
  | PayloadText {text :: String}
  | PayloadMutate MutateType
  | PayloadDrag {initialRange :: ASRange, dragRange :: ASRange}
  | PayloadCondFormat { condFormatRules :: [CondFormatRule] }
<<<<<<< HEAD
  | PayloadCondFormatResult { condFormatRulesResult :: [CondFormatRule], condFormatCellsUpdated :: [ASCell] }
  | PayloadSetBarProp BarIndex BarProp
=======
  | PayloadSetRowColProp RowColType Int RowColProp
>>>>>>> 5e30591... makeReplyMessageFromCells
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

-- | Not fully implemented yet
generateErrorMessage :: ASExecError -> String
generateErrorMessage e = case e of
  CircularDepError circDepLoc -> "Circular dependecy detected in cell " ++ (indexToExcel circDepLoc)
  DBNothingException locs     -> "Unable to fetch cells from database."
  ExpressionNotEvaluable      -> "Expression not does not contain evaluable statement."
  ExecError                   -> "Error while evaluating expression."
  SyntaxError                 -> "Syntax error."
  _                           -> show e


-- | Creates a server message from an ASExecError. Used in makeReplyMessageFromCells and  makeDeleteMessage.
makeErrorMessage :: ASExecError -> ASAction -> ASServerMessage
makeErrorMessage DecoupleAttempt a = ServerMessage a DecoupleDuringEval (PayloadN ())
makeErrorMessage e a = ServerMessage a (Failure (generateErrorMessage e)) (PayloadN ())

-- | When you have a list of cells from an eval request, this function constructs
-- the message to send back. 
makeReplyMessageFromCells :: ASAction -> [ASCell] -> ASServerMessage
makeReplyMessageFromCells action cells = ServerMessage action Success (PayloadSheetUpdate $ SheetUpdate cells [] [] [])

-- getBadLocs :: [ASReference] -> [Maybe ASCell] -> [ASReference]
-- getBadLocs locs mcells = map fst $ filter (\(l,c)->isNothing c) (zip locs mcells)

-- | Poorly named. When you have a list of cells from a get request, this function constructs
-- the message to send back.

-- ::ALEX:: no longer make it a matter of 
-- | Makes a delete message from an Update message and a list of locs to delete
makeDeleteMessage :: ASRange -> ASServerMessage -> ASServerMessage
makeDeleteMessage _ s@(ServerMessage _ (Failure _) _) = s
makeDeleteMessage _ s@(ServerMessage _ (DecoupleDuringEval) _) = ServerMessage Delete DecoupleDuringEval (PayloadN ())
makeDeleteMessage deleteLocs (ServerMessage _ _ (PayloadSheetUpdate (SheetUpdate cells _ _ _))) = ServerMessage Delete Success payload
  where locsCells = zip (map cellLocation cells) cells
        cells'    = map snd $ filter (\(l, _) -> not $ rangeContainsIndex deleteLocs l) locsCells
        payload   = PayloadDelete deleteLocs cells'
        -- remove the sels from the update that we know are blank from the deleted locs

-- handle failure cases like makeReplyMessageFromCells
-- otherwise: send payload CondFormatResult.
makeCondFormatMessage :: Either ASExecError [ASCell] -> [CondFormatRule] -> ASServerMessage
makeCondFormatMessage (Left err) _ = makeErrorMessage err SetCondFormatRules
makeCondFormatMessage (Right cells) rules = ServerMessage SetCondFormatRules Success payload
  where payload = PayloadSheetUpdate $ SheetUpdate cells [] [] rules

changeMessageAction :: ASAction -> ASServerMessage -> ASServerMessage
changeMessageAction a (ServerMessage _ r p) = ServerMessage a r p
