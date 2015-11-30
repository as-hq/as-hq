{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Messages where

import AS.Window

import AS.Types.DB (ASCommit)
import AS.Types.Cell
import AS.Types.Sheets
import AS.Types.Excel (indexToExcel)

import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Aeson.Types (defaultOptions) 
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
  | New | Import
  | Open | Close
  | Evaluate | EvaluateRepl | EvaluateHeader
  | Update
  | Get | Delete
  | Copy | Cut | CopyForced
  | Undo | Redo
  | Clear
  | UpdateWindow
  | SetProp | ToggleProp
  | Repeat
  | BugReport
  | JumpSelect
  | MutateSheet
  | Drag
  | SetCondFormatRules
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
  | PayloadCommit ASCommit
  | PayloadDelete ASRange [ASCell]
  | PayloadPaste {copyRange :: ASRange, copyTo :: ASRange}
  | PayloadProp {prop :: CellProp, tagRange :: ASRange}
  | PayloadXp ASExpression
  | PayloadOpen {initHeaderExpressions :: [ASExpression], initCondFormatRules :: [CondFormatRule]}
  | PayloadReplValue ASReplValue
  | PayloadValue CompositeValue
  | PayloadList QueryList
  | PayloadText {text :: String}
  | PayloadMutate MutateType
  | PayloadDrag {initialRange :: ASRange, dragRange :: ASRange}
  | PayloadCondFormat { condFormatRules :: [CondFormatRule] }
  | PayloadCondFormatResult { condFormatRulesResult :: [CondFormatRule], condFormatCellsUpdated :: [ASCell] }
  deriving (Show, Read, Generic)

data ASReplValue = ReplValue {replValue :: ASValue, replLang :: ASLanguage} deriving (Show, Read, Eq, Generic)

data ASInitConnection = ASInitConnection {connUserId :: ASUserId, connSheetId :: ASSheetId} deriving (Show,Read,Eq,Generic)

data ASInitDaemonConnection = ASInitDaemonConnection {parentUserId :: ASUserId, initDaemonLoc :: ASIndex} deriving (Show,Read,Eq,Generic)

data MutateType = InsertCol { insertColNum :: Int } | InsertRow { insertRowNum :: Int } |
                  DeleteCol { deleteColNum :: Int } | DeleteRow { deleteRowNum :: Int } |
                  DragCol { oldColNum :: Int, newColNum :: Int } | DragRow { oldRowNum :: Int, newRowNum :: Int } 
                  deriving (Show, Read, Eq, Generic)

data CondFormatRule = CondFormatRule { cellLocs :: [ASRange], 
                                       condition :: ASExpression, 
                                       condFormat :: CellProp } deriving (Show, Read, Generic, Eq)

-- should get renamed
data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Show, Read, Eq, Generic)




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

instance ToJSON CondFormatRule
instance FromJSON CondFormatRule

instance ToJSON Direction 
instance FromJSON Direction

instance FromJSON ASReplValue
instance ToJSON ASReplValue

instance FromJSON QueryList
instance ToJSON QueryList


--------------------------------------------------------------------------------------------------------------
-- Helpers

-- | Not fully implemented yet
generateErrorMessage :: ASExecError -> String
generateErrorMessage e = case e of
  CircularDepError circDepLoc -> "Circular dependecy detected in cell " ++ (indexToExcel circDepLoc)
  (DBNothingException _)      -> "Unable to fetch cells from database."
  ExpressionNotEvaluable      -> "Expression not does not contain evaluable statement."
  ExecError                   -> "Error while evaluating expression."
  SyntaxError                 -> "Syntax error."
  _                           -> show e


-- | When you have a list of cells from an eval request, this function constructs
-- the message to send back. 
makeUpdateMessage :: Either ASExecError [ASCell] -> ASServerMessage
makeUpdateMessage (Left e) = ServerMessage Update (Failure (generateErrorMessage e)) (PayloadN ())
makeUpdateMessage (Right cells) = ServerMessage Update Success (PayloadCL cells)

-- getBadLocs :: [ASReference] -> [Maybe ASCell] -> [ASReference]
-- getBadLocs locs mcells = map fst $ filter (\(l,c)->isNothing c) (zip locs mcells)

-- | Poorly named. When you have a list of cells from a get request, this function constructs
-- the message to send back. 
makeGetMessage :: [ASCell] -> ASServerMessage
makeGetMessage cells = changeMessageAction Get $ makeUpdateMessage (Right cells)

makeUpdateWindowMessage :: [ASCell] -> ASServerMessage
makeUpdateWindowMessage cells = changeMessageAction UpdateWindow $ makeUpdateMessage (Right cells)

-- | Makes a delete message from an Update message and a list of locs to delete
makeDeleteMessage :: ASRange -> ASServerMessage -> ASServerMessage
makeDeleteMessage _ s@(ServerMessage _ (Failure _) _) = s
makeDeleteMessage deleteLocs s@(ServerMessage _ _ (PayloadCL cells)) = ServerMessage Delete Success payload
  where locsCells = zip (map cellLocation cells) cells
        cells'    = map snd $ filter (\(l, _) -> not $ rangeContainsIndex deleteLocs l) locsCells
        payload   = PayloadDelete deleteLocs cells'
        -- remove the sels from the update that we know are blank from the deleted locs

makeCondFormatMessage :: [CondFormatRule] -> ASServerMessage -> ASServerMessage
makeCondFormatMessage _ s@(ServerMessage _ (Failure _) _) = s
makeCondFormatMessage rules s@(ServerMessage _ _ (PayloadCL cells)) = ServerMessage SetCondFormatRules Success payload
  where payload = PayloadCondFormatResult rules cells

changeMessageAction :: ASAction -> ASServerMessage -> ASServerMessage
changeMessageAction a (ServerMessage _ r p) = ServerMessage a r p