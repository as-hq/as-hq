module AS.Types.Messages where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.SafeCopy
import qualified Data.Text as T

import AS.Prelude
import Prelude()
import AS.ASJSON
import AS.Types.Window
import AS.Types.Commits
import AS.Types.Selection
import AS.Types.Cell
import AS.Types.Bar
import AS.Types.BarProps
import AS.Types.Sheets
import AS.Types.Excel (indexToExcel)
import AS.Types.Locations
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.CellProps
import AS.Types.CondFormat
import AS.Types.Mutate hiding (Delete, Drag)
import AS.Types.Updates 
import AS.Types.User

type MessageId = T.Text
type SessionId = T.Text

data LoginMessage = Login AuthStrategy deriving (Show, Read, Generic)

data AuthStrategy = 
    GoogleAuth { idToken :: T.Text }
  | TestAuth
  deriving (Show, Read, Generic)

data ServerMessage = ServerMessage {
    serverMessageId :: MessageId
  , serverAction :: ServerAction
} deriving (Show, Read, Generic)

data ClientMessage = ClientMessage {
    clientMessageId :: MessageId
  , clientAction :: ClientAction
} deriving (Show, Read, Generic)

-- maybe move to Util?
failureMessage :: MessageId -> String -> ClientMessage
failureMessage mid s = ClientMessage mid $ ShowFailureMessage s

-- the constructors (i.e. the first words) are verbs telling client what action to take
-- (anand 2/05) I think this is a bad design principle. Not all messages exchanged with 
-- the client are verbs, such as authentication status.
data ClientAction = 
    NoAction
  | AskDecouple
  | AskTimeout { timeoutMessageId :: MessageId, serverActionType :: String }
  | AskOpenSheet ASSheetId
  | SetSheetData { updateSheetId :: ASSheetId, update :: SheetUpdate, headers :: [EvalHeader] } -- list of expressions in header
  | SetMySheets { mySheets :: [ASSheet], sharedSheets :: [ASSheet] }
  | ShowFailureMessage String
  | UpdateSheet SheetUpdate 
  | ClearSheet ASSheetId
  | MakeSelection Selection
  | HandleEvaluatedHeader { headerContents :: EvalHeader, headerResult :: HeaderResult, headerEvaluator :: ASUserId }
  | PassBarToTest Bar
  | PassIsCoupledToTest Bool
  | PassCellsToTest [ASCell]
  | AuthFailure { failureReason :: String }
  | AuthSuccess { authUserId :: ASUserId, defaultSheetId :: ASSheetId }
  | SessionLog { sessionLog :: [LogData] }
  | AllSessions { allSessions :: [SessionData] }
  deriving (Show, Read, Eq, Generic)

data ServerAction =
    InitializeDaemon { parentUserId :: ASUserId, parentLoc :: ASIndex }
  | OpenSheet ASSheetId
  | NewSheet SheetName
  -- currently used for sharing sheets.
  | AcquireSheet ASSheetId 
  | GetMySheets
  | UpdateWindow ASWindow
  -- | Import 
  -- | JumpSelect {jumpRange :: ASRange, jumpOrigin :: ASIndex, isShifted :: Bool, jumpDirection :: Direction}
  | Export ASSheetId
  | Evaluate [EvalInstruction]
  | EvaluateHeader EvalHeader
  | Get [ASIndex]
  | GetBar BarIndex
  | GetIsCoupled ASIndex
  | Delete ASRange
  | ClearSheetServer ASSheetId
  | Undo 
  | Redo 
  | Copy { copyFrom :: ASRange, copyTo :: ASRange }
  | Cut  { cutFrom :: ASRange, cutTo :: ASRange }
  | ToggleProp CellProp ASRange
  | SetProp CellProp ASRange
  | Repeat Selection
  | BugReport String
  | MutateSheet Mutate
  | Drag { initialRange :: ASRange, dragRange :: ASRange }
  | Decouple
  | Timeout MessageId
  | UpdateCondFormatRules { newRules :: [CondFormatRule], oldRuleIds :: [CondFormatRuleId] }
  | SetBarProp BarIndex BarProp
  | SetLanguagesInRange ASLanguage ASRange
  | ImportCSV { csvIndex :: ASIndex, csvLang :: ASLanguage, csvFileName :: String }
  | ImportExcel { excelSheetId :: ASSheetId, excelFileName :: String}
  | ChangeDecimalPrecision Int ASRange
  | LogAction String
  | GetSessionLogs LogSource
  | StartDebuggingLog
  | GetAllSessions
  | RenameSheet { renameSheetId :: ASSheetId, newSheetName :: SheetName }
  deriving (Show, Read, Eq, Data, Typeable, Generic)

-- for open, close dialogs
-- data QueryList =
--   Sheets |
--   Workbooks |
--   WorkbookSheets
--   deriving (Show, Read, Eq, Generic)

-- This type represents log data, which is either a frontend action (isAction = true, logMsg = JSON.stringify(action)), 
-- or a message send to backend (isAction = False)
-- #needsrefactor make this an enum FrontendAction | BackendMessage instead of bool
data LogData = LogData {logMsg :: String, isAction :: Bool}
   deriving (Show, Read, Eq, Generic)
-- These are the keys of logs in the DB (user and session info)
data LogSource = LogSource {logUserId :: ASUserId, logSessionId :: SessionId}
   deriving (Show, Read, Eq, Generic, Data)
-- Data for a session
data SessionData = SessionData {seshUserId :: ASUserId, seshId :: SessionId, seshTime :: String} 
  deriving (Show, Read, Eq, Generic, Data)


-- Indicates where to eval and what to eval
data EvalInstruction = EvalInstruction { evalXp :: ASExpression, evalLoc :: ASIndex } 
  deriving (Show, Read, Eq, Data, Typeable, Generic)

-- should get renamed
-- data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Show, Read, Eq, Generic)


--------------------------------------------------------------------------------------------------------------
-- Instances

instance FromJSON ServerMessage where
  parseJSON (Object v) = ServerMessage <$> v .: "messageId" <*> v .: "serverAction"
  parseJSON _ = $error "expected JSON to be object type"

-- #anand
-- Daemons require a ToJSON instance, as they need to be able to generate ServerMessages.
-- the reason being that daemons are currently implemented as actual WebSocket clients.
-- I think daemons could be implemented as async threads which directly hook into our handler
-- functions; there's no need for them to *actually* create JSON-ified server messages.
instance ToJSON ServerMessage where
  toJSON (ServerMessage mid action) = object  [ "messageId" .= mid 
                                              , "serverAction" .= action ]

asToFromJSON ''ServerAction
asToFromJSON ''EvalInstruction

instance ToJSON ClientMessage where
  toJSON (ClientMessage mid action) = object  [ "messageId" .= mid 
                                              , "clientAction" .= action ]

-- are legit.
asToJSON ''ClientAction
asToJSON ''LogData
asToJSON ''LogSource
asToJSON ''SessionData

asFromJSON ''LoginMessage
asFromJSON ''AuthStrategy
asFromJSON ''LogSource


deriveSafeCopy 1 'base ''ServerMessage
deriveSafeCopy 1 'base ''ServerAction
deriveSafeCopy 1 'base ''EvalInstruction
deriveSafeCopy 1 'base ''LogSource
deriveSafeCopy 1 'base ''LogData

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
  TooFarBack                  -> "Too far back"
  TooFarForwards              -> "Too far forwards"
  _                           -> show e


-- | Creates a server message from an ASExecError. Used in makeReplyMessageFromCells and  makeDeleteMessage.
-- #needsrefactor when makeCondFormatMessage goes, this can probably go too
makeErrorMessage :: MessageId -> ASExecError -> ClientMessage
makeErrorMessage mid DecoupleAttempt = ClientMessage mid AskDecouple 
makeErrorMessage mid e = ClientMessage mid $ ShowFailureMessage $ generateErrorMessage e

-- I only need a string here, because the ActionType is never used to construct anything -
-- only to insert a field into one of the JSON messages (AskTimeout)
-- For example, this function takes
--        ToggleProp _ _ -> "ToggleProp"
--        Decouple       -> "Decouple"
getServerActionType :: ServerAction -> String
getServerActionType = show . toConstr
