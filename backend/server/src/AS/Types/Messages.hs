{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module AS.Types.Messages where

import AS.Prelude
import Prelude()

import AS.Window
import AS.ASJSON

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
import AS.Types.Updates 

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.SafeCopy
import qualified Data.Text as T

type MessageId = T.Text

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
data ClientAction = 
    NoAction
  | AskDecouple
  | SetInitialProperties SheetUpdate [EvalHeader] -- list of expressions in header
  | ShowFailureMessage String
  | UpdateSheet SheetUpdate 
  | ClearSheet ASSheetId
  | MakeSelection Selection
  | AskUserToOpen ASSheetId
  | ShowHeaderResult EvalResult
  | PassBarToTest Bar
  | PassIsCoupledToTest Bool
  | PassCellsToTest [ASCell]
  deriving (Show, Read, Eq, Generic)

data ServerAction =
    Acknowledge
  | Initialize { connUserId :: ASUserId, connSheetId :: ASSheetId }
  | InitializeDaemon { parentUserId :: ASUserId, parentLoc :: ASIndex }
  | Open ASSheetId
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
  | MutateSheet MutateType
  | Drag { initialRange :: ASRange, dragRange :: ASRange }
  | Decouple
  | UpdateCondFormatRules CondFormatRuleUpdate
  | SetBarProp BarIndex BarProp
  | SetLanguagesInRange ASLanguage ASRange
  | ImportCSV { csvIndex :: ASIndex, csvLang :: ASLanguage, csvFileName :: String }
  | ChangeDecimalPrecision Int ASRange
  deriving (Show, Read, Eq, Generic)

-- for open, close dialogs
-- data QueryList =
--   Sheets |
--   Workbooks |
--   WorkbookSheets
--   deriving (Show, Read, Eq, Generic)

-- Indicates where to eval and what to eval
data EvalInstruction = EvalInstruction { evalXp :: ASExpression, evalLoc :: ASIndex } deriving (Show, Read, Eq, Generic)

data MutateType = InsertCol { insertColNum :: Int } | InsertRow { insertRowNum :: Int } |
                  DeleteCol { deleteColNum :: Int } | DeleteRow { deleteRowNum :: Int } |
                  DragCol { oldColNum :: Int, newColNum :: Int } | DragRow { oldRowNum :: Int, newRowNum :: Int }
                  deriving (Show, Read, Eq, Generic)

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
asToFromJSON ''MutateType
asToFromJSON ''EvalInstruction

instance ToJSON ClientMessage where
  toJSON (ClientMessage mid action) = object  [ "messageId" .= mid 
                                              , "clientAction" .= action ]

-- are legit.
asToJSON ''ClientAction

deriveSafeCopy 1 'base ''ServerMessage
deriveSafeCopy 1 'base ''ServerAction
deriveSafeCopy 1 'base ''MutateType
deriveSafeCopy 1 'base ''EvalInstruction

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
