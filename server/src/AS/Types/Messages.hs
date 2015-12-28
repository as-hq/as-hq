{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Messages where

import AS.Window

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
import AS.Types.CellProps
import AS.Types.CondFormat
import AS.Types.Updates 

import GHC.Generics
import Data.Aeson hiding (Success)
import Data.Aeson.Types (defaultOptions)
import Data.Serialize (Serialize)
import qualified Data.Text as T


data ASClientMessage = ClientMessage {
  clientAction :: ClientAction
} deriving (Show, Read, Generic)

-- ::ALEX:: swap names
data ASServerMessage = ServerMessage {
  serverAction :: ServerAction
} deriving (Show, Read, Generic)

-- maybe move to Util?
failureMessage :: String -> ASServerMessage
failureMessage s = ServerMessage $ ShowFailureMessage s

-- the constructors (i.e. the first words) are verbs telling client what action to take
data ServerAction = 
    NoAction
  | AskDecouple
  | SetInitialProperties SheetUpdate [ASExpression] -- list of expressions in header
  | ShowFailureMessage String
  | UpdateSheet SheetUpdate
  | ClearSheet ASSheetId
  | MakeSelection Selection
  | LoadImportedCells [ASCell] -- cells to add to sheet
  | ApplyCommit SheetUpdate -- an update, not a commit, is passed back
  | ShowHeaderResult CompositeValue
  deriving (Show, Read, Eq, Generic)

data ClientAction =
    Acknowledge
  | Initialize {connUserId :: ASUserId, connSheetId :: ASSheetId}
  | InitializeDaemon {parentUserId :: ASUserId, parentLoc :: ASIndex}
  | Open ASSheetId
  | UpdateWindow ASWindow
  -- | Import 
  -- | JumpSelect {jumpRange :: ASRange, jumpOrigin :: ASIndex, isShifted :: Bool, jumpDirection :: Direction}
  | Export ASSheetId
  | Evaluate ASExpression ASIndex
  | EvaluateHeader ASExpression
  | Get [ASIndex]
  | Delete Selection
  | ClearSheetServer ASSheetId
  | Undo 
  | Redo 
  | Copy ASRange ASRange
  | Cut  ASRange ASRange
  | ToggleProp CellProp ASRange
  | SetProp CellProp ASRange
  | Repeat Selection
  | BugReport String
  | MutateSheet MutateType
  | Drag {initialRange :: ASRange, dragRange :: ASRange}
  | Decouple
  | UpdateCondFormatRules CondFormatRuleUpdate
  | SetBarProp BarIndex BarProp
  | ImportCSV {csvIndex :: ASIndex, csvLang :: ASLanguage, csvFileName :: String}
  deriving (Show, Read, Eq, Generic)

-- for open, close dialogs
-- data QueryList =
--   Sheets |
--   Workbooks |
--   WorkbookSheets
--   deriving (Show, Read, Eq, Generic)

data MutateType = InsertCol { insertColNum :: Int } | InsertRow { insertRowNum :: Int } |
                  DeleteCol { deleteColNum :: Int } | DeleteRow { deleteRowNum :: Int } |
                  DragCol { oldColNum :: Int, newColNum :: Int } | DragRow { oldRowNum :: Int, newRowNum :: Int }
                  deriving (Show, Read, Eq, Generic)

-- should get renamed
-- data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Show, Read, Eq, Generic)


--------------------------------------------------------------------------------------------------------------
-- Instances

-- The format Frontend uses for both client->server and server->client is
-- { messageUserId: blah, action: blah, result: blah, payload: blah }
instance ToJSON ASClientMessage
instance FromJSON ASClientMessage

instance ToJSON ASServerMessage
instance FromJSON ASServerMessage

instance ToJSON ServerAction
instance FromJSON ServerAction

instance ToJSON ClientAction
instance FromJSON ClientAction

instance ToJSON MutateType
instance FromJSON MutateType

instance Serialize ASServerMessage
instance Serialize ASClientMessage
instance Serialize ServerAction
instance Serialize ClientAction
instance Serialize MutateType
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
makeErrorMessage :: ASExecError -> ASServerMessage
makeErrorMessage DecoupleAttempt = ServerMessage AskDecouple
makeErrorMessage e = ServerMessage $ ShowFailureMessage $ generateErrorMessage e

makeReplyMessageFromUpdate :: SheetUpdate -> ASServerMessage
makeReplyMessageFromUpdate update = ServerMessage $ UpdateSheet update

makeReplyMessageFromCommit :: ASCommit -> ASServerMessage
makeReplyMessageFromCommit = makeReplyMessageFromUpdate . sheetUpdateFromCommit

makeReplyMessageFromErrOrUpdate :: Either ASExecError SheetUpdate -> ASServerMessage
makeReplyMessageFromErrOrUpdate (Left err) = makeErrorMessage err
makeReplyMessageFromErrOrUpdate (Right update) = makeReplyMessageFromUpdate update

makeReplyMessageFromErrOrCommit :: Either ASExecError ASCommit -> ASServerMessage
makeReplyMessageFromErrOrCommit = makeReplyMessageFromErrOrUpdate . (fmap sheetUpdateFromCommit)

-- | Pass in a list of cells and an action, and this function constructs the message for updating those cells. 
makeReplyMessageFromCells :: [ASCell] -> ASServerMessage
makeReplyMessageFromCells cells = makeReplyMessageFromUpdate $ SheetUpdate (Update cells []) emptyUpdate emptyUpdate emptyUpdate