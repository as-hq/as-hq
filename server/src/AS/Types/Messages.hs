{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module AS.Types.Messages where

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
import AS.Types.CellProps
import AS.Types.CondFormat
import AS.Types.Updates 

import GHC.Generics
import Data.Aeson.Types (defaultOptions)
import qualified Data.Text as T


data ServerMessage = ServerMessage {
  serverAction :: ServerAction
} deriving (Show, Read, Generic)

data ClientMessage = ClientMessage {
  clientAction :: ClientAction
} deriving (Show, Read, Generic)

-- maybe move to Util?
failureMessage :: String -> ClientMessage
failureMessage s = ClientMessage $ ShowFailureMessage s

-- the constructors (i.e. the first words) are verbs telling client what action to take
data ClientAction = 
    NoAction
  | AskDecouple
  | SetInitialProperties SheetUpdate [ASExpression] -- list of expressions in header
  | ShowFailureMessage String
  | UpdateSheet SheetUpdate 
  | ClearSheet ASSheetId
  | MakeSelection Selection
  | LoadImportedCells [ASCell] -- cells to add to sheet
  | ShowHeaderResult CompositeValue
  | PassBarToTest Bar
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
  | EvaluateHeader ASExpression
  | Get [ASIndex]
  | GetBar BarIndex
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
  | ImportCSV { csvIndex :: ASIndex, csvLang :: ASLanguage, csvFileName :: String }
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

asToFromJSON ''ServerMessage
asToFromJSON ''ServerAction
asToFromJSON ''MutateType
asToFromJSON ''EvalInstruction

asToJSON ''ClientMessage
asToJSON ''ClientAction


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
  TooFarBack                  -> "Too far back"
  TooFarForwards              -> "Too far forwards"
  _                           -> show e


-- | Creates a server message from an ASExecError. Used in makeReplyMessageFromCells and  makeDeleteMessage.
-- #needsrefactor when makeCondFormatMessage goes, this can probably go too
makeErrorMessage :: ASExecError -> ClientMessage
makeErrorMessage DecoupleAttempt = ClientMessage AskDecouple
makeErrorMessage e = ClientMessage $ ShowFailureMessage $ generateErrorMessage e
