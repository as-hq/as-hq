{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Messages where

import AS.Window

import AS.Types.DB (ASCommit)
import AS.Types.Cell
import AS.Types.RowColProps
import AS.Types.Sheets
import AS.Types.Excel (indexToExcel)
import AS.Types.Locations
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.CellProps

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
  | New
  | Import | Export
  | Open | Close
  | Evaluate | EvaluateRepl | EvaluateHeader
  | Update
  | Get | Delete
  | Copy | Cut | CopyForced
  | Undo | Redo
  | Clear
  | UpdateWindow
  | SetProp | ToggleProp
  | SetRowColProp
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
  | PayloadOpen {initHeaderExpressions :: [ASExpression], initCondFormatRules :: [CondFormatRule], initRowCols :: [RowCol]}
  | PayloadReplValue ASReplValue
  | PayloadValue CompositeValue
  | PayloadList QueryList
  | PayloadText {text :: String}
  | PayloadMutate MutateType
  | PayloadDrag {initialRange :: ASRange, dragRange :: ASRange}
  | PayloadCondFormat { condFormatRules :: [CondFormatRule] }
  | PayloadCondFormatResult { condFormatRulesResult :: [CondFormatRule], condFormatCellsUpdated :: [ASCell] }
  | PayloadSetRowColProp RowColType Int RowColProp
  deriving (Show, Read, Generic)

data ASReplValue = ReplValue {replValue :: ASValue, replLang :: ASLanguage} deriving (Show, Read, Eq, Generic)

data ASInitConnection = ASInitConnection {connUserId :: ASUserId, connSheetId :: ASSheetId} deriving (Show,Read,Eq,Generic)

data ASInitDaemonConnection = ASInitDaemonConnection {parentUserId :: ASUserId, initDaemonLoc :: ASIndex} deriving (Show,Read,Eq,Generic)

data MutateType = InsertCol { insertColNum :: Int } | InsertRow { insertRowNum :: Int } |
                  DeleteCol { deleteColNum :: Int } | DeleteRow { deleteRowNum :: Int } |
                  DragCol { oldColNum :: Int, newColNum :: Int } | DragRow { oldRowNum :: Int, newRowNum :: Int }
                  deriving (Show, Read, Eq, Generic)

data CondFormatRule = CondFormatRule { cellLocs :: [ASRange],
                                       condition :: CondFormatCondition,
                                       condFormat :: CellProp } deriving (Show, Read, Generic, Eq)

-- TODO: Timchu, 12/14/15. This is not complete! Date expressions are not online,
-- nor are text expressions.
-- CustomExpressions are separate from OneExpresssionConditions since
-- OneExpressionConditions work by evaluating the condition, then applying a
-- function with two ASValue Arguments into it.
-- Could potentially refactor CustomExpression to be a OneExpressionCondition
-- by passing in an appropriate function.
data CondFormatCondition =
  CustomExpressionCondition { customExpression :: ASExpression }
  |  OneExpressionCondition { oneExpressionType :: OneExpressionType
                           , oneExpression :: ASExpression }
  | NoExpressionsCondition { noExpressionsType :: NoExpressionsType }
--  | DateExpression { dateExpressionType :: DateExpressionType
--                   , dateExpression  :: DateExpression }
  | TwoExpressionsCondition { twoExpressionsType  :: TwoExpressionsType
                  , expression1 ::ASExpression
                  , expression2 :: ASExpression}
   deriving (Show, Read, Generic, Eq)


-- TODO: timchu, 12/17/15. Maybe want to split this into InequalityExpTypes and TextExpTypes?
data OneExpressionType = GreaterThan | Equals | Geq | Leq | LessThan | NotEquals
  deriving (Show, Read, Generic, Eq)

data NoExpressionsType = IsEmpty | IsNotEmpty
  deriving (Show, Read, Generic, Eq)

-- data DateExpressionType = DateIs  | DateIsBefore | DateIsAfter
--   deriving (Show, Read, Generic, Eq)

data TwoExpressionsType = IsBetween | IsNotBetween
  deriving (Show, Read, Generic, Eq)

-- timchu, 12/17/15. Begin helper functions that help with Conditional formatting.
-- TODO: timchu, 12/17/15.  Ask Ritesh about names.
-- | Functions that help with InequalityExpressionTypes
functionFromOneExpressionType :: OneExpressionType -> (ASValue -> ASValue -> Bool)
functionFromOneExpressionType iet =
  case iet of
       GreaterThan -> (>=)
       Geq -> (>)
       LessThan -> (<)
       Leq -> (<=)
       Equals -> (==)
       NotEquals -> (/=)

-- NOTE: timchu, this may be in wrong place.
-- TODO: timchu, 12/17/15. this is not exactly right. Should be the same as Evalues.
-- This is a temporary thing to avoid having to reimplement all the EValue helper
-- functions.
instance Ord ASValue where
  -- TODO: is this right?
  (<=) NoValue v  = (<=) (ValueI 0) v
  (<=) (ValueB True) v = (<=) (ValueI 1) v
  (<=) v (ValueB True) = (<=) v (ValueI 1)
  (<=) (ValueB False) v = (<=) (ValueI 0) v
  (<=) v (ValueB False) = (<=) v (ValueI 0)

  (<=) (ValueS s) (ValueI i) = False
  (<=) (ValueI i) (ValueS s)  = False

  (<=) (ValueS s) (ValueD d) = False
  (<=) (ValueD d) (ValueS s)  = False

  (<=) (ValueI i) (ValueD d) = (<=) (fromIntegral i) d
  (<=) (ValueD d) (ValueI i)  = (<=) d (fromIntegral i)

  (<=) (ValueS s1) (ValueS s2) = (<=) s1 s2
  (<=) (ValueI i1) (ValueI i2) = (<=) i1 i2
  (<=) (ValueD d1) (ValueD d2) = (<=) d1 d2
  (<=) _ _ = error "Invalid ASValue comparison"

-- | Functions that help with isEmpty types.
-- TODO: timchu, these names suck!!!!
isEmpty :: ASValue  -> Bool
isEmpty v =
  case v of
       NoValue -> True
       otherwise -> False

functionFromNoExpressionsType ::  NoExpressionsType -> (ASValue -> Bool)
functionFromNoExpressionsType neType =
  case neType of
       IsEmpty -> isEmpty
       IsNotEmpty -> not . isEmpty

-- tests if value is between a1 and a2 inclusive.  Excel does it this way.
isBetween :: ASValue -> ASValue -> ASValue -> Bool
isBetween value a1 a2 = value >= min a1 a2 && max a1 a2 >= value

functionFromTwoExpressionsType :: TwoExpressionsType -> ASValue -> ASValue -> ASValue -> Bool
functionFromTwoExpressionsType tet value a1 a2 =
  case tet of
       IsBetween ->  isBetween value a1 a2
       IsNotBetween ->  not $ isBetween value a1 a2
-- End of functions that help with conditional formatting.

instance ToJSON CondFormatCondition
instance FromJSON CondFormatCondition

instance ToJSON OneExpressionType
instance FromJSON OneExpressionType

instance ToJSON NoExpressionsType
instance FromJSON NoExpressionsType

instance ToJSON TwoExpressionsType
instance FromJSON TwoExpressionsType

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


-- | Creates a server message from an ASExecError. Used in makeUpdateMessage and  makeDeleteMessage.
makeErrorMessage :: ASExecError -> ASAction -> ASServerMessage
makeErrorMessage DecoupleAttempt a = ServerMessage a DecoupleDuringEval (PayloadN ())
makeErrorMessage e a = ServerMessage a (Failure (generateErrorMessage e)) (PayloadN ())

-- | When you have a list of cells from an eval request, this function constructs
-- the message to send back.
makeUpdateMessage :: Either ASExecError [ASCell] -> ASServerMessage
makeUpdateMessage (Left err) = makeErrorMessage err Update
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
makeDeleteMessage _ s@(ServerMessage _ (DecoupleDuringEval) _) = ServerMessage Delete DecoupleDuringEval (PayloadN ())
makeDeleteMessage deleteLocs s@(ServerMessage _ _ (PayloadCL cells)) = ServerMessage Delete Success payload
  where locsCells = zip (map cellLocation cells) cells
        cells'    = map snd $ filter (\(l, _) -> not $ rangeContainsIndex deleteLocs l) locsCells
        payload   = PayloadDelete deleteLocs cells'
        -- remove the sels from the update that we know are blank from the deleted locs

-- handle failure cases like makeUpdateMessage
-- otherwise: send payload CondFormatResult.
makeCondFormatMessage :: Either ASExecError [ASCell] -> [CondFormatRule] -> ASServerMessage
makeCondFormatMessage (Left err) _ = makeErrorMessage err SetCondFormatRules
makeCondFormatMessage (Right cells) rules = ServerMessage SetCondFormatRules Success payload
  where payload = PayloadCondFormatResult rules cells

changeMessageAction :: ASAction -> ASServerMessage -> ASServerMessage
changeMessageAction a (ServerMessage _ r p) = ServerMessage a r p
