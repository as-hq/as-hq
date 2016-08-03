module AS.Clients where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Aeson as A

import AS.Prelude
import AS.Types.Network 
import AS.Types.Cell
import AS.Types.Commits
import AS.Types.Messages hiding (userId)
import AS.Types.Eval
import AS.Types.DB hiding (Clear, UserType)
import AS.Types.Window

import AS.Handlers.Mutate
import AS.Handlers.Delete
import AS.Handlers.Paste
import AS.Handlers.Props
import AS.Handlers.Eval
import AS.Handlers.JumpSelect
import AS.Handlers.Misc
import AS.Handlers.Import
import AS.Handlers.Sheets
import AS.Handlers.LogAction
import AS.Handlers.BugReport

import qualified AS.Daemon as DM
import AS.DB.API (getPropsAt, storeLastMessage, getCellsInSheet)
import AS.Dispatch.Core
import AS.Reply
import AS.Logging


-------------------------------------------------------------------------------------------------------------------------
-- UserClient is a client

shouldLogMessage :: ServerMessage -> Bool
shouldLogMessage msg = case msg of 
  ServerMessage _ (LogAction _)       -> False
  ServerMessage _ (GetSessionLogs _)  -> False
  _ -> True

instance Client UserClient where
  clientType _ = UserType
  clientConn = view userConn
  sessionId = view userSessionId
  ownerName = view userId
  addClient uc s
    | uc `elem` (s^.userClients) = s
    | otherwise = s & userClients %~ (uc :)
  removeClient uc s
    | uc `elem` (s^.userClients) = s & userClients %~ (L.delete uc)
    | otherwise = s
  lookupClient uc s = L.find (== uc) (s^.userClients)
  clientCommitSource = userCommitSource
  handleImportBinary uc state bin = do
    curState <- readState state
    let msgctx = MessageContext { _messageState = state
                                , _messageId = "import_message_id"
                                , _userClient = uc 
                                , _dbConnection = curState^.dbConn
                                }
    handleImportBinaryUser msgctx bin
  handleServerMessage uc state message = do 
    -- second arg is supposed to be sheet id; temporary hack is to always set userId = sheetId
    -- on frontend. 
    when (shouldLogMessage message) $ do
      puts "=========================================================="
      putsSheet (srcSheetId . clientCommitSource $ uc) (show message)
    curState <- readState state
    storeLastMessage (curState^.dbConn) message (clientCommitSource uc)
    -- everything commented out here is a thing we are temporarily not supporting, because we only partially implemented them
    -- but don't want to maintain them (Alex 12/28)
    let mid = serverMessageId message 
    -- Log every message sent in the DB to help in replaying/debugging
    let msgctx = MessageContext { _messageState = state
                                , _messageId = mid
                                , _userClient = uc 
                                , _dbConnection = curState^.dbConn
                                }
    case serverAction message of
      LogAction _ -> return ()
      GetSessionLogs _ -> return ()
      otherwise -> handleLogMessage msgctx $ A.encode message
    case (serverAction message) of
      -- the following 3 actions take the mutable state rather than ServerState because it updates the uc's window
      OpenWorkbook wid            -> handleOpenWorkbook msgctx wid
      OpenSheet sid               -> handleOpenSheet msgctx sid
      NewWorkbook name            -> handleNewWorkbook msgctx name
      NewSheet name               -> handleNewSheet msgctx name
      CloneSheet sid              -> handleCloneSheet msgctx sid
      AcquireSheet sid            -> handleAcquireSheet msgctx sid
      AcquireWorkbook wid         -> handleAcquireWorkbook msgctx wid
      DereferenceSheet sid        -> handleDereferenceSheet msgctx sid
      DeleteSheet sid             -> handleDeleteSheet msgctx sid
      GetOpenedWorkbook           -> handleGetOpenedWorkbook msgctx
      GetMyWorkbooks              -> handleGetMyWorkbooks msgctx
      ExportWorkbook wid          -> handleExportWorkbook msgctx wid
      ExportCell idx              -> handleExportCell msgctx idx
      Evaluate xpsAndIndices      -> handleEval msgctx xpsAndIndices
      EvaluateHeader evalHeader   -> handleEvalHeader msgctx evalHeader
      Get locs                    -> handleGet msgctx locs
      GetIsCoupled loc            -> handleIsCoupled msgctx loc
      Delete sel                  -> handleDelete msgctx sel
      ClearSheetServer sid        -> handleClear msgctx sid
      Undo                        -> handleUndo msgctx
      Redo                        -> handleRedo msgctx
      Copy from to                -> handleCopy msgctx from to
      Cut from to                 -> handleCut msgctx from to
      ToggleProp prop rng         -> handleToggleProp msgctx prop rng
      SetProp prop rng            -> handleSetProp msgctx prop rng
      ChangeDecimalPrecision i rng-> handleChangeDecimalPrecision msgctx i rng
      Repeat sel                  -> handleRepeat msgctx sel
      BugReport report            -> handleBugReport msgctx report
      MutateSheet mutateType      -> handleMutateSheet msgctx mutateType
      Drag selRng dragRng         -> handleDrag msgctx selRng dragRng
      Decouple                    -> handleDecouple msgctx
      Timeout timeoutMid          -> handleTimeout (msgctx^.messageState) timeoutMid
      UpdateCondFormatRules rs ids-> handleUpdateCondFormatRules msgctx rs ids
      GetBar bInd                 -> handleGetBar msgctx bInd
      SetBarProp bInd prop        -> handleSetBarProp msgctx bInd prop
      ImportCSV ind lang fileName -> handleCSVImport msgctx ind lang fileName
      ImportExcel sid fileName    -> handleExcelImport msgctx sid fileName
      SetLanguagesInRange lang rng -> handleSetLanguagesInRange msgctx lang rng
      LogAction fAction            -> handleLogAction msgctx fAction
      GetSessionLogs logSource     -> handleGetSessionLogs msgctx logSource
      StartDebuggingLog            -> handleStopLoggingActions state
      GetAllSessions               -> handleGetAllSessions msgctx
      RenameSheet sid sname        -> handleRenameSheet msgctx sid sname
      RenameWorkbook wname         -> handleRenameWorkbook msgctx wname
      -- #needsrefactor the object view should be a part of the cell
      GetObjectView idx            -> handleGetObjectView msgctx idx
      TogglePauseMode sid          -> handleTogglePauseMode msgctx sid
      ReEval sid                   -> handleReEval msgctx sid
      MakeCheckpoint desc          -> handleMakeCheckpoint msgctx desc
      GetAllCheckpoints            -> handleGetAllCheckpoints msgctx
      ViewCheckpoint name user     -> handleViewCheckpoint msgctx name user
      ApplyCheckpoint name user    -> handleApplyCheckpoint msgctx name user
      RevertCheckpoint             -> handleRevertCheckpoint msgctx
      SetAutoEval idx delay        -> handleAutoEval msgctx idx delay

-------------------------------------------------------------------------------------------------------------------------
-- ASDaemonClient is a client

instance Client ASDaemonClient where
  clientType = undefined
  clientConn = undefined
  sessionId = undefined
  ownerName = undefined
  addClient = undefined
  removeClient = undefined
  lookupClient = undefined
  clientCommitSource = undefined
  handleServerMessage = undefined
