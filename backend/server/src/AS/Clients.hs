module AS.Clients where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Aeson as A
import Control.Monad (when)
import Control.Lens hiding ((.=))

import AS.Prelude
import Prelude()
import AS.Types.Network 
import AS.Types.Cell
import AS.Types.Messages hiding (userId)
import AS.Types.Eval
import AS.Types.DB hiding (Clear, UserType)

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

import qualified AS.Daemon as DM
import AS.DB.API (getPropsAt, storeLastMessage, getCellsInSheet)
import AS.Dispatch.Core
import AS.Reply
import AS.Logging


-------------------------------------------------------------------------------------------------------------------------
-- ASUserClient is a client

shouldLogMessage :: ServerMessage -> Bool
shouldLogMessage (ServerMessage _ (UpdateWindow _)) = False
shouldLogMessage (ServerMessage _ (OpenSheet _)) = False
shouldLogMessage _ = True

-- Whether to print the reception of the message on the console or not. 
-- Do not show logging messages, as they are distracting and happen on every frontend action
shouldPrintMessage :: ServerMessage -> Bool
shouldPrintMessage (ServerMessage _ (LogAction _)) = False
shouldPrintMessage (ServerMessage _ (GetSessionLogs _)) = False
shouldPrintMessage _ = True

instance Client ASUserClient where
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
  lookupClient uc s = $fromJust $ L.find (== uc) (s^.userClients)
  handleServerMessage user state message = do 
    -- second arg is supposed to be sheet id; temporary hack is to always set userId = sheetId
    -- on frontend. 
    when (shouldLogMessage message) $ logServerMessage (show message) (userCommitSource user)
    when (shouldPrintMessage message) $ do 
      putStrLn "=========================================================="
      printObjForced "Server received message" message
    curState <- readState state
    storeLastMessage (curState^.dbConn) message (userCommitSource user)
    -- everything commented out here is a thing we are temporarily not supporting, because we only partially implemented them
    -- but don't want to maintain them (Alex 12/28)
    let mid = serverMessageId message 
    -- Log every message sent in the DB to help in replaying/debugging
    case serverAction message of
      LogAction _ -> return ()
      GetSessionLogs _ -> return ()
      otherwise -> handleLogMessage user curState $ A.encode message
    case (serverAction message) of
      -- New                -> handleNew user state payload
      OpenSheet sid                    -> handleOpenSheet mid user state sid
      NewSheet sheetName               -> handleNewSheet mid user curState sheetName
      GetMySheets                 -> handleGetSheets mid user curState
      -- Close                 -> handleClose user curState payload
      UpdateWindow win            -> handleUpdateWindow mid user curState win
      -- Import                -> handleImport user curState payload
      Export sid                  -> handleExport user curState sid
      Evaluate xpsAndIndices      -> handleEval mid user curState xpsAndIndices
      -- EvaluateRepl          -> handleEvalRepl user payload
      EvaluateHeader evalHeader   -> handleEvalHeader mid user curState evalHeader
      Get locs                    -> handleGet mid user curState locs
      GetIsCoupled loc            -> handleIsCoupled mid user curState loc
      Delete sel                  -> handleDelete mid user curState sel
      ClearSheetServer sid        -> handleClear mid user curState sid
      Undo                        -> handleUndo mid user curState
      Redo                        -> handleRedo mid user curState
      Copy from to                -> handleCopy mid user curState from to
      Cut from to                 -> handleCut mid user curState from to
      ToggleProp prop rng         -> handleToggleProp mid user curState prop rng
      SetProp prop rng            -> handleSetProp mid user curState prop rng
      ChangeDecimalPrecision i rng -> handleChangeDecimalPrecision mid user curState i rng
      Repeat sel                  -> handleRepeat mid user curState sel
      BugReport report            -> handleBugReport user report
      -- JumpSelect            -> handleJumpSelect user curState payload
      MutateSheet mutateType      -> handleMutateSheet mid user curState mutateType
      Drag selRng dragRng         -> handleDrag mid user curState selRng dragRng
      Decouple                    -> handleDecouple mid user curState
      Timeout timeoutMid          -> handleTimeout timeoutMid state
      UpdateCondFormatRules rs ids-> handleUpdateCondFormatRules mid user curState rs ids
      GetBar bInd                 -> handleGetBar mid user curState bInd
      SetBarProp bInd prop        -> handleSetBarProp mid user curState bInd prop
      ImportCSV ind lang fileName -> handleCSVImport mid user curState ind lang fileName
      ImportExcel sid fileName    -> handleExcelImport mid user curState sid fileName
      SetLanguagesInRange lang rng -> handleSetLanguagesInRange mid user curState lang rng
      LogAction fAction            -> handleLogAction user curState fAction
      GetSessionLogs logSource     -> handleGetSessionLogs mid user curState logSource
      StartDebuggingLog            -> handleStopLoggingActions state
      GetAllSessions               -> handleGetAllSessions mid user
      --Undo         -> $error "Simulated crash"
      -- ^^ above is to test API endpoints which don't have a frontend implementation

-------------------------------------------------------------------------------------------------------------------------
-- ASDaemonClient is a client

instance Client ASDaemonClient where
  clientType _ = DaemonType
  clientConn = daemonConn
  sessionId = T.pack . DM.getDaemonName . daemonLoc
  ownerName = daemonOwner
  addClient dc s
    | dc `elem` (s^.daemonClients) = s
    | otherwise = s & daemonClients %~ (dc :)
  removeClient dc s
    | dc `elem` (s^.daemonClients) = s & daemonClients %~ (L.delete dc)
    | otherwise = s
  lookupClient dc s = $fromJust $ L.find (== dc) (s^.daemonClients)
  handleServerMessage daemon mstate message = case (serverAction message) of
    Evaluate xpsAndIndices -> do
      state <- readState mstate
      handleEval' (serverMessageId message) daemon state xpsAndIndices
    where 
      handleEval' :: MessageId -> ASDaemonClient -> ServerState -> [EvalInstruction] -> IO ()
      handleEval' mid dm state evalInstructions  = do
        let xps  = map evalXp evalInstructions
            inds = map evalLoc evalInstructions 
            conn = state^.dbConn
        oldProps <- mapM (getPropsAt conn) inds
        let cells = map (\(xp, ind, props) -> Cell ind xp NoValue props Nothing Nothing) $ zip3 xps inds oldProps
        errOrUpdate <- runDispatchCycle state cells DescendantsWithParent (daemonCommitSource dm) id
        either (const $ return ()) (broadcastSheetUpdate mid state) errOrUpdate
      -- difference between this and handleEval being that it can't take back a failure message. 
      -- yes, code replication, whatever. 
