module AS.Clients where

import AS.Prelude
import Prelude()

import AS.Types.Network
import AS.Types.Cell
import AS.Types.Messages
import AS.Types.Eval
import AS.Types.DB hiding (Clear)

import AS.Handlers.Mutate
import AS.Handlers.Delete
import AS.Handlers.Paste
import AS.Handlers.Props
import AS.Handlers.Eval
import AS.Handlers.JumpSelect
import AS.Handlers.Misc
import AS.Handlers.Import

import qualified AS.Daemon as DM
import AS.DB.API (getPropsAt, storeLastMessage, getCellsInSheet)
import AS.Dispatch.Core
import AS.Reply
import AS.Logging

import qualified Data.List as L
import qualified Data.Text as T

import Control.Concurrent
import Control.Monad (when)
import Control.Lens hiding ((.=))

-------------------------------------------------------------------------------------------------------------------------
-- ASUserClient is a client

shouldLogMessage :: ServerMessage -> Bool
shouldLogMessage (ServerMessage Acknowledge)      = False
shouldLogMessage (ServerMessage (UpdateWindow _)) = False
shouldLogMessage (ServerMessage (Open _))         = False
shouldLogMessage _                                = True

shouldPrintMessage :: ServerMessage -> Bool
shouldPrintMessage (ServerMessage Acknowledge) = False
shouldPrintMessage _                           = True

instance Client ASUserClient where
  clientType uc = User
  conn = userConn
  clientId = sessionId
  ownerName = userId
  addClient uc s@(State ucs dcs dbc port)
    | uc `elem` ucs = s
    | otherwise = State (uc:ucs) dcs dbc port
  removeClient uc s@(State ucs dcs dbc port)
    | uc `elem` ucs = State (L.delete uc ucs) dcs dbc port
    | otherwise = s
  handleServerMessage user state message = do 
    -- second arg is supposed to be sheet id; temporary hack is to always set userId = sheetId
    -- on frontend. 
    when (shouldLogMessage message) $ logServerMessage (show message) (userCommitSource user)
    when (shouldPrintMessage message) $ do 
      putStrLn "=========================================================="
      printObjForced "Server received message" message
    curState <- readMVar state
    storeLastMessage (curState^.dbConn) message (userCommitSource user)
    -- everything commented out here is a thing we are temporarily not supporting, because we only partially implemented them
    -- but don't want to maintain them (Alex 12/28)
    case (serverAction message) of
      Acknowledge                 -> handleAcknowledge user
      Initialize _ _              -> handleInitialize user 
      -- New                -> handleNew user state payload
      Open sid                    -> handleOpen user state sid
      -- Close                 -> handleClose user state payload
      UpdateWindow win            -> handleUpdateWindow user state win
      -- Import                -> handleImport user state payload
      Export sid                  -> handleExport user state sid
      Evaluate xpsAndIndices      -> handleEval user state xpsAndIndices
      -- EvaluateRepl          -> handleEvalRepl user payload
      EvaluateHeader evalHeader   -> handleEvalHeader user curState evalHeader
      Get locs                    -> handleGet user state locs
      GetIsCoupled loc            -> handleIsCoupled user state loc
      Delete sel                  -> handleDelete user state sel
      ClearSheetServer sid        -> handleClear user state sid
      Undo                        -> handleUndo user state
      Redo                        -> handleRedo user state
      Copy from to                -> handleCopy user state from to
      Cut from to                 -> handleCut user state from to
      ToggleProp prop rng         -> handleToggleProp user state prop rng
      SetProp prop rng            -> handleSetProp (curState^.dbConn) user prop rng
      Repeat sel                  -> handleRepeat user state sel
      BugReport report            -> handleBugReport user report
      -- JumpSelect            -> handleJumpSelect user state payload
      MutateSheet mutateType      -> handleMutateSheet user state mutateType
      Drag selRng dragRng         -> handleDrag user state selRng dragRng
      Decouple                    -> handleDecouple user state
      UpdateCondFormatRules cfru  -> handleUpdateCondFormatRules user state cfru
      GetBar bInd                 -> handleGetBar user state bInd
      SetBarProp bInd prop        -> handleSetBarProp user state bInd prop
      ImportCSV ind lang fileName -> handleCSVImport user state ind lang fileName
      --Undo         -> $error "Simulated crash"
      -- ^^ above is to test streaming when frontend hasn't been implemented yet

-------------------------------------------------------------------------------------------------------------------------
-- ASDaemonClient is a client

instance Client ASDaemonClient where
  clientType uc = Daemon
  conn = daemonConn
  clientId = T.pack . DM.getDaemonName . daemonLoc
  ownerName = daemonOwner
  addClient dc s@(State ucs dcs dbc port)
    | dc `elem` dcs = s
    | otherwise = State ucs (dc:dcs) dbc port
  removeClient dc s@(State ucs dcs dbc port)
    | dc `elem` dcs = State ucs (L.delete dc dcs) dbc port
    | otherwise = s
  handleServerMessage daemon state message = case (serverAction message) of
    Evaluate xpsAndIndices -> handleEval' daemon state xpsAndIndices
    where 
      handleEval' :: ASDaemonClient -> MVar ServerState -> [EvalInstruction] -> IO ()
      handleEval' dm state evalInstructions  = do
        let xps  = map evalXp evalInstructions
            inds = map evalLoc evalInstructions 
        conn <- view dbConn <$> readMVar state
        oldProps <- mapM (getPropsAt conn) inds
        let cells = map (\(xp, ind, props) -> Cell ind xp NoValue props Nothing Nothing) $ zip3 xps inds oldProps
        errOrUpdate <- runDispatchCycle state cells DescendantsWithParent (daemonCommitSource dm) id
        either (const $ return ()) (broadcastSheetUpdate state) errOrUpdate
      -- difference between this and handleEval being that it can't take back a failure message. 
      -- yes, code replication, whatever. 
