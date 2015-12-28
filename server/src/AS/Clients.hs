module AS.Clients where

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

-------------------------------------------------------------------------------------------------------------------------
-- ASUserClient is a client

shouldLogAction :: ServerAction -> Bool
shouldLogAction Acknowledge      = False
shouldLogAction (UpdateWindow _) = False
shouldLogAction (Open _)         = False
shouldLogAction  _               = True

instance Client ASUserClient where
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
    when (shouldLogAction $ serverAction message) $ do 
      logServerMessage (show message) (userCommitSource user)
      putStrLn "=========================================================="
      printObj "Message" (show message)
    redisConn <- dbConn <$> readMVar state
    storeLastMessage redisConn message (userCommitSource user)
    -- everything commented out here is a thing we are temporarily not supporting, because we only partially implemented them
    -- but don't want to maintain them (Alex 12/28)
    case (serverAction message) of
      Acknowledge                 -> handleAcknowledge user
      -- New                -> handleNew user state payload
      Open sid                    -> handleOpen user state sid
      -- Close                 -> handleClose user state payload
      UpdateWindow win            -> handleUpdateWindow (sessionId user) state win
      -- Import                -> handleImport user state payload
      Export sid                  -> handleExport user state sid
      Evaluate xp loc             -> handleEval user state xp loc
      -- EvaluateRepl          -> handleEvalRepl user payload
      EvaluateHeader xp           -> handleEvalHeader user state xp
      Get locs                    -> handleGet user state locs
      Delete sel                  -> handleDelete user state sel
      ClearSheetServer sid        -> handleClear user state sid
      Undo                        -> handleUndo user state
      Redo                        -> handleRedo user state
      Copy from to                -> handleCopy user state from to
      Cut from to                 -> handleCut user state from to
      ToggleProp prop rng         -> handleToggleProp user state prop rng
      SetProp prop rng            -> handleSetProp user state prop rng
      Repeat sel                  -> handleRepeat user state sel
      BugReport report            -> handleBugReport user report
      -- JumpSelect            -> handleJumpSelect user state payload
      MutateSheet mutateType      -> handleMutateSheet user state mutateType
      Drag selRng dragRng         -> handleDrag user state selRng dragRng
      Decouple                    -> handleDecouple user state
      UpdateCondFormatRules cfru  -> handleUpdateCondFormatRules user state cfru
      SetBarProp bInd prop        -> handleSetBarProp user state bInd prop
      ImportCSV ind lang fileName -> handleCSVImport user state ind lang fileName
      -- Undo         -> handleToggleProp user state (PayloadTags [StreamTag (Stream NoSource 1000)] (Index (T.pack "TEST_SHEET_ID2") (1,1)))
      -- ^^ above is to test streaming when frontend hasn't been implemented yet

-------------------------------------------------------------------------------------------------------------------------
-- ASDaemonClient is a client

instance Client ASDaemonClient where
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
    Evaluate xp loc -> handleEval' daemon state xp loc
    where 
      handleEval' :: ASDaemonClient -> MVar ServerState -> ASExpression -> ASIndex -> IO ()
      handleEval' dm state xp ind  = do
        conn <- dbConn <$> readMVar state
        oldProps <- getPropsAt conn ind
        let cell = Cell ind xp NoValue oldProps
        errOrUpdate <- runDispatchCycle state [cell] DescendantsWithParent (daemonCommitSource dm) id
        either (const $ return ()) (broadcastSheetUpdate state) errOrUpdate
      -- difference between this and handleEval being that it can't take back a failure message. 
      -- yes, code replication, whatever. 