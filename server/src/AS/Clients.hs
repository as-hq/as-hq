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
import Control.Monad (unless)

-------------------------------------------------------------------------------------------------------------------------
-- ASUserClient is a client

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
  handleClientMessage user state message = do 
    -- second arg is supposed to be sheet id; temporary hack is to always set userId = sheetId
    -- on frontend. 
    unless (clientAction message == Acknowledge) $ do 
      logClientMessage (show message) (userCommitSource user)
      putStrLn "=========================================================="
      printObj "Message" (show message)
    redisConn <- dbConn <$> readMVar state
    storeLastMessage redisConn message (userCommitSource user)
    case (clientAction message) of
      Acknowledge        -> handleAcknowledge user
      -- New                -> handleNew user state payload -- temporarly disabled
      Open               -> handleOpen user state payload
      Close              -> handleClose user state payload
      UpdateWindow       -> handleUpdateWindow (sessionId user) state payload
      Import             -> handleImport user state payload
      Export             -> handleExport user state payload
      Evaluate           -> handleEval user state payload
      -- this case is deprecated for now. (anand 1/1/16)
      --EvaluateRepl       -> handleEvalRepl user payload
      EvaluateHeader     -> handleEvalHeader user state payload
      Get                -> handleGet user state payload
      Delete             -> handleDelete user state payload
      Clear              -> handleClear user state payload
      Undo               -> handleUndo user state
      Redo               -> handleRedo user state
      Copy               -> handleCopy user state payload
      Cut                -> handleCut user state payload
      ToggleProp         -> handleToggleProp user state payload
      SetProp            -> handleSetProp user state payload
      Repeat             -> handleRepeat user state payload
      BugReport          -> handleBugReport user payload
      JumpSelect         -> handleJumpSelect user state payload
      MutateSheet        -> handleMutateSheet user state payload
      Drag               -> handleDrag user state payload
      Decouple           -> handleDecouple user state payload
      SetCondFormatRules -> handleSetCondFormatRules user state payload
      SetBarProp      -> handleSetBarProp user state payload
      ImportCSV          -> handleCSVImport user state payload
      where payload = clientPayload message
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
  handleClientMessage daemon state message = case (clientAction message) of
    Evaluate -> handleEval' daemon state (clientPayload message)
    where 
      handleEval' :: ASDaemonClient -> MVar ServerState -> ASPayload -> IO ()
      handleEval' dm@(DaemonClient (Index sid _ ) _ uid) state payload  = do
        let cells = case payload of 
                      PayloadCL cells' -> cells'
        -- The PayloadCL is sort of a misnomer; it's only really being used as a wrapper around the
        -- expression to evaluate and the location of evaluation. In particular, the value passed in the cells
        -- are irrelevant, and there are no tags passed in, so we have to get the tags from the database
        -- manually. 
        redisConn <- dbConn <$> readMVar state
        oldTags <- getPropsAt redisConn (map cellLocation cells)
        let cells' = map (\(c, ps) -> c { cellProps = ps }) (zip cells oldTags)
        errOrCommit <- runDispatchCycle state cells' DescendantsWithParent (CommitSource sid uid) id
        either (const $ return ()) ((broadcastFiltered' state) . makeReplyMessageFromCommit) errOrCommit
-- Handlers take message payloads and send the response to the client(s)

