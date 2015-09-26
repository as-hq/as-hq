module AS.Clients where

import Prelude
import AS.Types
import AS.Handler as H
import qualified Data.List as L
import Data.Text as T
import Data.Maybe
import Control.Concurrent (MVar)
import AS.Util
import Data.Aeson hiding (Success)
import AS.Daemon (getDaemonName)
import qualified Network.WebSockets as WS


initDaemonFromMessageAndConn :: ASMessage -> WS.Connection -> Maybe ASDaemon
initDaemonFromMessageAndConn m c' = case m of 
  (Message _ _ _ (PayloadDaemonInit (ASInitDaemonConnection _ loc))) -> Just $ ASDaemon loc c'
  otherwise -> Nothing 

initUserFromMessageAndConn :: ASMessage -> WS.Connection -> IO ASUser
initUserFromMessageAndConn m c' = do 
    let uid = messageUserId m 
    time <- getTime
    return $ UserClient uid c' [initialViewingWindow] ((show uid) ++ (show time))

-------------------------------------------------------------------------------------------------------------------------
-- ASUser is a client

instance Client ASUser where 
  conn = userConn
  clientId = sessionId
  addClient uc s@(State ucs dcs dbc)
    | uc `elem` ucs = s
    | otherwise = State (uc:ucs) dcs dbc 
  removeClient uc s@(State ucs dcs dbc)
    | uc `elem` ucs = State (L.delete uc ucs) dcs dbc
    | otherwise = s
  handleClientMessage user state message = case (action message) of 
    Acknowledge  -> H.handleAcknowledge user
    New          -> H.handleNew state message
    Open         -> H.handleOpen user state message
    Close        -> H.handleClose user state message
    UpdateWindow -> H.handleUpdateWindow (sessionId user) state message
    Import       -> H.handleImport state message
    Evaluate     -> H.handleEval state message
    EvaluateRepl -> H.handleEvalRepl user state message
    Get          -> H.handleGet user state (payload message)
    Delete       -> H.handleDelete user state (payload message)
    Clear        -> H.handleClear state
    Undo         -> H.handleUndo (userId user) state
    Redo         -> H.handleRedo (userId user) state
    Copy         -> H.handleCopy user state (payload message)
    CopyForced   -> H.handleCopyForced user state (payload message)
    AddTags      -> H.handleAddTags user state message
    RemoveTags   -> H.handleRemoveTags user state message
-- Undo         -> putStrLn "\n\n\nHI!!!!\n\n\n" >> H.handleAddTags user state (Message (userId user) AddTags (NoResult) (PayloadTags [StreamTag (Stream NoSource 5000)] (Index (T.pack "TEST_SHEET_ID2") (1,1)))) <-- to test streaming when frontend hasn't been implemented yet

-------------------------------------------------------------------------------------------------------------------------
-- ASDaemon is a client

instance Client ASDaemon where 
  conn = daemonConn
  clientId d = getDaemonName (daemonLoc d)
  addClient dc s@(State ucs dcs dbc)
    | dc `elem` dcs = s
    | otherwise = State ucs (dc:dcs) dbc 
  removeClient dc s@(State ucs dcs dbc)
    | dc `elem` dcs = State ucs (L.delete dc dcs) dbc
    | otherwise = s
  handleClientMessage daemon state message = case (action message) of 
    Evaluate -> H.handleEval state message