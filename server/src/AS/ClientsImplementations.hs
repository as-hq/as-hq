module AS.ClientsImplementations where 

import Prelude
import AS.Types
import AS.Handler as H
import qualified Data.List as L
import Data.Text (Text)
import Data.Maybe
import Control.Concurrent (MVar)
import AS.Util
import qualified Network.WebSockets as WS


initDaemonFromMessageAndConn :: ASMessage -> WS.Connection -> Maybe ASDaemon
initDaemonFromMessageAndConn m c' = case m of 
  (Message _ _ _ (PayloadDaemonInit (ASInitDaemonConnection _ loc))) -> Just $ ASDaemon loc c'
  otherwise -> Nothing 

initUserFromMessageAndConn :: ASMessage -> WS.Connection -> ASUser
initUserFromMessageAndConn m c' = UserClient (messageUserId m) c' [initialViewingWindow]

-------------------------------------------------------------------------------------------------------------------------
-- | ASUser is a client

instance Client ASUser where 
  conn = userConn
  addClient uc s@(State ucs dcs dbc)
    | uc `elem` ucs = s
    | otherwise = State (uc:ucs) dcs dbc 
  removeClient uc s@(State ucs dcs dbc)
    | uc `elem` ucs = State (L.delete uc ucs) dcs dbc
    | otherwise = s
  handleMessage user state message = case (action message) of 
    Acknowledge -> WS.sendTextData (userConn user) ("ACK" :: Text)
    New         -> H.handleNew user state message
    Import      -> H.handleImport user state message
    Open        -> H.handleOpen user state message
    Close       -> H.handleClose user state message
    Evaluate    -> H.handleEval user state message 
    EvaluateRepl-> H.handleEvalRepl user state message
    Get         -> H.handleGet user state (payload message)
    Delete      -> H.handleDelete user state (payload message)
    Copy        -> H.handleCopy user state (payload message)
    CopyForced  -> H.handleCopyForced user state (payload message)
    Undo        -> (H.handleUndo user state) >> (printTimed "Server processed undo")
    Redo        -> (H.handleRedo user state) >> (printTimed "Server processed redo")
    Clear       -> H.handleClear user state
    AddTags     -> H.handleAddTags user state message
    RemoveTags  -> H.handleRemoveTags user state message
    UpdateWindow-> H.handleUpdateWindow user state message

-------------------------------------------------------------------------------------------------------------------------
-- | ASDaemon is a client

instance Client ASDaemon where 
  conn = daemonConn
  addClient dc s@(State ucs dcs dbc)
    | dc `elem` dcs = s
    | otherwise = State ucs (dc:dcs) dbc 
  removeClient dc s@(State ucs dcs dbc)
    | dc `elem` dcs = State ucs (L.delete dc dcs) dbc
    | otherwise = s
  handleMessage daemon state message = handleMessage user state message 
      where user = initUserFromMessageAndConn message (daemonConn daemon)
  -- ::ALEX:: refactor above