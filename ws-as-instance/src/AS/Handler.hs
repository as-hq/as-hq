module AS.Handler where

import AS.DB as DB
import AS.Types
import AS.Util as U
import AS.Dispatch as DP 
import Data.Text hiding (head)
import Data.Maybe(fromJust)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Success)
import qualified Network.WebSockets as WS

import AS.Daemon as DM
import Data.List as L


-- | Handlers take message payloads and send the response to the client(s)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Sending message to client(s)

broadcast :: Text -> ServerState -> IO ()
broadcast message (State u) = forM_ (L.map fst u) $ \(User _ conn _) -> WS.sendTextData conn message

sendBroadcastFiltered :: ASUser -> MVar ServerState -> ASMessage -> IO ()
sendBroadcastFiltered user state msg = liftIO $ do 
	(State allUsers) <- readMVar state
	broadcastFiltered (updateMessageUser (userId user) msg) (L.map fst allUsers)

-- | Given a message (commit, cells, etc), only send (to each user) the cells in their viewing window
broadcastFiltered :: ASMessage -> [ASUser] -> IO ()
broadcastFiltered (Message uid _ _ (PayloadCL cells)) users = mapM_ (sendCells cells) users 
  where
    sendCells :: [ASCell] -> ASUser -> IO ()
    sendCells cells user = do 
      let cells' = intersectViewingWindows cells (userWindows user)
      let msg = Message uid Update Success (PayloadCL cells')
      putStrLn $ "Sending msg to client: " ++ (show msg)
      WS.sendTextData (userConn user) (encode msg)
broadcastFiltered (Message uid _ _ (PayloadCommit c)) users = mapM_ (sendCommit c) users
  where
    sendCommit :: ASCommit -> ASUser -> IO ()
    sendCommit commit user = do 
      let b = intersectViewingWindows (before commit) (userWindows user)
      let a = intersectViewingWindows (after commit) (userWindows user)
      let msg = Message uid Commit Success (PayloadCommit (ASCommit (commitUserId c) b a (time c)))
      WS.sendTextData (userConn user) (encode msg)

sendToOriginalUser :: ASUser -> ASMessage -> IO ()
sendToOriginalUser user msg = WS.sendTextData (userConn user) (encode (U.updateMessageUser (userId user) msg))  

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Eval handler 

handleEval :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleEval user state msg  = do 
  putStrLn $ "IN EVAL HANDLER"
  msg <- DP.runDispatchCycle user state msg
  sendToOriginalUser user msg

----------------------------------------------------------------------------------------------------------------------------------------------
-- | DB Handlers

handleGet :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleGet user state (PayloadLL locs) = do 
	let msg = Message genericText Get Success (PayloadCL [])
	sendBroadcastFiltered user state msg 

-- | Not yet implemented
handleDelete :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleDelete user state p@(PayloadLL locs) = do 
	let msg = Message genericText Delete Success p
	sendBroadcastFiltered user state msg 

handleClear :: ASUser -> MVar ServerState -> IO ()
handleClear user state = sendBroadcastFiltered user state (failureMessage "")

handleUndo :: ASUser -> MVar ServerState -> IO ()
handleUndo user state = do 
	commit <- DB.undo 
	msg <- case commit of 
		Nothing -> return $ failureMessage "Too far back"
		(Just c) -> return $ Message genericText Undo Success (PayloadCommit c)
	sendBroadcastFiltered user state msg

handleRedo :: ASUser -> MVar ServerState -> IO ()
handleRedo user state = do 
	commit <- DB.redo 
	msg <- case commit of 
		Nothing -> return $ failureMessage "Too far forwards"
		(Just c) -> return $ Message genericText Undo Success (PayloadCommit c)
	sendBroadcastFiltered user state msg


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Tag handlers

processAddTag :: ASUser -> MVar ServerState -> ASLocation -> ASMessage -> ASCellTag -> IO ()
processAddTag user state loc msg t = do 
  cell <- DB.getCell loc
  case cell of 
    Nothing -> return ()
    Just c@(Cell l e v ts) -> do 
      case (elem t ts) of 
        True -> return ()
        False -> do 
          let c' = Cell l e v (t:ts)
          DB.setCell c'
  case t of 
    StreamTag s -> do -- create daemon that sends an eval message
      mCells <- DB.getCells [loc]
      case (L.head mCells) of 
        Nothing -> return ()
        Just cell -> do 
          let evalMsg = Message (messageUserId msg) Evaluate NoResult (PayloadC cell)
          DM.modifyDaemon user state s loc evalMsg
    otherwise -> return () -- TODO: implement the rest

processRemoveTag :: ASLocation -> MVar ServerState -> ASCellTag -> IO ()
processRemoveTag loc state t = do 
  cell <- DB.getCell loc 
  case cell of 
    Nothing -> return ()
    Just c@(Cell l e v ts) -> do 
      let c' = Cell l e v (L.delete t ts)
      DB.setCell c'
  case t of
    StreamTag s -> DM.removeDaemon loc state
    otherwise -> return () -- TODO: implement the rest

handleAddTags :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleAddTags user state msg@(Message _ _ _ (PayloadAddTags ts loc)) = do 
  _ <- (mapM_ (processAddTag user state loc msg) ts) 
  let sendMsg = Message genericText AddTags Success (PayloadN ())
  sendToOriginalUser user sendMsg

handleRemoveTags :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleRemoveTags user state msg@(Message _ _ _ (PayloadRemoveTags ts loc)) = do 
  _ <- (mapM_ (processRemoveTag loc state) ts) 
  let sendMsg = Message genericText RemoveTags Success (PayloadN ())
  sendToOriginalUser user sendMsg


