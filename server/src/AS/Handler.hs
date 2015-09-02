module AS.Handler where

import Data.List as L
import Data.Text hiding (head)
import Data.Maybe(fromJust)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Success)
import qualified Network.WebSockets as WS

import AS.Types
import AS.DB as DB
import AS.Util as U
import AS.Dispatch as DP 
import AS.Daemon as DM
import AS.Clients as C


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
-- | Open/close/import/new/window handlers

handleNew :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleNew user state msg = case (payload msg) of 
  (PayloadSheet sheet) -> DB.createSheet sheet >>= 
    (\newSheet -> sendToOriginalUser user (Message (userId user) Update NoResult (PayloadSheet newSheet)))
  (PayloadWorkbook workbook) -> DB.createWorkbook >> return ()

handleOpen :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleOpen user state (Message _ _ _ (PayloadSheet sheetid)) = C.modifyUser makeNewWindow user state
  where makeNewWindow (User uid conn windows) = User uid conn ((Window sheetid (-1,-1) (-1,-1)):windows)

handleClose :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleClose user state (Message _ _ _ (PayloadSheet sheetid)) = C.modifyUser closeWindow user state
  where closeWindow (User uid conn windows) = User uid conn (filter (((\=) sheetid) . windowSheetId) windows)

handleUpdateWindow :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleUpdateWindow user state (Message uid _ _ (PayloadW window)) = 
  let maybeWindow = U.getWindow (windowSheetId window) user in 
  case maybeWindow of 
    Nothing -> putStrLn "ERROR: could not update nothing window" >> return ()
    (Just oldWindow) -> do
      cells <- DB.getCells $ U.getScrolledLocs oldWindow window 
      sendToOriginalUser user (Message uid NoAction Success (PayloadCL cells))
      C.modifyUser (U.updateWindow window) user state

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Eval handler 

handleEval :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleEval user state msg  = do 
  putStrLn $ "IN EVAL HANDLER"
  msg <- DP.runDispatchCycle user state msg
  sendBroadcastFiltered user state msg

----------------------------------------------------------------------------------------------------------------------------------------------
-- | DB Handlers

handleGet :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleGet user state (PayloadLL locs) = do 
	let msg = Message (userId user) Get Success (PayloadCL [])
	sendToOriginalUser user msg 
handleGet user state (PayloadList Sheets) = return () -- TODO
handleGet user state (PayloadList Workbooks) = return () -- TODO

-- | Not yet implemented
handleDelete :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleDelete user state p@(PayloadLL locs) = do 
  DB.deleteLocs locs
	let msg = Message (userId user) Delete Success p
	sendBroadcastFiltered user state msg 
handleDelete user state (PayloadSheet sheet) = DB.deleteSheet >> return ()
handleDelete user state (PayloadWorkbook workbook) = DB.deleteWorkbook >> return ()

handleClear :: ASUser -> MVar ServerState -> IO ()
handleClear user state = sendBroadcastFiltered user state (failureMessage "")

handleUndo :: ASUser -> MVar ServerState -> IO ()
handleUndo user state = do 
	commit <- DB.undo 
	msg <- case commit of 
		Nothing -> return $ failureMessage "Too far back"
		(Just c) -> return $ Message (userId user) Undo Success (PayloadCommit c)
	sendBroadcastFiltered user state msg

handleRedo :: ASUser -> MVar ServerState -> IO ()
handleRedo user state = do 
	commit <- DB.redo 
	msg <- case commit of 
		Nothing -> return $ failureMessage "Too far forwards"
		(Just c) -> return $ Message (userId user) Undo Success (PayloadCommit c)
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
handleAddTags user state msg@(Message uid _ _ (PayloadTags ts loc)) = do 
  _ <- (mapM_ (processAddTag user state loc msg) ts) 
  let sendMsg = Message uid AddTags Success (PayloadN ())
  sendToOriginalUser user sendMsg

handleRemoveTags :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleRemoveTags user state msg@(Message uid _ _ (PayloadTags ts loc)) = do 
  _ <- (mapM_ (processRemoveTag loc state) ts) 
  let sendMsg = Message uid RemoveTags Success (PayloadN ())
  sendToOriginalUser user sendMsg


