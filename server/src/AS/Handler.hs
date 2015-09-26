module AS.Handler where

import Control.Exception
import qualified Data.List as L hiding (any, zip, map, all)
import Data.Text hiding (head, any, filter, zip, map, all, concat)
import Data.Maybe(fromJust, isNothing)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Success)
import qualified Network.WebSockets as WS

import AS.Types
import AS.DB.API as DB
import AS.Util as U
import AS.Dispatch.Core as DP
import AS.Dispatch.Repl as DR 
import AS.Daemon as DM
import AS.Users as Users
import AS.Parsing.Out as O


-- | Handlers take message payloads and send the response to the client(s)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Sending message to client(s)

broadcast :: MVar ServerState -> ASMessage -> IO ()
broadcast state message = do
  (State ucs _ _) <- readMVar state
  forM_ ucs $ \(UserClient _ conn _ _) -> U.sendMessage message conn

sendBroadcastFiltered :: MVar ServerState -> ASMessage -> IO ()
sendBroadcastFiltered state msg = liftIO $ do 
	(State ucs _ _) <- readMVar state
	broadcastFiltered msg ucs

-- | Given a message (commit, cells, etc), only send (to each user) the cells in their viewing window
broadcastFiltered :: ASMessage -> [ASUser] -> IO ()
broadcastFiltered msg@(Message uid _ _ (PayloadCL cells)) users = mapM_ (sendCells cells) users 
  where
    sendCells :: [ASCell] -> ASUser -> IO ()
    sendCells cells user = do 
      let cells' = intersectViewingWindows cells (windows user)
      U.sendMessage msg (userConn user)

broadcastFiltered msg@(Message uid a r (PayloadLL locs)) users = mapM_ (sendLocs locs) users 
  where
    sendLocs :: [ASLocation] -> ASUser -> IO ()
    sendLocs locs user = do 
      let locs' = intersectViewingWindowsLocs locs (windows user)
      --putStrLn $ "Sending msg to client: " ++ (show msg)
      U.sendMessage msg (userConn user)

broadcastFiltered msg@(Message uid act res (PayloadCommit c)) users = mapM_ (sendCommit c) users
  where
    sendCommit :: ASCommit -> ASUser -> IO ()
    sendCommit commit user = do 
      let b = intersectViewingWindows (before commit) (windows user)
      let a = intersectViewingWindows (after commit) (windows user)
      U.sendMessage msg (userConn user)

sendToOriginalUser :: ASUser -> ASMessage -> IO ()
sendToOriginalUser user msg = WS.sendTextData (userConn user) (encode (U.updateMessageUser (userId user) msg))
----------------------------------------------------------------------------------------------------------------------------------------------
-- Open/close/import/new/window handlers

handleAcknowledge :: ASUser -> IO ()
handleAcknowledge user = WS.sendTextData (userConn user) ("ACK" :: Text)

handleNew :: MVar ServerState -> ASMessage -> IO ()
handleNew state (Message uid a _ p@(PayloadWorkbookSheets (wbs:[]))) = do
  conn <- fmap dbConn $ readMVar state
  wbs' <- DB.createWorkbookSheet conn wbs
  broadcast state (Message uid a Success (PayloadWorkbookSheets [wbs']))
handleNew state (Message uid a _(PayloadWB wb)) = do
  conn <- fmap dbConn $ readMVar state 
  wb' <- DB.createWorkbook conn (workbookSheets wb)
  broadcast state $ Message uid a Success (PayloadWB wb')
  return () -- TODO determine whether users should be notified

handleOpen :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleOpen user state (Message _ _ _ (PayloadS (Sheet sheetid _ _))) = Users.modifyUser makeNewWindow user state 
  where makeNewWindow (UserClient uid conn windows sid) = UserClient uid conn ((Window sheetid (-1,-1) (-1,-1)):windows) sid

handleClose :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleClose user state (Message _ _ _ (PayloadS (Sheet sheetid _ _))) = Users.modifyUser closeWindow user state
  where closeWindow (UserClient uid conn windows sid) = UserClient uid conn (filter (((/=) sheetid) . windowSheetId) windows) sid

-- ::ALEX:: point of user' ?? 
handleUpdateWindow :: SessionId -> MVar ServerState -> ASMessage -> IO ()
handleUpdateWindow sid state (Message uid _ _ (PayloadW window)) = do
  curState <- readMVar state
  let (Just user') = Users.getUserBySessionId sid curState -- if this fails then somehow your connection isn't stored in the state
  let maybeWindow = U.getWindow (windowSheetId window) user' 
  case maybeWindow of 
    Nothing -> putStrLn "ERROR: could not update nothing window" >> return ()
    (Just oldWindow) -> do
      let locs = U.getScrolledLocs oldWindow window 
      printTimed $ "Sending locs: " ++ (show locs)
      mcells <- DB.getCells (dbConn curState) locs
      let msg = U.getDBCellMessage (userId user') locs mcells
      sendToOriginalUser user' msg
      Users.modifyUser (U.updateWindow window) user' state

handleImport :: MVar ServerState -> ASMessage -> IO ()
handleImport state msg = return () -- TODO 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval handler 

handleEval :: MVar ServerState -> ASMessage -> IO ()
handleEval state msg  = do 
  putStrLn $ "IN EVAL HANDLER"
  msg' <- DP.runDispatchCycle state msg
  catch (sendBroadcastFiltered state msg') (\e -> putStrLn $ "error" ++ (show $ (e :: SomeException)))

handleEvalRepl :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleEvalRepl user state msg = do
  putStrLn $ "IN EVAL HANDLER"
  msg' <- DR.runReplDispatch user state msg
  sendToOriginalUser user msg'

----------------------------------------------------------------------------------------------------------------------------------------------
-- DB Handlers

handleGet :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleGet user state (PayloadLL locs) = do 
  curState <- readMVar state
  mcells <- DB.getCells (dbConn curState) locs 
  sendToOriginalUser user (U.getDBCellMessage (userId user) locs mcells) 
handleGet user state (PayloadList Sheets) = do
  curState <- readMVar state
  ss <- DB.getAllSheets (dbConn curState)
  let msg = Message (userId user) Update Success (PayloadSS ss)
  sendToOriginalUser user msg
handleGet user state (PayloadList Workbooks) = do
  curState <- readMVar state
  ws <- DB.getAllWorkbooks (dbConn curState)
  let msg = Message (userId user) Update Success (PayloadWBS ws)
  sendToOriginalUser user msg
handleGet user state (PayloadList WorkbookSheets) = do
  curState <- readMVar state
  wss <- DB.getAllWorkbookSheets (dbConn curState)
  printTimed $ "getting all workbooks: "  ++ (show wss)
  let msg = Message (userId user) Update Success (PayloadWorkbookSheets wss)
  sendToOriginalUser user msg

handleDelete :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleDelete user state p@(PayloadL loc) = do 
  conn <- fmap dbConn $ readMVar state
  DB.deleteLocs conn [loc]
  let msg = Message (userId user) Delete Success p
  sendBroadcastFiltered state msg
  return () 
handleDelete user state p@(PayloadLL locs) = do 
  conn <- fmap dbConn $ readMVar state
  DB.deleteLocs conn locs
  let msg = Message (userId user) Delete Success p
  sendBroadcastFiltered state msg
  return () 
handleDelete user state p@(PayloadWorkbookSheets (wbs:[])) = do
  conn <- fmap dbConn $ readMVar state
  DB.deleteWorkbookSheet conn wbs
  let msg = Message (userId user) Delete Success p
  broadcast state msg
  return () 
handleDelete user state p@(PayloadWB workbook) = do
  conn <- fmap dbConn $ readMVar state
  DB.deleteWorkbook conn (workbookName workbook) 
  let msg = Message (userId user) Delete Success p
  sendBroadcastFiltered state msg
  return () 

handleClear :: MVar ServerState -> IO ()
handleClear state = sendBroadcastFiltered state (failureMessage "")

handleUndo :: ASUserId -> MVar ServerState -> IO ()
handleUndo userId state = do 
  conn <- fmap dbConn $ readMVar state
  commit <- DB.undo conn
  msg <- case commit of 
    Nothing -> return $ failureMessage "Too far back"
    (Just c) -> return $ Message userId Undo Success (PayloadCommit c)
  sendBroadcastFiltered state msg
  printTimed "Server processed undo"

handleRedo :: ASUserId -> MVar ServerState -> IO ()
handleRedo userId state = do 
  curState <- readMVar state
  commit <- DB.redo (dbConn curState)
  msg <- case commit of 
		Nothing -> return $ failureMessage "Too far forwards"
		(Just c) -> return $ Message userId Redo Success (PayloadCommit c)
  sendBroadcastFiltered state msg
  printTimed "Server processed redo"

-- parse deps
-- check that all locations exist, else throw error
-- shift deps 
-- show deps into exlocs
-- update expression
-- update dag
-- insert new cells into db
handleCopy :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleCopy user state (PayloadLL (from:to:[])) = do -- this is a list of 2 locations
  curState <- readMVar state
  let conn = dbConn curState
  maybeCells <- DB.getCells conn [from]
  let fromCells = filterNothing maybeCells
  let offset = U.getOffsetBetweenLocs from to
  let toCellsAndDeps = map (O.shiftCell offset) fromCells
  let shiftedDeps = map snd toCellsAndDeps
  let allDeps = concat shiftedDeps
  let toCells = map fst toCellsAndDeps
  let toLocs = map cellLocation toCells
  printTimed $ "Copying cells: "
  allExistDB <- DB.locationsExist conn allDeps   -- check if deps exist in DB
  let allNonexistentDB = U.isoFilter not allExistDB allDeps  
  let allExist = U.isSubsetOf allNonexistentDB toLocs -- else if the dep was something we copied
  if allExist
    then do
      DB.setCells conn toCells
      DB.updateDAG conn $ zip shiftedDeps toLocs
      let msg = Message (userId user) Update Success (PayloadCL toCells)
      sendBroadcastFiltered state msg
    else do
      let msg = Message (userId user) Update (Failure $ generateErrorMessage CopyNonexistentDependencies) (PayloadE CopyNonexistentDependencies)
      sendToOriginalUser user msg

-- same without checking
handleCopyForced :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleCopyForced user state (PayloadLL (from:[to])) = return ()

----------------------------------------------------------------------------------------------------------------------------------------------
-- Tag handlers

processAddTag :: ASUser -> MVar ServerState -> ASLocation -> ASMessage -> ASCellTag -> IO ()
processAddTag user state loc msg t = do 
  curState <- readMVar state
  cell <- DB.getCell (dbConn curState) loc
  case cell of 
    Nothing -> return ()
    Just c@(Cell l e v ts) -> do 
      case (elem t ts) of 
        True -> return ()
        False -> do 
          let c' = Cell l e v (t:ts)
          DB.setCell (dbConn curState) c'
  case t of 
    StreamTag s -> do -- create daemon that sends an eval message
      mCells <- DB.getCells (dbConn curState) [loc]
      case (L.head mCells) of 
        Nothing -> return ()
        Just cell -> do 
          let evalMsg = Message (messageUserId msg) Evaluate NoResult (PayloadC cell)
          putStrLn "\n\n\nMADE IT HERE!!!!!!!!\n\n\n"
          DM.modifyDaemon state s loc evalMsg -- ::ALEX:: make this clearer
    otherwise -> return () -- TODO: implement the rest

processRemoveTag :: ASLocation -> MVar ServerState -> ASCellTag -> IO ()
processRemoveTag loc state t = do 
  curState <- readMVar state
  cell <- DB.getCell (dbConn curState) loc 
  case cell of 
    Nothing -> return ()
    Just c@(Cell l e v ts) -> do 
      let c' = Cell l e v (L.delete t ts)
      DB.setCell (dbConn curState) c'
  case t of
    StreamTag s -> DM.removeDaemon loc state
    otherwise -> return () -- TODO: implement the rest

handleAddTags :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleAddTags user state msg@(Message uid _ _ (PayloadTags ts loc)) = do 
  putStrLn "\n\n\nMADE IT INTO handleAddTags!!!!!!!!\n\n\n"
  mapM_ (processAddTag user state loc msg) ts
  let sendMsg = Message uid AddTags Success (PayloadN ())
  sendToOriginalUser user sendMsg

handleRemoveTags :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleRemoveTags user state msg@(Message uid _ _ (PayloadTags ts loc)) = do 
  mapM_ (processRemoveTag loc state) ts
  let sendMsg = Message uid RemoveTags Success (PayloadN ())
  sendToOriginalUser user sendMsg