module AS.Handler where

import qualified Data.List as L hiding (any, zip, map, all)
import Data.Text hiding (head, any, filter, zip, map, all, concat)
import Data.Maybe(fromJust, isNothing)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Success)
import qualified Network.WebSockets as WS

import AS.Types.Core
import AS.DB.API            as DB
import AS.DB.Graph          as G
import AS.Util              as U
import AS.Dispatch.Core     as DP
import AS.Dispatch.Repl     as DR 
import AS.Daemon            as DM
import AS.Clients           as C
import AS.Parsing.Out       as O


-- | Handlers take message payloads and send the response to the client(s)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Sending message to client(s)

broadcast :: MVar ServerState -> ASMessage -> IO ()
broadcast state message = do
  (State u _) <- readMVar state
  forM_ (map fst u) $ \(User _ conn _) -> WS.sendTextData conn (encode message)

sendBroadcastFiltered :: ASUser -> MVar ServerState -> ASMessage -> IO ()
sendBroadcastFiltered user state msg@(Message _ _ (Failure e) _) = sendToOriginalUser user msg  
sendBroadcastFiltered user state msg = liftIO $ do 
	(State allUsers _) <- readMVar state
	broadcastFiltered (updateMessageUser (userId user) msg) (map fst allUsers)

-- | Given a message (commit, cells, etc), only send (to each user) the cells in their viewing window
broadcastFiltered :: ASMessage -> [ASUser] -> IO ()
broadcastFiltered msg@(Message uid _ _ (PayloadCL cells)) users = mapM_ (sendCells cells) users 
  where
    sendCells :: [ASCell] -> ASUser -> IO ()
    sendCells cells user = do 
      let cells' = intersectViewingWindows cells (userWindows user)
      --putStrLn $ "Sending msg to client: " ++ (show msg)
      WS.sendTextData (userConn user) (encode msg)
broadcastFiltered msg@(Message uid a r (PayloadLL locs)) users = mapM_ (sendLocs locs) users 
  where
    sendLocs :: [ASLocation] -> ASUser -> IO ()
    sendLocs locs user = do 
      let locs' = intersectViewingWindowsLocs locs (userWindows user)
      --putStrLn $ "Sending msg to client: " ++ (show msg)
      WS.sendTextData (userConn user) (encode msg)
broadcastFiltered msg@(Message uid act res (PayloadCommit c)) users = mapM_ (sendCommit c) users
  where
    sendCommit :: ASCommit -> ASUser -> IO ()
    sendCommit commit user = do 
      let b = intersectViewingWindows (before commit) (userWindows user)
      let a = intersectViewingWindows (after commit) (userWindows user)
      WS.sendTextData (userConn user) (encode msg)

sendToOriginalUser :: ASUser -> ASMessage -> IO ()
sendToOriginalUser user msg = WS.sendTextData (userConn user) (encode (U.updateMessageUser (userId user) msg))  
----------------------------------------------------------------------------------------------------------------------------------------------
-- | Open/close/import/new/window handlers

handleNew :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleNew user state (Message uid a _ p@(PayloadWorkbookSheets (wbs:[]))) = do
  conn <- fmap dbConn $ readMVar state
  wbs' <- DB.createWorkbookSheet conn wbs
  broadcast state (Message uid a Success (PayloadWorkbookSheets [wbs']))
handleNew user state (Message uid a _(PayloadWB wb)) = do
  conn <- fmap dbConn $ readMVar state 
  wb' <- DB.createWorkbook conn (workbookSheets wb)
  broadcast state $ Message uid a Success (PayloadWB wb')
  return () -- TODO determine whether users should be notified

handleOpen :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleOpen user state (Message _ _ _ (PayloadS (Sheet sheetid _ _))) = C.modifyUser makeNewWindow user state 
  where makeNewWindow (User uid conn windows) = User uid conn ((Window sheetid (-1,-1) (-1,-1)):windows)

handleClose :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleClose user state (Message _ _ _ (PayloadS (Sheet sheetid _ _))) = C.modifyUser closeWindow user state
  where closeWindow (User uid conn windows) = User uid conn (filter (((/=) sheetid) . windowSheetId) windows)

handleUpdateWindow :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleUpdateWindow user state (Message uid _ _ (PayloadW window)) = do
  curState <- readMVar state
  let (Just user') = C.getUserById (userId user) curState
  let maybeWindow = U.getWindow (windowSheetId window) user' 
  --printTimed $ "Current user' windows before update: " ++ (show $ userWindows user')
  case maybeWindow of 
    Nothing -> putStrLn "ERROR: could not update nothing window" >> return ()
    (Just oldWindow) -> do
      let locs = U.getScrolledLocs oldWindow window 
      printTimed $ "Sending locs: " ++ (show locs)
      mcells <- DB.getCells locs
      let msg = U.getDBCellMessage user' locs mcells
      --printTimed $ "Sending scroll message: " ++ (show msg)
      sendToOriginalUser user' msg
      C.modifyUser (U.updateWindow window) user' state
      --readState' <- readMVar state
      --let (Just user'') = C.getUserById (userId user) readState'
      --printTimed $ "Current user' windows after update: " ++ (show $ userWindows user'')

handleImport :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleImport user state msg = return () -- TODO 

----msg------------------------------------------------------------------------------------------------------------------------------------------
-- | Eval handler 

handleEval :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleEval user state msg  = do 
  putStrLn $ "IN EVAL HANDLER"
  msg' <- DP.runDispatchCycle user state msg
  sendBroadcastFiltered user state msg'

handleEvalRepl :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleEvalRepl user state msg = do
  putStrLn $ "IN EVAL HANDLER"
  msg' <- DR.runReplDispatch user state msg
  sendToOriginalUser user msg'

----------------------------------------------------------------------------------------------------------------------------------------------
-- | DB Handlers

handleGet :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleGet user state (PayloadLL locs) = do 
  mcells <- DB.getCells locs 
  sendToOriginalUser user (U.getDBCellMessage user locs mcells) 
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
  sendBroadcastFiltered user state msg
  return () 
handleDelete user state p@(PayloadLL locs) = do 
  conn <- fmap dbConn $ readMVar state
  DB.deleteLocs conn locs
  let msg = Message (userId user) Delete Success p
  sendBroadcastFiltered user state msg
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
  sendBroadcastFiltered user state msg
  return () 

handleClear :: ASUser -> MVar ServerState -> IO ()
handleClear user state = sendBroadcastFiltered user state (failureMessage "")

handleUndo :: ASUser -> MVar ServerState -> IO ()
handleUndo user state = do 
  conn <- fmap dbConn $ readMVar state
  commit <- DB.undo conn
  msg <- case commit of 
    Nothing -> return $ failureMessage "Too far back"
    (Just c) -> return $ Message (userId user) Undo Success (PayloadCommit c)
  sendBroadcastFiltered user state msg

handleRedo :: ASUser -> MVar ServerState -> IO ()
handleRedo user state = do 
  curState <- readMVar state
  commit <- DB.redo (dbConn curState)
  msg <- case commit of 
		Nothing -> return $ failureMessage "Too far forwards"
		(Just c) -> return $ Message (userId user) Redo Success (PayloadCommit c)
  sendBroadcastFiltered user state msg

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
  maybeCells <- DB.getCells [from]
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
      DB.setCells toCells
      G.setRelations $ zip toLocs shiftedDeps 
      let msg = Message (userId user) Update Success (PayloadCL toCells)
      sendBroadcastFiltered user state msg
    else do
      let msg = Message (userId user) Update (Failure $ generateErrorMessage CopyNonexistentDependencies) (PayloadE CopyNonexistentDependencies)
      sendToOriginalUser user msg

-- same without checking
handleCopyForced :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleCopyForced user state (PayloadLL (from:[to])) = return ()

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
  curState <- readMVar state
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


