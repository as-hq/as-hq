module AS.Clients where

import Control.Exception
import Prelude
import qualified Data.List as L
import qualified Data.Text as T
import Data.Maybe
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Success)
import AS.Daemon (getDaemonName)
import qualified Network.WebSockets as WS

import AS.Types.Core
import AS.DB.API            as DB
import AS.DB.Graph          as G
import AS.Util              as U
import AS.Dispatch.Core     as DP
import AS.Dispatch.Repl     as DR 
import AS.Users             as US
import AS.Parsing.Out       as O
import AS.Daemon            as DM

-------------------------------------------------------------------------------------------------------------------------
-- Initializations

initDaemonFromMessageAndConn :: ASMessage -> WS.Connection -> Maybe ASDaemon
initDaemonFromMessageAndConn m c' = case m of 
  (Message _ _ _ (PayloadDaemonInit (ASInitDaemonConnection _ loc))) -> Just $ ASDaemon loc c'
  otherwise -> Nothing 

initUserFromMessageAndConn :: ASMessage -> WS.Connection -> IO ASUser
initUserFromMessageAndConn m c' = do 
    let uid = messageUserId m 
    time <- getTime
    return $ UserClient uid c' [initialViewingWindow] $ T.pack ((show uid) ++ (show time))

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
    Acknowledge  -> handleAcknowledge user
    New          -> handleNew state message
    Open         -> handleOpen user state message
    Close        -> handleClose user state message
    UpdateWindow -> handleUpdateWindow (sessionId user) state message
    Import       -> handleImport state message
    Evaluate     -> handleEval user state message
    EvaluateRepl -> handleEvalRepl user state message
    Get          -> handleGet user state (payload message)
    Delete       -> handleDelete user state (payload message)
    Clear        -> handleClear user state
    Undo         -> handleUndo user state
    Redo         -> handleRedo user state
    Copy         -> handleCopy user state (payload message)
    CopyForced   -> handleCopyForced user state (payload message)
    AddTags      -> handleAddTags user state message
    RemoveTags   -> handleRemoveTags user state message
    -- Undo         -> handleAddTags user state (Message (userId user) AddTags (NoResult) (PayloadTags [StreamTag (Stream NoSource 1000)] (Index (T.pack "TEST_SHEET_ID2") (1,1))))
    -- ^^ above is to test streaming when frontend hasn't been implemented yet

-------------------------------------------------------------------------------------------------------------------------
-- ASDaemon is a client

instance Client ASDaemon where 
  conn = daemonConn
  clientId = T.pack . getDaemonName . daemonLoc
  addClient dc s@(State ucs dcs dbc)
    | dc `elem` dcs = s
    | otherwise = State ucs (dc:dcs) dbc 
  removeClient dc s@(State ucs dcs dbc)
    | dc `elem` dcs = State ucs (L.delete dc dcs) dbc
    | otherwise = s
  handleClientMessage daemon state message = case (action message) of 
    Evaluate -> handleEval daemon state message

-- Handlers take message payloads and send the response to the client(s)


----------------------------------------------------------------------------------------------------------------------------------------------
-- Sending message to user client(s)

broadcast :: MVar ServerState -> ASMessage -> IO ()
broadcast state message = do
  (State ucs _ _) <- readMVar state
  forM_ ucs $ \(UserClient _ conn _ _) -> U.sendMessage message conn

sendBroadcastFiltered :: (Client c) => c -> MVar ServerState -> ASMessage -> IO ()
sendBroadcastFiltered cl state msg@(Message _ _ (Failure e) _) = sendToOriginal cl msg          -- send error to original user only
sendBroadcastFiltered _ state msg@(Message _ Delete Success _) = broadcast state msg            -- broadcast all deletes (scrolling only refreshes non-deleted cells)
sendBroadcastFiltered _ state msg@(Message _ _ Success (PayloadCommit _)) = broadcast state msg -- broadcast all undo/redos for same reason
sendBroadcastFiltered _ state msg = liftIO $ do 
  (State ucs _ _) <- readMVar state
  broadcastFiltered msg ucs

-- | Given a message (commit, cells, etc), only send (to each user) the cells in their viewing window
broadcastFiltered :: ASMessage -> [ASUser] -> IO ()
broadcastFiltered msg@(Message uid a r (PayloadCL cells)) users = mapM_ (sendCells cells) users 
  where
    sendCells :: [ASCell] -> ASUser -> IO ()
    sendCells cells user = do 
      let cells' = intersectViewingWindows cells (windows user)
      case cells' of 
        [] -> return ()
        _ -> U.sendMessage (Message uid a r (PayloadCL cells')) (userConn user)

broadcastFiltered msg@(Message uid a r (PayloadLL locs)) users = mapM_ (sendLocs locs) users 
  where
    sendLocs :: [ASLocation] -> ASUser -> IO ()
    sendLocs locs user = do 
      let locs' = intersectViewingWindowsLocs locs (windows user)
      case locs' of 
        [] -> return ()
        _ -> U.sendMessage (Message uid a r (PayloadLL locs')) (userConn user)

sendToOriginal :: (Client c) => c -> ASMessage -> IO ()
sendToOriginal cl msg = WS.sendTextData (conn cl) (encode (U.updateMessageUser (clientId cl) msg))

----------------------------------------------------------------------------------------------------------------------------------------------
-- Open/close/import/new/window handlers

handleAcknowledge :: ASUser -> IO ()
handleAcknowledge user = WS.sendTextData (userConn user) ("ACK" :: T.Text)

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
handleOpen user state (Message _ _ _ (PayloadS (Sheet sheetid _ _))) = US.modifyUser makeNewWindow user state 
  where makeNewWindow (UserClient uid conn windows sid) = UserClient uid conn ((Window sheetid (-1,-1) (-1,-1)):windows) sid

handleClose :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleClose user state (Message _ _ _ (PayloadS (Sheet sheetid _ _))) = US.modifyUser closeWindow user state
  where closeWindow (UserClient uid conn windows sid) = UserClient uid conn (filter (((/=) sheetid) . windowSheetId) windows) sid

handleUpdateWindow :: ClientId -> MVar ServerState -> ASMessage -> IO ()
handleUpdateWindow sid state (Message uid _ _ (PayloadW window)) = do
  curState <- readMVar state
  let (Just user') = US.getUserByClientId sid curState -- user' is to get latest user on server; if this fails then somehow your connection isn't stored in the state
  let maybeWindow = U.getWindow (windowSheetId window) user' 
  case maybeWindow of 
    Nothing -> putStrLn "ERROR: could not update nothing window" >> return ()
    (Just oldWindow) -> do
      let locs = U.getScrolledLocs oldWindow window 
      printTimed $ "Sending locs: " ++ (show locs)
      mcells <- DB.getCells locs
      let msg = U.getDBCellMessage (userId user') locs mcells
      sendToOriginal user' msg
      US.modifyUser (U.updateWindow window) user' state

handleImport :: MVar ServerState -> ASMessage -> IO ()
handleImport state msg = return () -- TODO 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval handler 

handleEval :: (Client c) => c -> MVar ServerState -> ASMessage -> IO ()
handleEval cl state msg  = do 
  putStrLn $ "IN EVAL HANDLER"
  msg' <- DP.runDispatchCycle state msg
  catch (sendBroadcastFiltered cl state msg') (\e -> putStrLn $ "handleEval error: " ++ (show $ (e :: SomeException)))

handleEvalRepl :: (Client c) => c -> MVar ServerState -> ASMessage -> IO ()
handleEvalRepl cl state msg = do
  putStrLn $ "IN EVAL HANDLER"
  msg' <- DR.runReplDispatch state msg
  sendToOriginal cl msg'

----------------------------------------------------------------------------------------------------------------------------------------------
-- DB Handlers

handleGet :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleGet user state (PayloadLL locs) = do 
  curState <- readMVar state
  mcells <- DB.getCells locs 
  sendToOriginal user (U.getDBCellMessage (userId user) locs mcells) 
handleGet user state (PayloadList Sheets) = do
  curState <- readMVar state
  ss <- DB.getAllSheets (dbConn curState)
  let msg = Message (userId user) Update Success (PayloadSS ss)
  sendToOriginal user msg
handleGet user state (PayloadList Workbooks) = do
  curState <- readMVar state
  ws <- DB.getAllWorkbooks (dbConn curState)
  let msg = Message (userId user) Update Success (PayloadWBS ws)
  sendToOriginal user msg
handleGet user state (PayloadList WorkbookSheets) = do
  curState <- readMVar state
  wss <- DB.getAllWorkbookSheets (dbConn curState)
  printTimed $ "getting all workbooks: "  ++ (show wss)
  let msg = Message (userId user) Update Success (PayloadWorkbookSheets wss)
  sendToOriginal user msg

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
  printTimed "Server processed undo"

handleRedo :: ASUser -> MVar ServerState -> IO ()
handleRedo user state = do 
  curState <- readMVar state
  commit <- DB.redo (dbConn curState)
  msg <- case commit of 
    Nothing -> return $ failureMessage "Too far forwards"
    (Just c) -> return $ Message (userId user) Redo Success (PayloadCommit c)
  sendBroadcastFiltered user state msg
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
  maybeCells <- DB.getCells [from]
  let fromCells = filterNothing maybeCells
      offset = U.getOffsetBetweenLocs from to
      toCellsAndDeps = map (O.shiftCell offset) fromCells
      shiftedDeps = map snd toCellsAndDeps
      allDeps = concat shiftedDeps
      toCells = map fst toCellsAndDeps
      toLocs = map cellLocation toCells
  printTimed $ "Copying cells: "
  allExistDB <- DB.locationsExist conn allDeps   -- check if deps exist in DB
  let allNonexistentDB = U.isoFilter not allExistDB allDeps  
      allExist = U.isSubsetOf allNonexistentDB toLocs -- else if the dep was something we copied
  if allExist
    then do
      DB.setCells toCells
      G.setRelations $ zip toLocs shiftedDeps 
      let msg = Message (userId user) Update Success (PayloadCL toCells)
      sendBroadcastFiltered user state msg
    else do
      let msg = Message (userId user) Update (Failure $ generateErrorMessage CopyNonexistentDependencies) (PayloadE CopyNonexistentDependencies)
      sendToOriginal user msg

-- same without checking
handleCopyForced :: ASUser -> MVar ServerState -> ASPayload -> IO ()
handleCopyForced user state (PayloadLL (from:[to])) = return ()

----------------------------------------------------------------------------------------------------------------------------------------------
-- Tag handlers

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
          DM.modifyDaemon state s loc evalMsg -- put the daemon with loc and evalMsg on that cell -- overwrite if already exists, create if not
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
  mapM_ (processAddTag user state loc msg) ts
  let sendMsg = Message uid AddTags Success (PayloadN ())
  sendToOriginal user sendMsg

handleRemoveTags :: ASUser -> MVar ServerState -> ASMessage -> IO ()
handleRemoveTags user state msg@(Message uid _ _ (PayloadTags ts loc)) = do 
  mapM_ (processRemoveTag loc state) ts
  let sendMsg = Message uid RemoveTags Success (PayloadN ())
  sendToOriginal user sendMsg

-- Debugging
--getScrollCells :: Connection -> ASSheetId -> [ASLocation] -> IO [Maybe ASCell]
--getScrollCells conn sid locs = if ((sid == (T.pack "SHEET_ID")) && S.isDebug)
--  then do
--    let dlocs = concat $ map U.decomposeLocs locs
--    return $ map (\l -> Just $ Cell l (Expression "scrolled" Python) (ValueS (show . index $ l)) []) dlocs
-- --  else DB.getCells conn locs
-- getScrollCells :: ASSheetId -> [ASLocation] -> IO [Maybe ASCell]
-- getScrollCells sid locs = DB.getCells locs