module AS.Clients where

import Control.Exception
import Prelude
import qualified Data.List as L
import qualified Data.Text as T
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Success)
import qualified Network.WebSockets as WS
import qualified Database.Redis as R

import AS.Types.Core
import AS.DB.API                as DB
import AS.DB.Util               as DU
import AS.DB.Graph              as G
import AS.Util                  as U
import AS.Dispatch.Core         as DP
import AS.Dispatch.Repl         as DR
import AS.Users                 as US
import AS.Parsing.Substitutions as S
import AS.Daemon                as DM

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-------------------------------------------------------------------------------------------------------------------------
-- ASUserClient is a client

instance Client ASUserClient where
  conn = userConn
  clientId = sessionId
  ownerName = userId
  addClient uc s@(State ucs dcs dbc)
    | uc `elem` ucs = s
    | otherwise = State (uc:ucs) dcs dbc
  removeClient uc s@(State ucs dcs dbc)
    | uc `elem` ucs = State (L.delete uc ucs) dcs dbc
    | otherwise = s
  handleClientMessage user state message = do 
    printWithTime ("\n\nMessage: " ++ (show $ message))
    redisConn <- dbConn <$> readMVar state
    recordMessage redisConn message
    case (clientAction message) of
      Acknowledge  -> handleAcknowledge user
      New          -> handleNew state payload
      Open         -> handleOpen user state payload
      Close        -> handleClose user state payload
      UpdateWindow -> handleUpdateWindow (sessionId user) state payload
      Import       -> handleImport state payload
      Evaluate     -> handleEval user state payload
      EvaluateRepl -> handleEvalRepl user state payload
      Get          -> handleGet user state payload
      Delete       -> handleDelete user state payload
      Clear        -> handleClear user state
      Undo         -> handleUndo user state
      Redo         -> handleRedo user state
      Copy         -> handleCopy user state payload
      Cut          -> handleCut user state payload
      CopyForced   -> handleCopyForced user state payload
      AddTags      -> handleAddTags user state payload
      RemoveTags   -> handleRemoveTags user state payload
      Repeat       -> handleRepeat user state payload
      where payload = clientPayload message
    -- Undo         -> handleAddTags user state (PayloadTags [StreamTag (Stream NoSource 1000)] (Index (T.pack "TEST_SHEET_ID2") (1,1)))
    -- ^^ above is to test streaming when frontend hasn't been implemented yet

-------------------------------------------------------------------------------------------------------------------------
-- ASDaemonClient is a client

instance Client ASDaemonClient where
  conn = daemonConn
  clientId = T.pack . DM.getDaemonName . daemonLoc
  ownerName = daemonOwner
  addClient dc s@(State ucs dcs dbc)
    | dc `elem` dcs = s
    | otherwise = State ucs (dc:dcs) dbc
  removeClient dc s@(State ucs dcs dbc)
    | dc `elem` dcs = State ucs (L.delete dc dcs) dbc
    | otherwise = s
  handleClientMessage daemon state message = case (clientAction message) of
    Evaluate -> handleEval daemon state (clientPayload message)

-- Handlers take message payloads and send the response to the client(s)


----------------------------------------------------------------------------------------------------------------------------------------------
-- Sending message to user client(s)

broadcast :: MVar ServerState -> ASServerMessage -> IO ()
broadcast state message = do
  (State ucs _ _) <- readMVar state
  forM_ ucs $ \(UserClient _ conn _ _) -> U.sendMessage message conn

sendBroadcastFiltered :: (Client c) => c -> MVar ServerState -> ASServerMessage -> IO ()
sendBroadcastFiltered cl state msg@(ServerMessage _ (Failure e) _) = sendToOriginal cl msg     -- send error to original user only
sendBroadcastFiltered _  state msg@(ServerMessage _ _ (PayloadCommit _)) = broadcast state msg -- broadcast all undo/redos (scrolling only refreshes non-undone cells)
sendBroadcastFiltered _  state msg@(ServerMessage Clear _ _) = broadcast state msg             -- broadcast all clears for the same reason
-- sendBroadcastFiltered _  state msg@(ServerMessage Delete _ _) = broadcast state msg         -- no separate broadcast for delete anymore
sendBroadcastFiltered _  state msg = liftIO $ do
  (State ucs _ _) <- readMVar state
  broadcastFiltered msg ucs

-- | Given a message (commit, cells, etc), only send (to each user) the cells in their viewing window
broadcastFiltered :: ASServerMessage -> [ASUserClient] -> IO ()
broadcastFiltered msg@(ServerMessage a r (PayloadCL cells)) users = mapM_ (sendCells cells) users
  where
    sendCells :: [ASCell] -> ASUserClient -> IO ()
    sendCells cells user = do
      let cells' = intersectViewingWindows cells (windows user)
      case cells' of
        [] -> return ()
        _ -> U.sendMessage (ServerMessage a r (PayloadCL cells')) (userConn user)

broadcastFiltered msg@(ServerMessage a r (PayloadLL locs)) users = mapM_ (sendLocs locs) users
  where
    sendLocs :: [ASIndex] -> ASUserClient -> IO ()
    sendLocs locs user = do
      let locs' = intersectViewingWindowsLocs locs (windows user)
      case locs' of
        [] -> return ()
        _ -> U.sendMessage (ServerMessage a r (PayloadLL locs')) (userConn user)

sendToOriginal :: (Client c) => c -> ASServerMessage -> IO ()
sendToOriginal cl msg = WS.sendTextData (conn cl) (encode msg)


----------------------------------------------------------------------------------------------------------------------------------------------
-- Open/close/import/new/window handlers

handleAcknowledge :: ASUserClient -> IO ()
handleAcknowledge user = WS.sendTextData (userConn user) ("ACK" :: T.Text)

handleNew :: MVar ServerState -> ASPayload -> IO ()
handleNew state (PayloadWorkbookSheets (wbs:[])) = do
  conn <- dbConn <$> readMVar state
  wbs' <- DB.createWorkbookSheet conn wbs
  broadcast state $ ServerMessage New Success (PayloadWorkbookSheets [wbs'])
handleNew state (PayloadWB wb) = do
  conn <- dbConn <$> readMVar state
  wb' <- DB.createWorkbook conn (workbookSheets wb)
  broadcast state $ ServerMessage New Success (PayloadWB wb')
  return () -- TODO determine whether users should be notified

handleOpen :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleOpen user state (PayloadS (Sheet sheetid _ _)) = US.modifyUser makeNewWindow user state
  where makeNewWindow (UserClient uid conn windows sid) = UserClient uid conn ((Window sheetid (-1,-1) (-1,-1)):windows) sid

handleClose :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleClose user state (PayloadS (Sheet sheetid _ _)) = US.modifyUser closeWindow user state
  where closeWindow (UserClient uid conn windows sid) = UserClient uid conn (filter (((/=) sheetid) . windowSheetId) windows) sid

handleUpdateWindow :: ClientId -> MVar ServerState -> ASPayload -> IO ()
handleUpdateWindow sid state (PayloadW window) = do
  curState <- readMVar state
  let (Just user') = US.getUserByClientId sid curState -- user' is to get latest user on server; if this fails then somehow your connection isn't stored in the state
  let maybeWindow = U.getWindow (windowSheetId window) user'
  case maybeWindow of
    Nothing -> putStrLn "ERROR: could not update nothing window" >> return ()
    (Just oldWindow) -> (flip catch) (badCellsHandler $ dbConn curState) (do
      let locs = U.getScrolledLocs oldWindow window
      printWithTime $ "Sending locs: " ++ (show locs)
      mcells <- DB.getCells $ concat $ map rangeToIndices locs
      sendToOriginal user' $ U.getDBCellMessage mcells
      US.modifyUser (U.updateWindow window) user' state)

-- | If a message is failing to parse from the server, undo the last commit (the one that added
-- the message to the server.) I doubt this fix is completely foolproof, but it keeps data
-- from getting lost and doesn't require us to manually reset the server. 
badCellsHandler :: R.Connection -> SomeException -> IO ()
badCellsHandler conn e = do 
  printWithTime ("Error while fetching cells: " ++ (show e))
  printWithTime "Undoing last commit"
  DB.undo conn
  return ()

handleImport :: MVar ServerState -> ASPayload -> IO ()
handleImport state msg = return () -- TODO

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval handler

handleEval :: (Client c) => c -> MVar ServerState -> ASPayload -> IO ()
handleEval cl state payload  = do
  let cells = case payload of 
                PayloadCL cells' -> cells'
  putStrLn $ "IN EVAL HANDLER"
  msg' <- DP.runDispatchCycle state cells (ownerName cl)
  sendBroadcastFiltered cl state msg'

handleEvalRepl :: (Client c) => c -> MVar ServerState -> ASPayload -> IO ()
handleEvalRepl cl state (PayloadXp xp) = do
  putStrLn $ "IN EVAL HANDLER"
  msg' <- DR.runReplDispatch state xp
  sendToOriginal cl msg'

----------------------------------------------------------------------------------------------------------------------------------------------
-- DB Handlers

handleGet :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleGet user state (PayloadLL locs) = do
  curState <- readMVar state
  mcells <- DB.getCells locs
  sendToOriginal user (U.getDBCellMessage mcells)
handleGet user state (PayloadList Sheets) = do
  curState <- readMVar state
  ss <- DB.getAllSheets (dbConn curState)
  sendToOriginal user $ ServerMessage Update Success (PayloadSS ss)
handleGet user state (PayloadList Workbooks) = do
  curState <- readMVar state
  ws <- DB.getAllWorkbooks (dbConn curState)
  sendToOriginal user $ ServerMessage Update Success (PayloadWBS ws)
handleGet user state (PayloadList WorkbookSheets) = do
  curState <- readMVar state
  wss <- DB.getAllWorkbookSheets (dbConn curState)
  printWithTime $ "getting all workbooks: "  ++ (show wss)
  sendToOriginal user $ ServerMessage Update Success (PayloadWorkbookSheets wss)

handleDelete :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleDelete user state p@(PayloadWorkbookSheets (wbs:[])) = do
  conn <- dbConn <$> readMVar state
  DB.deleteWorkbookSheet conn wbs
  broadcast state $ ServerMessage Delete Success p
  return ()
handleDelete user state p@(PayloadWB workbook) = do
  conn <- dbConn <$> readMVar state
  DB.deleteWorkbook conn (workbookName workbook)
  sendBroadcastFiltered user state $ ServerMessage Delete Success p
  return ()
handleDelete user state payload = do
  let locs = case payload of
               PayloadLL locs' -> locs'
               PayloadR rng -> rangeToIndices rng
  conn <- dbConn <$> readMVar state
  let blankedCells = U.blankCellsAt Python locs -- TODO set language appropriately 
  msg <- DP.runDispatchCycle state blankedCells (userId user)
  sendBroadcastFiltered user state msg

handleClear :: (Client c) => c  -> MVar ServerState -> IO ()
handleClear client state = do
  conn <- dbConn <$> readMVar state
  DB.clear conn
  G.clear
  sendBroadcastFiltered client state $ ServerMessage Clear Success $ PayloadN ()

handleUndo :: ASUserClient -> MVar ServerState -> IO ()
handleUndo user state = do
  conn <- dbConn <$> readMVar state
  commit <- DB.undo conn
  msg <- case commit of
    Nothing -> return $ failureMessage "Too far back"
    (Just c) -> return $ ServerMessage Undo Success (PayloadCommit c)
  sendBroadcastFiltered user state msg
  printWithTime "Server processed undo"

handleRedo :: ASUserClient -> MVar ServerState -> IO ()
handleRedo user state = do
  conn <- dbConn <$> readMVar state
  commit <- DB.redo conn
  msg <- case commit of
    Nothing -> return $ failureMessage "Too far forwards"
    (Just c) -> return $ ServerMessage Redo Success (PayloadCommit c)
  sendBroadcastFiltered user state msg
  printWithTime "Server processed redo"

handleCopy :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleCopy user state (PayloadPaste from to) = do
  conn <- dbConn <$> readMVar state
  toCells <- getPasteCells conn from to
  msg' <- DP.runDispatchCycle state toCells (userId user)
  sendBroadcastFiltered user state msg'

handleCut :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleCut user state (PayloadPaste from to) = do
  conn <- dbConn <$> readMVar state
  toCells <- getPasteCells conn from to
  let blankedCells = U.blankCellsAt Python (rangeToIndices from) -- TODO set actual language
      newCells = U.mergeCells toCells blankedCells -- content in pasted cells take precedence over deleted cells
  msg' <- DP.runDispatchCycle state newCells (userId user)
  sendBroadcastFiltered user state msg'

-- same without checking. This might be broken.
handleCopyForced :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleCopyForced user state (PayloadLL (from:to:[])) = return ()

----------------------------------------------------------------------------------------------------------------------------------------------
-- Copy/paste helpers

-- | Gets you the new cells to eval after shifting from a copy/paste or cut/paste. 
getPasteCells :: R.Connection -> ASRange -> ASRange -> IO [ASCell]
getPasteCells conn from to = do 
  fromCells    <- DB.getPossiblyBlankCells (rangeToIndices from)
  sanitizedFromCells <- sanitizeCopyCells conn fromCells from
  let offsets            = U.getPasteOffsets from to  -- how much to shift these cells for copy/copy/paste
      toCells            = concat $ map (\o -> map (S.shiftCell o) sanitizedFromCells) offsets
  return toCells

-- | Decouples cells appropriately for re-eval on copy/paste or cut/paste, as follows:
--   * if everything is a list head, leave it as is. 
--   * if a cell is not a part of a list, leave it as is. 
--   * if an entire list is contained in the range, keep just the head of the list. (So on eval
--     the entire list is re-evaluated)
--   * if a cell is part of a list that is not contained entirely in the selection, decouple it. 
sanitizeCopyCells :: R.Connection -> [ASCell] -> ASRange -> IO [ASCell]
sanitizeCopyCells conn cells from
  | all isListHead cells = return cells 
  | otherwise            = do 
      listsInRange <- getListsInRange conn from
      let (listCells, nonListCells)             = L.partition U.isListMember cells
          (containedListCells, cutoffListCells) = U.partitionByListKeys listCells listsInRange
          decoupledCells                        = map decoupleCell cutoffListCells
          containedListHeads                    = filter isListHead containedListCells
      return $ nonListCells ++ decoupledCells ++ containedListHeads

----------------------------------------------------------------------------------------------------------------------------------------------
-- Tag handlers

processAddTag :: ASUserClient -> MVar ServerState -> ASIndex -> ASCellTag -> IO ()
processAddTag user state loc t = do
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
          let evalMsg = ClientMessage Evaluate (PayloadCL [cell])
          DM.modifyDaemon state s loc evalMsg -- put the daemon with loc and evalMsg on that cell -- overwrite if already exists, create if not
    otherwise -> return () -- TODO: implement the rest

processRemoveTag :: ASIndex -> MVar ServerState -> ASCellTag -> IO ()
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

handleAddTags :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleAddTags user state (PayloadTags ts loc) = do
  mapM_ (processAddTag user state loc) ts
  sendToOriginal user $ ServerMessage AddTags Success (PayloadN ())

handleRemoveTags :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleRemoveTags user state (PayloadTags ts loc) = do
  mapM_ (processRemoveTag loc state) ts
  sendToOriginal user $ ServerMessage RemoveTags Success (PayloadN ())

handleRepeat :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleRepeat user state (PayloadSelection range origin) = do
  conn <- dbConn <$> readMVar state
  ClientMessage lastAction lastPayload <- getLastMessage conn
  case lastAction of 
    Evaluate -> do 
      let PayloadCL ((Cell l e v ts):[]) = lastPayload
          cells = map (\l' -> Cell l' e v ts) (rangeToIndices range)
      handleEval user state (PayloadCL cells)
    Copy -> do 
      let PayloadPaste from to = lastPayload
      handleCopy user state (PayloadPaste from range)
    Delete -> handleDelete user state (PayloadR range)
    Undo -> handleRedo user state
    otherwise -> sendToOriginal user $ ServerMessage Repeat (Failure "Repeat not supported for this action") (PayloadN ())

----------------------------------------------------------------------------------------------------------------------------------------------
-- Repeat handlers

recordMessage :: R.Connection -> ASClientMessage -> IO () 
recordMessage conn msg = case (clientAction msg) of 
  Repeat -> return ()
  _ -> R.runRedis conn (R.set (B.pack "LASTMESSAGE") (B.pack $ show msg)) >> return ()

getLastMessage :: R.Connection -> IO ASClientMessage
getLastMessage conn = R.runRedis conn $ do 
  msg <- R.get (B.pack ("LASTMESSAGE"))
  return $ case msg of 
    Right (Just msg') -> read (B.unpack msg')
    _ -> ClientMessage NoAction (PayloadN ())