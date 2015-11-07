module AS.Clients where

import Control.Exception
import Prelude
import qualified Data.List as L
import qualified Data.Text as T
import Data.Maybe
import Data.Bits (xor)
import qualified Data.ByteString.Char8 as B
import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Success)
import qualified Network.WebSockets as WS
import qualified Database.Redis as R

import AS.Types.Core
import qualified AS.Types.DB as TD
import AS.DB.API                as DB
import AS.DB.Util               as DU
import AS.DB.Graph              as G
import AS.Util                  as U
import AS.Dispatch.Core         as DP
import AS.Dispatch.Repl         as DR
import AS.Users                 as US
import AS.Parsing.Substitutions as S
import AS.Daemon                as DM

import AS.Config.Settings as  CS

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-------------------------------------------------------------------------------------------------------------------------
-- ASUserClient is a client

instance Client ASUserClient where
  conn = userConn
  clientId = sessionId
  ownerName = userId
  clientSheetId (UserClient _ _ (Window sid _ _) _) = sid
  addClient uc s@(State ucs dcs dbc port)
    | uc `elem` ucs = s
    | otherwise = State (uc:ucs) dcs dbc port
  removeClient uc s@(State ucs dcs dbc port)
    | uc `elem` ucs = State (L.delete uc ucs) dcs dbc port
    | otherwise = s
  clientCommitSource (UserClient uid _ (Window sid _ _) _) = (sid, uid)
  handleClientMessage user state message = do 
    printWithTime ("\n\nMessage: " ++ (show message))
    -- second arg is supposed to be sheet id; temporary hack is to always set userId = sheetId
    -- on frontend. 
    writeToLog (show message) (clientCommitSource user)
    redisConn <- dbConn <$> readMVar state
    recordMessage redisConn message (clientCommitSource user)
    case (clientAction message) of
      Acknowledge  -> handleAcknowledge user
      New          -> handleNew user state payload
      Open         -> handleOpen user state payload
      Close        -> handleClose user state payload
      UpdateWindow -> handleUpdateWindow (sessionId user) state payload
      Import       -> handleImport state payload
      Evaluate     -> handleEval user state payload
      EvaluateRepl -> handleEvalRepl user state payload
      Get          -> handleGet user state payload
      Delete       -> handleDelete user state payload
      Clear        -> handleClear user state payload
      Undo         -> handleClear user state (PayloadS (Sheet "SHEET_NAME" "SDf" (Blacklist [])))
      --Undo         -> handleUndo user state
      Redo         -> handleRedo user state
      Copy         -> handleCopy user state payload
      Cut          -> handleCut user state payload
      CopyForced   -> handleCopyForced user state payload
      AddTags      -> handleAddTags user state payload
      RemoveTags   -> handleRemoveTags user state payload
      Repeat       -> handleRepeat user state payload
      BugReport    -> handleBugReport user payload
      JumpSelect   -> handleJumpSelect user state payload
      where payload = clientPayload message
    -- ^^ above is to test streaming when frontend hasn't been implemented yet

-------------------------------------------------------------------------------------------------------------------------
-- ASDaemonClient is a client

instance Client ASDaemonClient where
  conn = daemonConn
  clientId = T.pack . DM.getDaemonName . daemonLoc
  ownerName = daemonOwner
  clientSheetId (DaemonClient (Index sid _ ) _ _) = sid
  addClient dc s@(State ucs dcs dbc port)
    | dc `elem` dcs = s
    | otherwise = State ucs (dc:dcs) dbc port
  removeClient dc s@(State ucs dcs dbc port)
    | dc `elem` dcs = State ucs (L.delete dc dcs) dbc port
    | otherwise = s
  clientCommitSource (DaemonClient (Index sid _ ) _ uid) = (sid, uid)
  handleClientMessage daemon state message = case (clientAction message) of
    Evaluate -> handleEval daemon state (clientPayload message)

-- Handlers take message payloads and send the response to the client(s)


----------------------------------------------------------------------------------------------------------------------------------------------
-- Sending message to user client(s)

broadcast :: ASSheetId -> MVar ServerState -> ASServerMessage -> IO ()
broadcast sid state message = do
  (State ucs _ _ _) <- readMVar state
  let ucsOnSheet = filter (\uc -> clientSheetId uc == sid) ucs
  forM_ ucsOnSheet $ \(UserClient _ conn _ _) -> U.sendMessage message conn

-- | Figures out whom to send the message back to, based on the payload, and broadcasts the message
-- to all the relevant recipients. 
reply :: (Client c) => c -> MVar ServerState -> ASServerMessage -> IO ()
reply cl state msg@(ServerMessage _ (Failure e) _) = sendToOriginal cl msg                        -- send error to original user only
reply cl state msg@(ServerMessage _ _ (PayloadCommit _)) = broadcast (clientSheetId cl) state msg -- broadcast all undo/redos (scrolling only refreshes non-undone cells)
reply cl state msg@(ServerMessage Clear _ _) = broadcast (clientSheetId cl) state msg             -- broadcast all clears for the same reason
reply cl state msg@(ServerMessage Delete _ _) = broadcast (clientSheetId cl) state msg
reply _  state msg = liftIO $ do
  (State ucs _ _ _) <- readMVar state
  broadcastFiltered msg ucs

-- | Given a message (commit, cells, etc), only send (to each user) the cells in their viewing window
broadcastFiltered :: ASServerMessage -> [ASUserClient] -> IO ()
broadcastFiltered msg@(ServerMessage a r (PayloadCL cells)) users = mapM_ (sendCells cells) users
  where
    sendCells :: [ASCell] -> ASUserClient -> IO ()
    sendCells cells user = do
      let cells' = intersectViewingWindow cells (userWindow user)
      case cells' of
        [] -> return ()
        _ -> U.sendMessage (ServerMessage a r (PayloadCL cells')) (userConn user)

broadcastFiltered msg@(ServerMessage a r (PayloadLL locs)) users = mapM_ (sendLocs locs) users
  where
    sendLocs :: [ASIndex] -> ASUserClient -> IO ()
    sendLocs locs user = do
      let locs' = intersectViewingWindowLocs locs (userWindow user)
      case locs' of
        [] -> return ()
        _ -> U.sendMessage (ServerMessage a r (PayloadLL locs')) (userConn user)

sendToOriginal :: (Client c) => c -> ASServerMessage -> IO ()
sendToOriginal cl msg = U.sendMessage msg (conn cl)


----------------------------------------------------------------------------------------------------------------------------------------------
-- Open/close/import/new/window handlers

handleAcknowledge :: ASUserClient -> IO ()
handleAcknowledge user = WS.sendTextData (userConn user) ("ACK" :: T.Text)

handleNew :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleNew user state (PayloadWorkbookSheets (wbs:[])) = do
  conn <- dbConn <$> readMVar state
  wbs' <- DB.createWorkbookSheet conn wbs
  broadcast (clientSheetId user) state $ ServerMessage New Success (PayloadWorkbookSheets [wbs'])
handleNew user state (PayloadWB wb) = do
  conn <- dbConn <$> readMVar state
  wb' <- DB.createWorkbook conn (workbookSheets wb)
  broadcast (clientSheetId user) state $ ServerMessage New Success (PayloadWB wb')
  return () -- TODO determine whether users should be notified

handleOpen :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleOpen user state (PayloadS (Sheet sheetid _ _)) = US.modifyUser makeNewWindow user state
  where makeNewWindow (UserClient uid conn _ sid) = UserClient uid conn startWindow sid
        startWindow = Window sheetid (-1,-1) (-1,-1)

-- Had relevance back when UserClients could have multiple windows, which never made sense anyway. 
-- (Alex 11/3)
handleClose :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleClose _ _ _ = return ()
-- handleClose user state (PayloadS (Sheet sheetid _ _)) = US.modifyUser closeWindow user state
--   where closeWindow (UserClient uid conn window sid) = UserClient uid conn (filter (((/=) sheetid) . windowSheetId) windows) sid

handleUpdateWindow :: ClientId -> MVar ServerState -> ASPayload -> IO ()
handleUpdateWindow sid state (PayloadW w) = do
  curState <- readMVar state
  let (Just user') = US.getUserByClientId sid curState -- user' is to get latest user on server; if this fails then somehow your connection isn't stored in the state
  let oldWindow = userWindow user'
  (flip catch) (badCellsHandler (dbConn curState) user') (do
    let newLocs = U.getScrolledLocs oldWindow w
    printObj "Sending newLocs" newLocs
    mcells <- DB.getCells $ concat $ map rangeToIndices newLocs
    sendToOriginal user' $ U.makeUpdateWindowMessage (catMaybes mcells)
    US.modifyUser (U.updateWindow w) user' state)

-- | If a message is failing to parse from the server, undo the last commit (the one that added
-- the message to the server.) I doubt this fix is completely foolproof, but it keeps data
-- from getting lost and doesn't require us to manually reset the server. 
badCellsHandler :: R.Connection -> ASUserClient -> SomeException -> IO ()
badCellsHandler conn user e = do 
  U.writeErrToLog ("Error while fetching cells: " ++ (show e)) (clientCommitSource user)
  printWithTime "Undoing last commit"
  DB.undo conn (clientCommitSource user)
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
  msg' <- DP.runDispatchCycle state cells (clientCommitSource cl)
  reply cl state msg'

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
  sendToOriginal user (U.makeGetMessage $ catMaybes mcells)
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
  broadcast (clientSheetId user) state $ ServerMessage Delete Success p
  return ()
handleDelete user state p@(PayloadWB workbook) = do
  conn <- dbConn <$> readMVar state
  DB.deleteWorkbook conn (workbookName workbook)
  reply user state $ ServerMessage Delete Success p
  return ()
handleDelete user state payload = do
  let locs = case payload of
               PayloadLL locs' -> locs'
               PayloadR rng -> rangeToIndices rng
  conn <- dbConn <$> readMVar state
  let blankedCells = U.blankCellsAt locs
  msg <- DP.runDispatchCycle state blankedCells (clientCommitSource user)
  reply user state $ ServerMessage Delete Success payload

handleClear :: (Client c) => c  -> MVar ServerState -> ASPayload -> IO ()
handleClear client state payload = case payload of 
  (PayloadN ()) -> do
    conn <- dbConn <$> readMVar state
    DB.clear conn
    G.exec_ TD.Clear
    reply client state $ ServerMessage Clear Success $ PayloadN ()
  (PayloadS (Sheet sid _ _)) -> do
    DB.clearSheet sid 
    G.exec_ TD.Recompute
    reply client state $ ServerMessage Clear Success payload

handleUndo :: ASUserClient -> MVar ServerState -> IO ()
handleUndo user state = do
  conn <- dbConn <$> readMVar state
  commit <- DB.undo conn (clientCommitSource user)
  msg <- case commit of
    Nothing -> return $ failureMessage "Too far back"
    (Just c) -> return $ ServerMessage Undo Success (PayloadCommit c)
  reply user state msg
  printWithTime "Server processed undo"

handleRedo :: ASUserClient -> MVar ServerState -> IO ()
handleRedo user state = do
  conn <- dbConn <$> readMVar state
  commit <- DB.redo conn (clientCommitSource user)
  msg <- case commit of
    Nothing -> return $ failureMessage "Too far forwards"
    (Just c) -> return $ ServerMessage Redo Success (PayloadCommit c)
  reply user state msg
  printWithTime "Server processed redo"

handleCopy :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleCopy user state (PayloadPaste from to) = do
  conn <- dbConn <$> readMVar state
  toCells <- getPasteCells conn from to
  msg' <- DP.runDispatchCycle state toCells (clientCommitSource user)
  reply user state msg'

handleCut :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleCut user state (PayloadPaste from to) = do
  conn <- dbConn <$> readMVar state
  toCells <- getPasteCells conn from to
  let blankedCells = U.blankCellsAt (rangeToIndices from)
      newCells = U.mergeCells toCells blankedCells -- content in pasted cells take precedence over deleted cells
  msg' <- DP.runDispatchCycle state newCells (clientCommitSource user)
  reply user state msg'

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
  ClientMessage lastAction lastPayload <- getLastMessage conn (clientCommitSource user)
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

recordMessage :: R.Connection -> ASClientMessage -> CommitSource -> IO () 
recordMessage conn msg src = case (clientAction msg) of 
  Repeat -> return ()
  _ -> R.runRedis conn (R.set (B.pack ("LASTMESSAGE" ++ show src)) (B.pack $ show msg)) >> return ()

getLastMessage :: R.Connection -> CommitSource -> IO ASClientMessage
getLastMessage conn src = R.runRedis conn $ do 
  msg <- R.get (B.pack ("LASTMESSAGE" ++ show src))
  return $ case msg of 
    Right (Just msg') -> read (B.unpack msg')
    _ -> ClientMessage NoAction (PayloadN ())

-- | For now, all this does is acknowledge that a bug report got sent. The actual contents
-- of the bug report (part of the payload) are output to the server log in handleClientMessage, 
-- which is where we want it end up anyway, for now. (Alex 10/28/15)
handleBugReport :: ASUserClient -> ASPayload -> IO ()
handleBugReport user (PayloadText report) = do 
  U.logBugReport report (clientCommitSource user)
  WS.sendTextData (userConn user) ("ACK" :: T.Text)

----------------------------------------------------------------------------------------------------------------------------------------------
-- JumpSelect handlers

-- #incomplete the logic here is actually wrong. Also isn't hooked up to anything on frontend yet. 
handleJumpSelect :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleJumpSelect user state p@(PayloadJump sel origin shifted dir) = 
  let 
    (Range sid (tl, br)) = sel
    (Index _ orig) = origin
    isOrientedH = (col orig) == (col tl)
    isOrientedV = (row orig) == (row tl)
    hExtremum = if isOrientedH then (col br) else (col tl)
    vExtremum = if isOrientedV then (row br) else (row tl)

    jumpSelect :: R.Connection -> [Int] -> Bool -> IO (Maybe ASReference)
    jumpSelect _ [] _ = return Nothing
    jumpSelect conn (iter:iters) startExists = case dir of
      DRight -> do
        thisExists <- DB.locationExists conn (Index sid (iter,row orig))
        let idx = if thisExists then (iter,row tl) else (iter-1,row tl)
            resultCol = if isOrientedH then (col tl) else (col idx)
            resultCol2 = if isOrientedH then (col idx) else (col br)
            result = if shifted
              then Just . RangeRef . orientRange $ Range sid ((resultCol,row tl), (resultCol2,row br))
              else Just . IndexRef $ Index sid idx
        if (xor startExists thisExists) 
          then return result 
          else jumpSelect conn iters startExists
              
      DDown -> do
        thisExists <- DB.locationExists conn (Index sid (col orig,iter))
        let idx = if thisExists then (col tl,iter) else (col tl,iter-1)
            resultRow = if isOrientedV then (row tl) else (row idx)
            resultRow2 = if isOrientedV then (row idx) else (row br)
            result = if shifted
              then Just . RangeRef . orientRange $ Range sid ((col tl,resultRow), (col br,resultRow2))
              else Just . IndexRef $ Index sid idx
        if (xor startExists thisExists) 
          then return result 
          else jumpSelect conn iters startExists

      DLeft -> do
        thisExists <- DB.locationExists conn (Index sid (iter,row orig))
        let idx = if thisExists then (iter,row tl) else (iter+1,row tl)
            resultCol = if isOrientedH then (col tl) else (col idx)
            resultCol2 = if isOrientedH then (col idx) else (col br)
            result = if shifted
              then Just . RangeRef . orientRange $ Range sid ((resultCol,row tl), (resultCol2,row br))
              else Just . IndexRef $ Index sid idx
        if (xor startExists thisExists) 
          then return result 
          else jumpSelect conn iters startExists

      DUp -> do
        thisExists <- DB.locationExists conn (Index sid (col orig,iter))
        let idx = if thisExists then (col tl,iter) else (col tl,iter+1)
            resultRow = if isOrientedV then (row tl) else (row idx)
            resultRow2 = if isOrientedV then (row idx) else (row br)
            result = if shifted
              then Just . RangeRef . orientRange $ Range sid ((col tl,resultRow), (col br,resultRow2))
              else Just . IndexRef $ Index sid idx
        if (xor startExists thisExists) 
          then return result 
          else jumpSelect conn iters startExists
              
  in do
    conn <- dbConn <$> readMVar state
    newSel <- case dir of 
      DRight -> do
        nextExists <- DB.locationExists conn (Index sid (hExtremum+1,row tl))
        let startCol = if nextExists then hExtremum else (hExtremum + 1)
        startExists <- DB.locationExists conn (Index sid (startCol,row tl))
        let iterCols = [startCol..CS.largeSearchBound]
        maybeNewSel <- jumpSelect conn iterCols startExists
        return $ case maybeNewSel of 
          Nothing -> RangeRef sel
          (Just newSel) -> newSel 
      DDown -> do
        nextExists <- DB.locationExists conn (Index sid (col tl,vExtremum+1))
        let startRow = if nextExists then vExtremum else (vExtremum + 1)
        startExists <- DB.locationExists conn (Index sid (col tl,startRow))
        let iterRows = [startRow..CS.largeSearchBound]
        maybeNewSel <- jumpSelect conn iterRows startExists
        return $ case maybeNewSel of 
          Nothing -> RangeRef sel
          (Just newSel) -> newSel 
      DLeft -> do
        nextExists <- DB.locationExists conn (Index sid (hExtremum-1,row tl))
        let startCol = if nextExists then hExtremum else (hExtremum - 1)
        startExists <- DB.locationExists conn (Index sid (startCol,row tl))
        let iterCols = reverse [1..startCol]
        maybeNewSel <- jumpSelect conn iterCols startExists
        return $ case maybeNewSel of 
          Nothing -> RangeRef $ Range sid ((1,row tl), (col2,row2))
            where
              col2 = if shifted then (if isOrientedH then (col tl) else (col br)) else 1
              row2 = if shifted then (row br) else (row tl)
          (Just newSel) -> newSel 
      DUp -> do
        nextExists <- DB.locationExists conn (Index sid (col tl,vExtremum-1))
        let startRow = if nextExists then vExtremum else (vExtremum - 1)
        startExists <- DB.locationExists conn (Index sid (col tl,startRow))
        let iterRows = reverse [1..startRow]
        maybeNewSel <- jumpSelect conn iterRows startExists
        return $ case maybeNewSel of 
          Nothing -> RangeRef $ Range sid ((col tl,1), (col2,row2))
            where
              col2 = if shifted then (col br) else (col tl)
              row2 = if shifted then (if isOrientedV then (row tl) else (row br)) else 1
          (Just newSel) -> newSel 
    let (newSel', newOrigin) = case newSel of 
                              RangeRef r@(Range _ _) -> (r, origin)
                              IndexRef i@(Index _ ind) -> (Range sid (ind,ind), i)
    sendToOriginal user $ ServerMessage JumpSelect Success (PayloadSelection newSel' newOrigin)