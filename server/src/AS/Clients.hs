{-# LANGUAGE OverloadedStrings #-}
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
import AS.Types.Excel hiding (row, col)
import qualified AS.Types.DB as TD
import AS.DB.API                as DB
import AS.DB.Transaction        as DT
import AS.DB.Util               as DU
import AS.DB.Graph              as G
import AS.Util                  as U
import AS.Kernels.LanguageUtils as LU
import AS.Dispatch.Core         as DP
import AS.Dispatch.Repl         as DR
import AS.Dispatch.EvalHeader   as DEH
import AS.Users                 as US
import AS.Parsing.Substitutions as S
import AS.Daemon                as DM
import AS.Parsing.Excel (exRefToASRef, asRefToExRef, refMatch)

import AS.Config.Settings as  CS
import qualified AS.InferenceUtils as IU

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
  addClient uc s@(State ucs dcs dbc port tc)
    | uc `elem` ucs = s
    | otherwise = State (uc:ucs) dcs dbc port tc
  removeClient uc s@(State ucs dcs dbc port tc)
    | uc `elem` ucs = State (L.delete uc ucs) dcs dbc port tc
    | otherwise = s
  clientCommitSource (UserClient uid _ (Window sid _ _) _) = (sid, uid)
  handleClientMessage user state message = do 
    -- second arg is supposed to be sheet id; temporary hack is to always set userId = sheetId
    -- on frontend. 
    unless (clientAction message == Acknowledge) $ do 
      writeToLog (show message) (clientCommitSource user)
      putStrLn "=========================================================="
      printObj "Message" (show message)
    redisConn <- dbConn <$> readMVar state
    recordMessage redisConn message (clientCommitSource user)
    case (clientAction message) of
      Acknowledge    -> handleAcknowledge user
      New            -> handleNew user state payload
      Open           -> handleOpen user state payload
      Close          -> handleClose user state payload
      UpdateWindow   -> handleUpdateWindow (sessionId user) state payload
      Import         -> handleImport state payload
      Evaluate       -> handleEval user state payload
      EvaluateRepl   -> handleEvalRepl user payload
      EvaluateHeader -> handleEvalHeader user payload
      Get            -> handleGet user state payload
      Delete         -> handleDelete user state payload
      Clear          -> handleClear user state payload
      Undo           -> handleUndo user state
      Redo           -> handleRedo user state
      Copy           -> handleCopy user state payload
      Cut            -> handleCut user state payload
      ToggleTag      -> handleToggleTag user state payload
      SetTag         -> handleSetTag user state payload
      Repeat         -> handleRepeat user state payload
      BugReport      -> handleBugReport user payload
      JumpSelect     -> handleJumpSelect user state payload
      MutateSheet    -> handleMutateSheet user state payload
      Drag           -> handleDrag user state payload
      Decouple       -> handleDecouple user state payload
      where payload = clientPayload message
      -- Undo         -> handleToggleTag user state (PayloadTags [StreamTag (Stream NoSource 1000)] (Index (T.pack "TEST_SHEET_ID2") (1,1)))
      -- ^^ above is to test streaming when frontend hasn't been implemented yet

-------------------------------------------------------------------------------------------------------------------------
-- ASDaemonClient is a client

instance Client ASDaemonClient where
  conn = daemonConn
  clientId = T.pack . DM.getDaemonName . daemonLoc
  ownerName = daemonOwner
  clientSheetId (DaemonClient (Index sid _ ) _ _) = sid
  addClient dc s@(State ucs dcs dbc port tc)
    | dc `elem` dcs = s
    | otherwise = State ucs (dc:dcs) dbc port tc
  removeClient dc s@(State ucs dcs dbc port tc)
    | dc `elem` dcs = State ucs (L.delete dc dcs) dbc port tc
    | otherwise = s
  clientCommitSource (DaemonClient (Index sid _ ) _ uid) = (sid, uid)
  handleClientMessage daemon state message = case (clientAction message) of
    Evaluate -> handleEval daemon state (clientPayload message)

-- Handlers take message payloads and send the response to the client(s)


----------------------------------------------------------------------------------------------------------------------------------------------
-- Sending message to user client(s)

broadcast :: MVar ServerState -> ASServerMessage -> IO ()
broadcast state message = do
  ucs <- userClients <$> readMVar state
  let ucsSheetIds = zip ucs (map clientSheetId ucs)
      affectedUsers = map fst $ filter (\(_, sid) ->  sid `elem` sheetsInPayload (serverPayload message)) ucsSheetIds
  forM_ affectedUsers $ \(UserClient _ conn _ _) -> U.sendMessage message conn

-- | Returns all the sheets referenced in a payload. Currently no support for PayloadWorkbookSheets
-- and PayloadWB, because those payloads suck. 
sheetsInPayload :: ASPayload -> [ASSheetId]
sheetsInPayload (PayloadDelete rng cells) = (rangeSheetId rng):(map (locSheetId . cellLocation) cells)
sheetsInPayload (PayloadS (Sheet sid _ _)) = [sid]
sheetsInPayload (PayloadCommit (Commit bf af _ _ _)) = (map (locSheetId . cellLocation) bf) ++ (map (locSheetId . cellLocation) af)
sheetsInPayload (PayloadN ()) = []

-- | Figures out whom to send the message back to, based on the payload, and broadcasts the message
-- to all the relevant recipients. 
-- Shouldn't this logic be dealt with by the individual handlers? (Alex 11/13)
reply :: (Client c) => c -> MVar ServerState -> ASServerMessage -> IO ()
reply cl state msg@(ServerMessage _ (Failure e) _) = sendToOriginal cl msg
reply cl state msg@(ServerMessage _ (DecoupleDuringEval) _) = sendToOriginal cl msg
reply _ state msg@(ServerMessage _ _ (PayloadCommit _)) = broadcast state msg 
reply _ state msg@(ServerMessage Clear _ _) = broadcast state msg 
reply _ state msg@(ServerMessage Delete _ _) = broadcast state msg  -- don't filter broadcast, since scrolling only updates non-blank cells. (See comment in handleUpdateWindow)
reply _  state msg = liftIO $ do
  ucs <- userClients <$> readMVar state
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
  reply user state $ ServerMessage New Success (PayloadWorkbookSheets [wbs'])
handleNew user state (PayloadWB wb) = do
  conn <- dbConn <$> readMVar state
  wb' <- DB.createWorkbook conn (workbookSheets wb)
  reply user state $ ServerMessage New Success (PayloadWB wb')
  return () -- TODO determine whether users should be notified

handleOpen :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleOpen user state (PayloadS (Sheet sheetid _ _)) = do 
  -- update state
  let makeNewWindow (UserClient uid conn _ sid) = UserClient uid conn startWindow sid
      startWindow = Window sheetid (-1,-1) (-1,-1)
  US.modifyUser makeNewWindow user state
  -- send back header files data to user
  let langs = [Python, R] -- should probably make list of langs a const somewhere...
      sid = clientSheetId user
  headers <- mapM (LU.getLanguageHeader sid) langs
  let xps = map (\(str, lang) -> Expression str lang) (zip headers langs)
  sendToOriginal user $ ServerMessage Open Success (PayloadXpL xps)

-- Had relevance back when UserClients could have multiple windows, which never made sense anyway. 
-- (Alex 11/3)
handleClose :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleClose _ _ _ = return ()
-- handleClose user state (PayloadS (Sheet sheetid _ _)) = US.modifyUser closeWindow user state
--   where closeWindow (UserClient uid conn window sid) = UserClient uid conn (filter (((/=) sheetid) . windowSheetId) windows) sid

-- NOTE: doesn't send back blank cells. This means that if, e.g., there are cells that got blanked
-- in the database, those blank cells will not get passed to the user (and those cells don't get
-- deleted on frontend), meaning we have to ensure that deleted cells are manually wiped from the 
-- frontend store the moment they get deleted. 
handleUpdateWindow :: ClientId -> MVar ServerState -> ASPayload -> IO ()
handleUpdateWindow cid state (PayloadW w) = do
  curState <- readMVar state
  let (Just user') = US.getUserByClientId cid curState -- user' is to get latest user on server; if this fails then somehow your connection isn't stored in the state
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
  DT.undo conn (clientCommitSource user)
  return ()

handleImport :: MVar ServerState -> ASPayload -> IO ()
handleImport state msg = return () -- TODO

----------------------------------------------------------------------------------------------------------------------------------------------
-- Eval handler

handleEval :: (Client c) => c -> MVar ServerState -> ASPayload -> IO ()
handleEval cl state payload  = do
  let cells = case payload of 
                PayloadCL cells' -> cells'
  -- The PayloadCL is sort of a misnomer; it's only really being used as a wrapper around the
  -- expression to evaluate and the location of evaluation. In particular, the value passed in the cells
  -- are irrelevant, and there are no tags passed in, so we have to get the tags from the database
  -- manually. 
  oldTags <- DB.getTagsAt (map cellLocation cells)
  let cells' = map (\(c, ts) -> c { cellTags = ts }) (zip cells oldTags)
  msg' <- DP.runDispatchCycle state cells' (clientCommitSource cl)
  reply cl state msg'

handleDecouple :: (Client c) => c -> MVar ServerState -> ASPayload -> IO ()
handleDecouple cl state payload = do 
  commit <- tempCommit <$> readMVar state
  conn <- dbConn <$> readMVar state
  DT.updateDBAfterEval conn (clientCommitSource cl) commit

handleEvalRepl :: (Client c) => c -> ASPayload -> IO ()
handleEvalRepl cl (PayloadXp xp) = do
  let sid = clientSheetId cl
  msg' <- DR.runReplDispatch sid xp
  sendToOriginal cl msg'

handleEvalHeader :: (Client c) => c -> ASPayload -> IO ()
handleEvalHeader cl (PayloadXp xp) = do
  let sid = clientSheetId cl
  msg' <- DEH.runEvalHeader sid xp
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
  reply user state $ ServerMessage Delete Success p
  return ()
handleDelete user state p@(PayloadWB workbook) = do
  conn <- dbConn <$> readMVar state
  DB.deleteWorkbook conn (workbookName workbook)
  reply user state $ ServerMessage Delete Success p
  return ()
handleDelete user state (PayloadR rng) = do
  let locs = rangeToIndices rng
  conn <- dbConn <$> readMVar state
  blankedCells <- DB.getBlankedCellsAt locs
  updateMsg <- DP.runDispatchCycle state blankedCells (clientCommitSource user)
  reply user state $ U.makeDeleteMessage rng updateMsg

handleClear :: (Client c) => c  -> MVar ServerState -> ASPayload -> IO ()
handleClear client state payload = case payload of 
  (PayloadN ()) -> do
    conn <- dbConn <$> readMVar state
    DB.clear conn
    G.exec_ TD.Clear
    reply client state $ ServerMessage Clear Success $ PayloadN ()
  (PayloadS (Sheet sid _ _)) -> do
    conn <- dbConn <$> readMVar state
    DB.clearSheet conn sid 
    G.exec_ TD.Recompute
    reply client state $ ServerMessage Clear Success payload

handleUndo :: ASUserClient -> MVar ServerState -> IO ()
handleUndo user state = do
  conn <- dbConn <$> readMVar state
  commit <- DT.undo conn (clientCommitSource user)
  msg <- case commit of
    Nothing -> return $ failureMessage "Too far back"
    (Just c) -> return $ ServerMessage Undo Success (PayloadCommit c)
  reply user state msg
  printWithTime "Server processed undo"

handleRedo :: ASUserClient -> MVar ServerState -> IO ()
handleRedo user state = do
  conn <- dbConn <$> readMVar state
  commit <- DT.redo conn (clientCommitSource user)
  msg <- case commit of
    Nothing -> return $ failureMessage "Too far forwards"
    (Just c) -> return $ ServerMessage Redo Success (PayloadCommit c)
  reply user state msg
  printWithTime "Server processed redo"

handleCopy :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleCopy user state (PayloadPaste from to) = do
  conn <- dbConn <$> readMVar state
  toCells <- getCopyCells conn from to
  msg' <- DP.runDispatchCycle state toCells (clientCommitSource user)
  reply user state msg'

handleCut :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleCut user state (PayloadPaste from to) = do
  conn <- dbConn <$> readMVar state
  newCells <- getCutCells conn from to
  msg' <- DP.runDispatchCycle state newCells (clientCommitSource user)
  reply user state msg'

----------------------------------------------------------------------------------------------------------------------------------------------
-- Drag/autofill

handleDrag :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleDrag user state (PayloadDrag selRng dragRng) = do 
  conn <- dbConn <$> readMVar state
  nCells <- IU.getCellsRect selRng dragRng
  let newCells = (IU.getMappedFormulaCells selRng dragRng nCells) ++ (IU.getMappedPatternGroups selRng dragRng nCells)
  msg' <- DP.runDispatchCycle state newCells (clientCommitSource user)
  reply user state msg'


----------------------------------------------------------------------------------------------------------------------------------------------
-- Copy/cut/paste helpers

-- | Gets you the new cells to eval after shifting from a copy/paste. 
getCopyCells :: R.Connection -> ASRange -> ASRange -> IO [ASCell]
getCopyCells conn from to = do 
  fromCells          <- DB.getPossiblyBlankCells (rangeToIndices from)
  sanitizedFromCells <- sanitizeCopyCells conn fromCells from
  let offsets       = U.getCopyOffSets from to  -- how much to shift these cells for copy/copy/paste
      toCells       = concat $ map (\o -> map (S.shiftCell o) sanitizedFromCells) offsets
      updateSheetId = \l -> l { locSheetId = rangeSheetId to }
      toCells'      = map (replaceCellLocs updateSheetId) toCells
  return toCells'

getCutCells :: R.Connection -> ASRange -> ASRange -> IO [ASCell]
getCutCells conn from to = do 
  let offset = U.getRangeOffset from to
  toCells      <- getCutToCells conn from offset
  newDescCells <- getCutNewDescCells from offset
  let blankedCells = U.blankCellsAt (rangeToIndices from) -- want to forget about tags
  -- precedence: toCells > updated descendant cells > blank cells
  return $ U.mergeCells toCells (U.mergeCells newDescCells blankedCells)

-- | Constructs the cells at the locations you'll be pasting to
getCutToCells :: R.Connection -> ASRange -> Offset -> IO [ASCell]
getCutToCells conn from offset = do 
  fromCells          <- DB.getPossiblyBlankCells (rangeToIndices from)
  sanitizedFromCells <- sanitizeCutCells conn fromCells from
  let shiftLoc    = shiftInd offset
      changeExpr  = S.shiftExpressionForCut from offset
  return $ map ((replaceCellLocs shiftLoc) . (replaceCellExpressions changeExpr)) sanitizedFromCells

-- | Returns the cells that reference the cut cells with their expressions updated. 
getCutNewDescCells :: ASRange -> Offset -> IO [ASCell]
getCutNewDescCells from offset = do 
  immDescLocs <- getImmediateDescendantsForced (rangeToIndices from)
  let immDescLocs' = filter (not . (rangeContainsIndex from)) immDescLocs
      changeExpr   = S.shiftExpressionForCut from offset
  descs <- catMaybes <$> DB.getCells immDescLocs'
  return $ map (replaceCellExpressions changeExpr) descs

-- | Decouples cells appropriately for re-eval on cut/paste, as follows:
--   * if a cell is not a part of a list, leave it as is. 
--   * if an entire list is contained in the range, keep just the head of the list. (So on eval
--     the entire list is re-evaluated)
--   * if a cell is part of a list that is not contained entirely in the selection, decouple it. 
sanitizeCutCells :: R.Connection -> [ASCell] -> ASRange -> IO [ASCell]
sanitizeCutCells conn cells from = do 
  keys <- fatCellsInRange conn from
  let (fatCellMembers, regularCells)  = L.partition DU.isFatCellMember cells
      (containedCells, cutoffCells)   = U.partitionByRangeKey fatCellMembers keys
      decoupledCells                  = map decoupleCell cutoffCells
      containedFatCellHeads           = filter DU.isFatCellHead containedCells
      containedFatCellHeadsUncoupled  = map DU.toUncoupled containedFatCellHeads
  return $ regularCells ++ decoupledCells ++ containedFatCellHeadsUncoupled

-- Same as above, except if everything is a list head, leave it as is. 
sanitizeCopyCells :: R.Connection -> [ASCell] -> ASRange -> IO [ASCell]
sanitizeCopyCells conn cells from
  | all DU.isFatCellHead cells = return $ map DU.toUncoupled cells 
  | otherwise = sanitizeCutCells conn cells from 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Tag handlers
handleToggleTag :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleToggleTag user state (PayloadTag t rng) = do
  let locs = rangeToIndices rng
  cells <- DB.getPossiblyBlankCells locs
  let (cellsWithTag, cellsWithoutTag) = L.partition (\c -> t `elem` cellTags c) cells
  -- if there's a single tag present in the range, remove this tag from all the cells; 
  -- otherwise set the tag in all the cells. 
  if (null cellsWithoutTag)
    then do 
      let cells' = map (\(Cell l e v ts) -> Cell l e v (L.delete t ts)) cellsWithTag
          (emptyCells, nonEmptyCells) = L.partition U.isEmptyCell cells'
      DB.setCells nonEmptyCells
      conn <- dbConn <$> readMVar state
      DB.deleteCells conn emptyCells
      mapM_ (removeTagEndware state t) nonEmptyCells
      reply user state $ ServerMessage Update Success (PayloadCL cells')
    else do
      let cells' = map (\(Cell l e v ts) -> Cell l e v (t:ts)) cellsWithoutTag
      DB.setCells cells'
      mapM_ (addTagEndware state t) cells'
      reply user state $ ServerMessage Update Success (PayloadCL cells')
    -- don't HAVE to send back the entire cells, but that's an optimization for a later time. 
    -- Said toad. (Alex 11/7)

addTagEndware :: MVar ServerState -> ASCellTag -> ASCell -> IO ()
addTagEndware state (StreamTag s) c = DM.modifyDaemon state s (cellLocation c) evalMsg
  where evalMsg = ClientMessage Evaluate (PayloadCL [c])
addTagEndware _ _ _ = return ()

removeTagEndware :: MVar ServerState -> ASCellTag -> ASCell -> IO ()
removeTagEndware state (StreamTag s) c = DM.removeDaemon (cellLocation c) state
removeTagEndware _ _ _ = return ()

-- Should refactor to props instead of tags for non-Bools. (Alex 11/11)
handleSetTag :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleSetTag user state (PayloadTag tag rng) = do
  curState <- readMVar state
  let locs = rangeToIndices rng
  cells <- DB.getPossiblyBlankCells locs
  cells' <- (flip mapM cells) $ \(Cell l e v ts) -> do 
        let ts' = filter (U.differentTagType tag) ts
            c'  = Cell l e v (tag:ts')
        if (ts' /= ts) -- if you ended up removing an old version of the tag
          then (removeTagEndware state tag c')
          else (addTagEndware state tag c')
        return c'
  DB.setCells cells'
  reply user state $ ServerMessage Update Success (PayloadCL cells')

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

handleMutateSheet :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleMutateSheet user state (PayloadMutate mutateType) = do 
  allCells <- DB.getCellsInSheet (clientSheetId user)
  conn <- dbConn <$> readMVar state
  let newCells = map (cellMap mutateType) allCells
      oldCellsNewCells = zip allCells newCells
      oldCellsNewCells' = filter (\(c, c') -> (isNothing c') || (c /= fromJust c')) oldCellsNewCells
      -- ^ get rid of cells that haven't changed.
      oldCells' = map fst oldCellsNewCells
      blankedCells = U.blankCellsAt (map cellLocation oldCells')
      newCells' = catMaybes $ map snd oldCellsNewCells
      updatedCells   = U.mergeCells newCells' blankedCells -- eval blanks at the old cell locations, re-eval at new locs
  updateMsg <- DP.runDispatchCycle state updatedCells (clientCommitSource user)
  reply user state updateMsg


cellLocMap :: MutateType -> (ASIndex -> Maybe ASIndex)
cellLocMap (InsertCol c') (Index sid (c, r)) = Just $ Index sid (if c >= c' then c+1 else c, r)
cellLocMap (InsertRow r') (Index sid (c, r)) = Just $ Index sid (c, if r >= r' then r+1 else r)
cellLocMap (DeleteCol c') i@(Index sid (c, r))
  | c == c'  = Nothing
  | c > c'   = Just $ Index sid (c-1, r)
  | c < c'   = Just i
cellLocMap (DeleteRow r') i@(Index sid (c, r))
  | r == r'  = Nothing
  | r > r'   = Just $ Index sid (c, r-1)
  | r < r'   = Just i
cellLocMap (DragCol oldC newC) i@(Index sid (c, r)) -- DragCol 3 1 : (123) -> (312)
  | c < min oldC newC = Just i
  | c > max oldC newC = Just i
  | c == oldC         = Just $ Index sid (newC, r) 
  | oldC < newC       = Just $ Index sid (c-1, r) -- here on we assume c is strictly between oldC and newC
  | oldC > newC       = Just $ Index sid (c+1, r)
  -- case oldC == newC can't happen because oldC < c < newC since third pattern-match
cellLocMap (DragRow oldR newR) i@(Index sid (c, r))
  | r < min oldR newR = Just i
  | r > max oldR newR = Just i
  | r == oldR         = Just $ Index sid (c, newR)
  | oldR < newR       = Just $ Index sid (c, r-1) -- here on we assume c is strictly between oldR and newR
  | oldR > newR       = Just $ Index sid (c, r+1)
  -- case oldR == newR can't happen because oldR < r < newR since third pattern-match

refMap :: MutateType -> (ExRef -> ExRef)
refMap mt ExOutOfBounds = ExOutOfBounds
refMap mt er@(ExLocRef (ExIndex rt _ _) ls lw) = er'
  where -- feels kinda ugly... 
    IndexRef ind = exRefToASRef (T.pack "") er
    er' = case (cellLocMap mt ind) of 
      Nothing -> ExOutOfBounds
      Just newRefLoc -> ExLocRef ei' ls lw
        where
          ExLocRef ei _ _ = asRefToExRef $ IndexRef newRefLoc
          ei' = ei { refType = rt }

refMap mt er@(ExRangeRef (ExRange f s) rs rw) = ExRangeRef (ExRange f' s') rs rw
  where 
    ExLocRef f' _ _ = refMap mt (ExLocRef f rs rw)
    ExLocRef s' _ _ = refMap mt (ExLocRef s rs rw)
refMap mt er@(ExPointerRef el ps pw) = ExPointerRef el' ps pw
  where 
    ExLocRef el' _ _ = refMap mt (ExLocRef el ps pw)

expressionMap :: MutateType -> (ASExpression -> ASExpression)
expressionMap mt = S.replaceRefs (show . (refMap mt))

cellMap :: MutateType -> (ASCell -> Maybe ASCell)
cellMap mt c@(Cell loc xp v ts) = case ((cellLocMap mt) loc) of 
  Nothing -> Nothing 
  Just loc' -> let c' = Cell loc' ((expressionMap mt) xp) v ts 
    in case xp of 
      Expression _ _ -> Just c'
      Coupled _ _ _ _ -> if DU.isFatCellHead c
        then Just $ DU.toUncoupled c' 
        else Just $ DU.decoupleCell c' 