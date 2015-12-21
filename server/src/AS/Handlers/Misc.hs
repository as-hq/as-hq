{-# LANGUAGE OverloadedStrings #-}
module AS.Handlers.Misc where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.DB hiding (Clear)
import AS.Types.Eval
import AS.Types.Commits
import AS.Types.CondFormat
import AS.Types.Bar
import qualified AS.Types.BarProps as BP

import AS.Handlers.Eval
import AS.Eval.CondFormat
import AS.Handlers.Paste

import AS.Window
import AS.Logging
import AS.Reply

import qualified AS.Dispatch.Core         as DP
import qualified AS.Util                  as U
import qualified AS.Kernels.LanguageUtils as LU
import qualified AS.Users                 as US
import qualified AS.InferenceUtils        as IU

import AS.DB.Eval
import qualified AS.DB.Transaction        as DT
import qualified AS.DB.API                as DB
import qualified AS.DB.Export             as DX
import qualified AS.DB.Graph              as G

import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as DS
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List
import Data.Maybe

import qualified Database.Redis as R
import qualified Network.WebSockets as WS

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans.Either

handleAcknowledge :: ASUserClient -> IO ()
handleAcknowledge uc = WS.sendTextData (userConn uc) ("ACK" :: T.Text)

handleNew :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleNew uc state (PayloadWorkbookSheets (wbs:[])) = do
  conn <- dbConn <$> readMVar state
  wbs' <- DB.createWorkbookSheet conn wbs
  broadcast state $ ServerMessage New Success (PayloadWorkbookSheets [wbs'])
handleNew uc state (PayloadWB wb) = do
  conn <- dbConn <$> readMVar state
  wb' <- DB.createWorkbook conn (workbookSheets wb)
  broadcast state $ ServerMessage New Success (PayloadWB wb')
  return () -- TODO determine whether users should be notified

handleOpen :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleOpen uc state (PayloadS (Sheet sid _ _)) = do
  -- update state
  conn <- dbConn <$> readMVar state
  let makeNewWindow (UserClient uid c _ sid) = UserClient uid c startWindow sid
      startWindow = Window sid (-1,-1) (-1,-1)
  US.modifyUser makeNewWindow uc state
  -- get header files data to send back to user user
  let langs = [Python, R] -- should probably make list of langs a const somewhere...
  headers         <- mapM (getEvalHeader conn sid) langs
  -- get conditional formatting data to send back to user user
  condFormatRules <- DB.getCondFormattingRules conn sid
  let xps = map (\(str, lang) -> Expression str lang) (zip headers langs)
  -- get column props
  barProps <- DB.getBarsInSheet conn sid
  sendToOriginal uc $ ServerMessage Open Success $ PayloadOpen xps condFormatRules barProps

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
    let newLocs = getScrolledLocs oldWindow w
    mcells <- DB.getCells (dbConn curState) $ concat $ map rangeToIndices newLocs
    sendToOriginal user' $ makeUpdateWindowMessage (catMaybes mcells)
    US.modifyUser (updateWindow w) user' state)

-- | If a message is failing to parse from the server, undo the last commit (the one that added
-- the message to the server.) I doubt this fix is completely foolproof, but it keeps data
-- from getting lost and doesn't require us to manually reset the server.
badCellsHandler :: R.Connection -> ASUserClient -> SomeException -> IO ()
badCellsHandler conn uc e = do
  logError ("Error while fetching cells: " ++ (show e)) (userCommitSource uc)
  printWithTime "Undoing last commit"
  DT.undo conn (userCommitSource uc)
  return ()

handleGet :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleGet uc state (PayloadLL locs) = do
  curState <- readMVar state
  mcells <- DB.getCells (dbConn curState) locs
  sendToOriginal uc (makeGetMessage $ catMaybes mcells)
handleGet uc state (PayloadList Sheets) = do
  curState <- readMVar state
  ss <- DB.getAllSheets (dbConn curState)
  sendToOriginal uc $ ServerMessage Update Success (PayloadSS ss)
handleGet uc state (PayloadList Workbooks) = do
  curState <- readMVar state
  ws <- DB.getAllWorkbooks (dbConn curState)
  sendToOriginal uc $ ServerMessage Update Success (PayloadWBS ws)
handleGet uc state (PayloadList WorkbookSheets) = do
  curState <- readMVar state
  wss <- DB.getAllWorkbookSheets (dbConn curState)
  printWithTime $ "getting all workbooks: "  ++ (show wss)
  sendToOriginal uc $ ServerMessage Update Success (PayloadWorkbookSheets wss)

handleDelete :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
-- these handlers are DEPRECATED
--handleDelete uc state p@(PayloadWorkbookSheets (wbs:[])) = do
--  conn <- dbConn <$> readMVar state
--  DB.deleteWorkbookSheet conn wbs
--  broadcast state $ ServerMessage Delete Success p
--  return ()
--handleDelete uc state p@(PayloadWB workbook) = do
--  conn <- dbConn <$> readMVar state
--  DB.deleteWorkbook conn (workbookName workbook)
--  broadcast state $ ServerMessage Delete Success p
--  return ()
handleDelete uc state (PayloadR rng) = do
  let locs = rangeToIndices rng
      badFormats = [ValueFormat Date]
      -- ^ Deleting a cell keeps some of the formats but deletes others. This is the current list of
      -- formats to remove upon deletion. 
  conn <- dbConn <$> readMVar state
  -- []
  blankedCells <- DB.getBlankedCellsAt conn locs -- need to know the formats at the old locations
  let removeBadFormat p c = if (hasProp p (cellProps c)) then removeCellProp (propType p) c else c
      -- ^ CellProp -> ASCell -> ASCell
      removeBadFormats ps = foldl' (.) id (map removeBadFormat ps)
      -- ^ [CellProp] -> ASCell -> ASCell
      blankedCells' = map (removeBadFormats badFormats) blankedCells
  updateMsg <- DP.runDispatchCycle state blankedCells' DescendantsWithParent (userCommitSource uc) id
  let msg = makeDeleteMessage rng updateMsg
  case (serverResult msg) of  
    Failure _ -> sendToOriginal uc msg
    DecoupleDuringEval -> sendToOriginal uc msg
    otherwise -> broadcast state msg

-- Had relevance back when UserClients could have multiple windows, which never made sense anyway.
-- (Alex 11/3)
handleClose :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleClose _ _ _ = return ()

handleClear :: (Client c) => c  -> MVar ServerState -> ASPayload -> IO ()
handleClear client state payload = case payload of 
  PayloadN () -> do
    conn <- dbConn <$> readMVar state
    DB.clear conn
    G.clear
    broadcast state $ ServerMessage Clear Success $ PayloadN ()
  PayloadS (Sheet sid _ _) -> do
    conn <- dbConn <$> readMVar state
    DB.clearSheet conn sid
    G.recompute conn
    broadcast state $ ServerMessage Clear Success payload

handleUndo :: ASUserClient -> MVar ServerState -> IO ()
handleUndo uc state = do
  conn <- dbConn <$> readMVar state
  commit <- DT.undo conn (userCommitSource uc)
  let msg = case commit of
              Nothing -> failureMessage "Too far back"
              Just c  -> ServerMessage Undo Success (PayloadCommit c)
  broadcast state msg
  printWithTime "Server processed undo"

handleRedo :: ASUserClient -> MVar ServerState -> IO ()
handleRedo uc state = do
  conn <- dbConn <$> readMVar state
  commit <- DT.redo conn (userCommitSource uc)
  let msg = case commit of
              Nothing -> failureMessage "Too far forwards"
              Just c  -> ServerMessage Redo Success (PayloadCommit c)
  broadcast state msg
  printWithTime "Server processed redo"

-- Drag/autofill
handleDrag :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleDrag uc state (PayloadDrag selRng dragRng) = do
  conn <- dbConn <$> readMVar state
  nCells <- IU.getCellsRect conn selRng dragRng
  let newCells = (IU.getMappedFormulaCells selRng dragRng nCells) ++ (IU.getMappedPatternGroups selRng dragRng nCells)
  msg' <- DP.runDispatchCycle state newCells DescendantsWithParent (userCommitSource uc) id
  broadcastFiltered state uc msg'

handleRepeat :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleRepeat uc state (PayloadSelection range origin) = do
  conn <- dbConn <$> readMVar state
  ClientMessage lastAction lastPayload <- DB.getLastMessage conn (userCommitSource uc)
  printObj "Got last thing for repeat: " (lastAction, lastPayload)
  case lastAction of
    Evaluate -> do
      let PayloadCL ((Cell l e v ts):[]) = lastPayload
          cells = map (\l' -> Cell l' e v ts) (rangeToIndices range)
      handleEval uc state (PayloadCL cells)
    Copy -> do
      let PayloadPaste from to = lastPayload
      handleCopy uc state (PayloadPaste from range)
    Delete -> handleDelete uc state (PayloadR range)
    Undo -> handleRedo uc state
    otherwise -> sendToOriginal uc $ ServerMessage Repeat (Failure "Repeat not supported for this action") (PayloadN ())

-- | For now, all this does is acknowledge that a bug report got sent. The actual contents
-- of the bug report (part of the payload) are output to the server log in handleClientMessage,
-- which is where we want it end up anyway, for now. (Alex 10/28/15)
handleBugReport :: ASUserClient -> ASPayload -> IO ()
handleBugReport uc (PayloadText report) = do
  logBugReport report (userCommitSource uc)
  WS.sendTextData (userConn uc) ("ACK" :: T.Text)

handleSetCondFormatRules :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleSetCondFormatRules uc state (PayloadCondFormat rules) = do
  conn <- dbConn <$> readMVar state
  let src = userCommitSource uc
      sid = srcSheetId src
  oldRules <- DB.getCondFormattingRules conn sid
  let symDiff = (union rules oldRules) \\ (intersect rules oldRules)
      locs = concatMap rangeToIndices $ concatMap cellLocs symDiff
  cells <- DB.getPossiblyBlankCells conn locs
  errOrCells <- runEitherT $ conditionallyFormatCells conn sid cells rules emptyContext
  let onFormatSuccess cs = DB.setCondFormattingRules conn sid rules >> DB.setCells conn cs
  either (const $ return ()) onFormatSuccess errOrCells
  broadcastFiltered state uc $ makeCondFormatMessage errOrCells rules

-- #needsrefactor Should eventually merge with handleSetProp. 
handleSetBarProp :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleSetBarProp uc state (PayloadSetBarProp bInd prop) = do 
  conn <- dbConn <$> readMVar state
  let sid = userSheetId uc
  mOldProps <- DB.getBarProps conn bInd
  let oldProps = maybe BP.emptyProps id mOldProps
      newProps = BP.setProp prop oldProps
      newRc    = Bar bInd newProps
      oldRcs   = case mOldProps of
                      Nothing -> []
                      Just _ -> [Bar bInd oldProps]
  DB.setBar conn newRc
  -- Add the barProps to the commit.
  time <- getASTime
  let bardiff = BarDiff { beforeBars = oldRcs, afterBars = [newRc]}
      commit = Commit { barDiff = bardiff, cellDiff = CellDiff { beforeCells = [], afterCells = [] }, commitDescriptorDiff = DescriptorDiff { addedDescriptors = [], removedDescriptors = [] }, time = time}
  DT.updateDBWithCommit conn (userCommitSource uc) commit
  sendToOriginal uc $ ServerMessage SetBarProp Success (PayloadN ())

-- #anand used for importing binary alphasheets files (making a separate REST server for alphasheets
  -- import/export seems overkill given that it's a temporarily needed solution)
  -- so we just send alphasheets files as binary data over websockets and immediately load
  -- into the current sheet.
handleImportBinary :: (Client c) => c -> MVar ServerState -> BL.ByteString -> IO ()
handleImportBinary c state bin = do
  redisConn <- dbConn <$> readMVar state
  case (DS.decodeLazy bin :: Either String ExportData) of
    Left s ->
      let msg = ServerMessage Import (Failure $ "could not process binary file, decode error: " ++ s) (PayloadN ())
      in U.sendMessage msg (conn c)
    Right exported -> do
      DX.importData redisConn exported
      let msg = ServerMessage Import Success (PayloadCL $ exportCells exported)
      U.sendMessage msg (conn c)

handleExport :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
handleExport uc state (PayloadS (Sheet sid _ _))  = do
  conn  <- dbConn <$> readMVar state
  exported <- DX.exportData conn sid
  WS.sendBinaryData (userConn uc) (DS.encodeLazy exported)
