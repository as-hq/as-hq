{-# LANGUAGE OverloadedStrings #-}
module AS.Handlers.Misc where

import AS.Prelude
import Prelude()

import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User
import AS.Types.DB hiding (Clear)
import AS.Types.Eval
import AS.Types.Commits
import AS.Types.CondFormat
import AS.Types.Bar
import AS.Types.BarProps (BarProp)
import AS.Types.Selection
import AS.Types.Updates
import qualified AS.Types.BarProps as BP

import AS.Config.Constants
import AS.Config.Settings

import AS.Handlers.Eval
import AS.Handlers.Paste
import AS.Handlers.Delete

import AS.Eval.CondFormat
import AS.Eval.Core

import AS.Window
import AS.Logging
import AS.Reply

import Database.Redis (Connection)

import qualified AS.Dispatch.Core         as DP
import qualified AS.Util                  as U
import qualified AS.Kernels.LanguageUtils as LU
import qualified AS.Users                 as US
import qualified AS.InferenceUtils        as IU
import qualified AS.Serialize             as S

import qualified AS.DB.Transaction        as DT
import qualified AS.DB.API                as DB
import qualified AS.DB.Clear              as DC
import qualified AS.DB.Export             as DX
import qualified AS.DB.Graph              as G

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List
import Data.Maybe

import qualified Database.Redis as R
import qualified Network.WebSockets as WS

import Control.Concurrent
import Control.Exception
import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Either

-- Used solely for acknowledging keepalive messages sent from the frontend. 
handleAcknowledge :: ASUserClient -> IO ()
handleAcknowledge = handleInitialize 

handleInitialize :: ASUserClient -> IO ()
handleInitialize uc = WS.sendTextData (userConn uc) ("ACK" :: T.Text)

-- #needsrefactor currently incomplete, and inactive. 
-- handleNew :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
-- handleNew uc state (PayloadWorkbookSheets (wbs:[])) = do
--   conn <- dbConn <$> readMVar state
--   wbs' <- DB.createWorkbookSheet conn wbs
--   broadcastTo state (wsSheets wbs') $ ClientMessage New Success (PayloadWorkbookSheets [wbs'])
-- handleNew uc state (PayloadWB wb) = do
--   conn <- dbConn <$> readMVar state
--   wb' <- DB.createWorkbook conn (workbookSheets wb)
--   broadcastTo state (wsSheets wb') $ ClientMessage New Success (PayloadWB wb')
--   return () -- TODO determine whether users should be notified

handleOpen :: MessageId -> ASUserClient -> MVar ServerState -> ASSheetId -> IO ()
handleOpen mid uc state sid = do 
  -- update state
  conn <- view dbConn <$> readMVar state
  settings <- view appSettings <$> readMVar state
  let makeNewWindow (UserClient uid c _ sid) = UserClient uid c startWindow sid
      startWindow = Window sid (Coord (-1) (-1)) (Coord (-1) (-1))
  US.modifyUser makeNewWindow uc state
  -- get initial sheet data
  cells <- DB.getCellsInSheet conn sid
  bars <- DB.getBarsInSheet conn sid
  rangeDescriptors <- DB.getRangeDescriptorsInSheet conn sid
  condFormatRules <- DB.getCondFormattingRulesInSheet conn sid
  headers         <- mapM (DB.getEvalHeader conn sid) headerLangs
  -- pre-evaluate the headers
  mapM (runEitherT . evaluateHeader settings) headers
  let sheetUpdate = SheetUpdate (Update cells []) (Update bars []) (Update rangeDescriptors []) (Update condFormatRules []) -- #exposed
  sendToOriginal uc $ ClientMessage mid $ SetInitialProperties sheetUpdate headers

-- NOTE: doesn't send back blank cells. This means that if, e.g., there are cells that got blanked
-- in the database, those blank cells will not get passed to the user (and those cells don't get
-- deleted on frontend), meaning we have to ensure that deleted cells are manually wiped from the
-- frontend store the moment they get deleted.
-- 
-- Also, might want to eventually send back things besides cells as well. 

-- Temporarily not supporting lazy loading. As of 1/14, it is not at all the 
-- speed bottleneck, but adds a ton of complexity to the UX. 
handleUpdateWindow :: MessageId -> ASUserClient -> ServerState -> ASWindow -> IO ()
handleUpdateWindow mid uc state w = sendToOriginal uc $ ClientMessage mid NoAction
-- handleUpdateWindow cid mstate w = do
  -- state <- readMVar mstate
--   let conn = state^.dbConn
--   let (Just user') = US.getUserByClientId cid state -- user' is to get latest user on server; if this fails then somehow your connection isn't stored in the state
--   let oldWindow = userWindow user'
--   (flip catch) (badCellsHandler state user') (do
--     let newLocs = getScrolledLocs oldWindow w
--     mcells <- DB.getCells conn $ concatMap rangeToIndices newLocs
--     sendSheetUpdate mid user' $ sheetUpdateFromCells $ catMaybes mcells
--     US.modifyUser (updateWindow w) user' mstate)

-- | If a message is failing to parse from the server, undo the last commit (the one that added
-- the message to the server.) I doubt this fix is completely foolproof, but it keeps data
-- from getting lost and doesn't require us to manually reset the server.
badCellsHandler :: ServerState -> ASUserClient -> SomeException -> IO ()
badCellsHandler state uc e = do
  logError ("Error while fetching cells: " ++ (show e)) (userCommitSource uc)
  printWithTime "Undoing last commit"
  DT.undo (state^.appSettings.graphDbAddress) (state^.dbConn) (userCommitSource uc)
  return ()

handleGet :: MessageId -> ASUserClient -> ServerState -> [ASIndex] -> IO ()
handleGet mid uc state locs = do
  mcells <- DB.getCells (state^.dbConn) locs
  sendToOriginal uc $ ClientMessage mid $ PassCellsToTest $ catMaybes mcells
-- handleGet uc state (PayloadList Sheets) = do
--   curState <- readMVar state
--   ss <- DB.getAllSheets (dbConn curState)
--   sendToOriginal uc $ ClientMessage UpdateSheet Success (PayloadSS ss)
-- handleGet uc state (PayloadList Workbooks) = do
--   curState <- readMVar state
--   ws <- DB.getAllWorkbooks (dbConn curState)
--   sendToOriginal uc $ ClientMessage UpdateSheet Success (PayloadWBS ws)
-- handleGet uc state (PayloadList WorkbookSheets) = do
--   curState <- readMVar state
--   wss <- DB.getAllWorkbookSheets (dbConn curState)
--   printWithTime $ "getting all workbooks: "  ++ (show wss)
--   sendToOriginal uc $ ClientMessage UpdateSheet Success (PayloadWorkbookSheets wss)
-- Will uncomment when we're actually using this code; in the meantime let's not bother to maintain it. (12/28)

-- Had relevance back when UserClients could have multiple windows, which never made sense anyway.
-- (Alex 11/3)
-- handleClose :: ASUserClient -> MVar ServerState -> ASPayload -> IO ()
-- handleClose _ _ _ = return ()

handleIsCoupled :: MessageId -> ASUserClient -> ServerState -> ASIndex -> IO ()
handleIsCoupled mid uc state loc = do 
  mCell <- DB.getCell (state^.dbConn) loc
  let isCoupled = maybe False (isJust . view cellRangeKey) mCell
  sendToOriginal uc $ ClientMessage mid $ PassIsCoupledToTest isCoupled


handleClear :: (Client c) => MessageId -> c  -> ServerState -> ASSheetId -> IO ()
handleClear mid client state sid = do
  let conn = state^.dbConn
      settings = state^.appSettings
      graphAddr = settings^.graphDbAddress
  DC.clearSheet settings conn sid
  G.recompute graphAddr conn
  broadcastTo state [sid] $ ClientMessage mid $ ClearSheet sid

handleUndo :: MessageId -> ASUserClient -> ServerState -> IO ()
handleUndo mid uc state = do
  let conn = state^.dbConn
      graphAddress = state^.appSettings.graphDbAddress
  printWithTime "right before commit"
  commit <- DT.undo graphAddress conn (userCommitSource uc)
  let errOrUpdate = maybe (Left TooFarBack) (Right . sheetUpdateFromCommit . flipCommit) commit
  broadcastErrOrUpdate mid state uc errOrUpdate

handleRedo :: MessageId -> ASUserClient -> ServerState -> IO ()
handleRedo mid uc state = do
  let conn = state^.dbConn
      graphAddress = state^.appSettings.graphDbAddress
  commit <- DT.redo graphAddress conn (userCommitSource uc)
  let errOrUpdate = maybe (Left TooFarForwards) (Right . sheetUpdateFromCommit) commit
  broadcastErrOrUpdate mid state uc errOrUpdate

-- Drag/autofill
handleDrag :: MessageId -> ASUserClient -> ServerState -> ASRange -> ASRange -> IO ()
handleDrag mid uc state selRng dragRng = do
  nCells <- IU.getCellsRect (state^.dbConn) selRng dragRng
  let newCells = (IU.getMappedFormulaCells selRng dragRng nCells) ++ (IU.getMappedPatternGroups selRng dragRng nCells)
  errOrUpdate <- DP.runDispatchCycle state newCells DescendantsWithParent (userCommitSource uc) id
  broadcastErrOrUpdate mid state uc errOrUpdate

handleRepeat :: MessageId -> ASUserClient -> ServerState -> Selection -> IO ()
handleRepeat mid uc state selection = return () -- do
  -- let conn = state^.dbConn
  -- ServerMessage lastAction lastPayload <- DB.getLastMessage conn (userCommitSource uc)
  -- printObj "Got last thing for repeat: " (lastAction, lastPayload)
  -- case lastAction of
  --   Evaluate -> do
  --     let PayloadCL ((Cell l e v ts):[]) = lastPayload
  --         cells = map (\l' -> Cell l' e v ts) (rangeToIndices range)
  --     handleEval uc state (PayloadCL cells)
  --   Copy -> do
  --     let PayloadPaste from to = lastPayload
  --     handleCopy uc state (PayloadPaste from range)
  --   Delete -> handleDelete uc state (PayloadR range)
  --   Undo -> handleRedo uc state
  --   otherwise -> sendToOriginal uc $ failureMessage "Repeat not supported for this action"
  -- temporarily disabling until we implement this for realsies (Alex 12/28)

-- | For now, all this does is acknowledge that a bug report got sent. The actual contents
-- of the bug report (part of the payload) are output to the server log in handleServerMessage,
-- which is where we want it end up anyway, for now. (Alex 10/28/15)
handleBugReport :: ASUserClient -> String -> IO ()
handleBugReport uc report = do
  logBugReport report (userCommitSource uc)
  WS.sendTextData (userConn uc) ("ACK" :: T.Text)

handleUpdateCondFormatRules :: MessageId -> ASUserClient -> ServerState -> CondFormatRuleUpdate -> IO ()
handleUpdateCondFormatRules mid uc state u@(Update updatedRules deleteRuleIds) = do
  let conn = state^.dbConn
      graphAddress = state^.appSettings.graphDbAddress
      src = userCommitSource uc 
      sid = srcSheetId src
  rulesToDelete <- DB.getCondFormattingRules conn sid deleteRuleIds
  oldRules <- DB.getCondFormattingRulesInSheet conn sid
  let allRulesUpdated = applyUpdate u oldRules
      updatedLocs = concatMap rangeToIndices $ concatMap cellLocs $ union updatedRules rulesToDelete
  cells <- DB.getPossiblyBlankCells conn updatedLocs
  errOrCells <- runEitherT $ conditionallyFormatCells state sid cells allRulesUpdated emptyContext
  time <- getASTime
  let errOrCommit = fmap (\cs -> Commit (Diff cs cells) emptyDiff emptyDiff (Diff updatedRules rulesToDelete) time) errOrCells
  either (const $ return ()) (DT.updateDBWithCommit graphAddress conn src) errOrCommit
  broadcastErrOrUpdate mid state uc $ fmap sheetUpdateFromCommit errOrCommit

handleGetBar :: MessageId -> ASUserClient -> ServerState -> BarIndex -> IO ()
handleGetBar mid uc state bInd = do 
  mBar <- DB.getBar (state^.dbConn) bInd
  let msg = maybe (ClientMessage mid $ PassBarToTest $ Bar bInd BP.emptyProps) (ClientMessage mid . PassBarToTest) mBar
  sendToOriginal uc msg

-- #needsrefactor Should eventually merge with handleSetProp. 
handleSetBarProp :: MessageId -> ASUserClient -> ServerState -> BarIndex -> BarProp -> IO ()
handleSetBarProp mid uc state bInd prop = do 
  let conn = state^.dbConn
      graphAddress = state^.appSettings.graphDbAddress
  time <- getASTime
  oldPropsAtInd <- maybe BP.emptyProps barProps <$> DB.getBar conn bInd
  let newPropsAtInd = BP.setProp prop oldPropsAtInd
      (oldBar, newBar) = (Bar bInd oldPropsAtInd, Bar bInd newPropsAtInd)
      commit = (emptyCommitWithTime time) { barDiff = Diff { beforeVals = [oldBar], afterVals = [newBar] } } -- #lens
  DT.updateDBWithCommit graphAddress conn (userCommitSource uc) commit
  broadcastSheetUpdate mid state (SheetUpdate emptyUpdate (Update [newBar] []) emptyUpdate emptyUpdate)

-- #anand used for importing binary alphasheets files (making a separate REST server for alphasheets
-- import/export seems overkill given that it's a temporarily needed solution)
-- so we just send alphasheets files as binary data over websockets and immediately load
-- into the current sheet.
handleImportBinary :: (Client c) => c -> MVar ServerState -> BL.ByteString -> IO ()
handleImportBinary c mstate bin = do
  state <- readMVar mstate
  case (S.decodeLazy bin :: Either String ExportData) of
    Left s ->
      let msg = failureMessage import_message_id $ "could not process binary file, decode error: " ++ s
      in U.sendMessage msg (clientConn c)
    Right exportedData -> do
      DX.importSheetData (state^.appSettings) (state^.dbConn) exportedData
      let msg = ClientMessage import_message_id $ AskUserToOpen $ exportDataSheetId exportedData
      U.sendMessage msg (clientConn c)

handleExport :: ASUserClient -> ServerState -> ASSheetId -> IO ()
handleExport uc state sid = do
  exported <- DX.exportSheetData (state^.dbConn) sid
  WS.sendBinaryData (userConn uc) (S.encodeLazy exported)
