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
import AS.Types.BarProps (BarProp)
import AS.Types.Selection
import AS.Types.Updates
import qualified AS.Types.BarProps as BP

import AS.Handlers.Eval
import AS.Handlers.Paste
import AS.Handlers.Delete

import AS.Eval.CondFormat

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
import Control.Applicative
import Control.Monad.Trans.Either

handleAcknowledge :: ASUserClient -> IO ()
handleAcknowledge uc = WS.sendTextData (userConn uc) ("ACK" :: T.Text)

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

handleOpen :: ASUserClient -> MVar ServerState -> ASSheetId -> IO ()
handleOpen uc state sid = do 
  -- update state
  conn <- dbConn <$> readMVar state
  let makeNewWindow (UserClient uid c _ sid) = UserClient uid c startWindow sid
      startWindow = Window sid (-1,-1) (-1,-1)
  US.modifyUser makeNewWindow uc state
  -- get header files data to send back to user user
  let langs = [Python, R] -- should probably make list of langs a const somewhere...
  headers         <- mapM (getEvalHeader conn sid) langs
  -- get conditional formatting data to send back to user user
  condFormatRules <- DB.getCondFormattingRulesInSheet conn sid
  let xps = map (\(str, lang) -> Expression str lang) (zip headers langs)
  -- get column props
  bars <- DB.getBarsInSheet conn sid

  let sheetUpdate = SheetUpdate emptyUpdate (Update bars []) emptyUpdate (Update condFormatRules []) -- #exposed
  sendToOriginal uc $ ClientMessage $ SetInitialProperties sheetUpdate xps

-- NOTE: doesn't send back blank cells. This means that if, e.g., there are cells that got blanked
-- in the database, those blank cells will not get passed to the user (and those cells don't get
-- deleted on frontend), meaning we have to ensure that deleted cells are manually wiped from the
-- frontend store the moment they get deleted.
-- 
-- Also, might want to eventually send back things besides cells as well. 
handleUpdateWindow :: ClientId -> MVar ServerState -> ASWindow -> IO ()
handleUpdateWindow cid state w = do
  curState <- readMVar state
  let (Just user') = US.getUserByClientId cid curState -- user' is to get latest user on server; if this fails then somehow your connection isn't stored in the state
  let oldWindow = userWindow user'
  (flip catch) (badCellsHandler (dbConn curState) user') (do
    let newLocs = getScrolledLocs oldWindow w
    mcells <- DB.getCells (dbConn curState) $ concat $ map rangeToIndices newLocs
    sendSheetUpdate user' $ sheetUpdateFromCells $ catMaybes mcells
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

handleGet :: ASUserClient -> MVar ServerState -> [ASIndex] -> IO ()
handleGet uc state locs = do
  curState <- readMVar state
  mcells <- DB.getCells (dbConn curState) locs
  sendSheetUpdate uc $ sheetUpdateFromCells $ catMaybes mcells
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

handleClear :: (Client c) => c  -> MVar ServerState -> ASSheetId -> IO ()
handleClear client state sid = do
  conn <- dbConn <$> readMVar state
  DB.clearSheet conn sid
  G.recompute conn
  broadcastTo state [sid] $ ClientMessage $ ClearSheet sid

handleUndo :: ASUserClient -> MVar ServerState -> IO ()
handleUndo uc state = do
  conn <- dbConn <$> readMVar state
  printWithTime "right before commit"
  commit <- DT.undo conn (userCommitSource uc)
  let errOrUpdate = maybe (Left TooFarBack) (Right . sheetUpdateFromCommit . flipCommit) commit
  broadcastErrOrUpdate state uc errOrUpdate

handleRedo :: ASUserClient -> MVar ServerState -> IO ()
handleRedo uc state = do
  conn <- dbConn <$> readMVar state
  commit <- DT.redo conn (userCommitSource uc)
  let errOrUpdate = maybe (Left TooFarForwards) (Right . sheetUpdateFromCommit) commit
  broadcastErrOrUpdate state uc errOrUpdate

-- Drag/autofill
handleDrag :: ASUserClient -> MVar ServerState -> ASRange -> ASRange -> IO ()
handleDrag uc state selRng dragRng = do
  conn <- dbConn <$> readMVar state
  nCells <- IU.getCellsRect conn selRng dragRng
  let newCells = (IU.getMappedFormulaCells selRng dragRng nCells) ++ (IU.getMappedPatternGroups selRng dragRng nCells)
  errOrUpdate <- DP.runDispatchCycle state newCells DescendantsWithParent (userCommitSource uc) id
  broadcastErrOrUpdate state uc errOrUpdate

handleRepeat :: ASUserClient -> MVar ServerState -> Selection -> IO ()
handleRepeat uc state selection = return () -- do
  -- conn <- dbConn <$> readMVar state
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

handleUpdateCondFormatRules :: ASUserClient -> MVar ServerState -> CondFormatRuleUpdate -> IO ()
handleUpdateCondFormatRules uc state u@(Update updatedRules deleteRuleIds) = do
  conn <- dbConn <$> readMVar state
  let src = userCommitSource uc 
      sid = srcSheetId src
  rulesToDelete <- DB.getCondFormattingRules conn sid deleteRuleIds
  oldRules <- DB.getCondFormattingRulesInSheet conn sid
  let allRulesUpdated = applyUpdate u oldRules
      updatedLocs = concatMap rangeToIndices $ concatMap cellLocs $ union updatedRules rulesToDelete
  cells <- DB.getPossiblyBlankCells conn updatedLocs
  errOrCells <- runEitherT $ conditionallyFormatCells conn sid cells allRulesUpdated emptyContext
  time <- getASTime
  let errOrCommit = fmap (\cs -> Commit (Diff cs cells) emptyDiff emptyDiff (Diff updatedRules rulesToDelete) time) errOrCells
  either (const $ return ()) (DT.updateDBWithCommit conn src) errOrCommit
  broadcastErrOrUpdate state uc $ fmap sheetUpdateFromCommit errOrCommit

-- #needsrefactor Should eventually merge with handleSetProp. 
handleSetBarProp :: ASUserClient -> MVar ServerState -> BarIndex -> BarProp -> IO ()
handleSetBarProp uc state bInd prop = do 
  conn <- dbConn <$> readMVar state
  oldProps <- maybe BP.emptyProps barProps <$> DB.getBar conn bInd
  let newProps = BP.setProp prop oldProps
      newBar   = Bar bInd newProps
      oldBar   = Bar bInd oldProps
  DB.setBar conn newBar
  -- Commit barProps.
  time <- getASTime
  let bd     = Diff { beforeVals = [oldBar], afterVals = [newBar] }
      commit = (emptyCommitWithTime time) { barDiff = bd }
  DT.updateDBWithCommit conn (userCommitSource uc) commit
  sendToOriginal uc $ ClientMessage NoAction

-- #anand used for importing binary alphasheets files (making a separate REST server for alphasheets
-- import/export seems overkill given that it's a temporarily needed solution)
-- so we just send alphasheets files as binary data over websockets and immediately load
-- into the current sheet.
handleImportBinary :: (Client c) => c -> MVar ServerState -> BL.ByteString -> IO ()
handleImportBinary c state bin = do
  redisConn <- dbConn <$> readMVar state
  case (DS.decodeLazy bin :: Either String ExportData) of
    Left s ->
      let msg = failureMessage $ "could not process binary file, decode error: " ++ s
      in U.sendMessage msg (conn c)
    Right exported -> do
      DX.importData redisConn exported
      let msg = ClientMessage $ LoadImportedCells $ exportCells exported
      U.sendMessage msg (conn c)

handleExport :: ASUserClient -> MVar ServerState -> ASSheetId -> IO ()
handleExport uc state sid  = do
  conn  <- dbConn <$> readMVar state
  exported <- DX.exportData conn sid
  WS.sendBinaryData (userConn uc) (DS.encodeLazy exported)
