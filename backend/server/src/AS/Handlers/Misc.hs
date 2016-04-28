{-# LANGUAGE OverloadedStrings #-}
module AS.Handlers.Misc where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List
import Data.Maybe
import qualified Network.WebSockets as WS
import Control.Exception
import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Either
import Database.Redis (Connection)

import AS.Prelude
import AS.Types.Cell
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User (ASUserId)
import AS.Types.DB hiding (Clear)
import AS.Types.Graph
import AS.Types.Eval
import AS.Types.Commits
import AS.Types.CondFormat
import AS.Types.Bar
import AS.Types.BarProps (BarProp)
import AS.Types.Selection
import AS.Types.Updates
import AS.Types.Window
import qualified AS.Types.BarProps as BP

import AS.Config.Constants
import AS.Config.Settings
import AS.Handlers.Eval
import AS.Handlers.Paste
import AS.Handlers.Delete
import AS.Eval.CondFormat
import AS.Eval.Core
import AS.Logging
import AS.Reply

import qualified AS.Dispatch.Core         as DP
import qualified AS.Util                  as U
import qualified AS.Users                 as US
import qualified AS.InferenceUtils        as IU
import qualified AS.Serialize             as S
import qualified AS.DB.Transaction        as DT
import qualified AS.DB.API                as DB
import qualified AS.DB.Export             as DX
import qualified AS.DB.Graph              as G
import qualified AS.DB.Clear              as DC

import qualified Data.Set as S


-- Temporarily not supporting lazy loading. As of 1/14, it is not at all the 
-- speed bottleneck, but adds a ton of complexity to the UX. 
handleUpdateWindow :: MessageContext -> ASWindow -> IO ()
handleUpdateWindow msgctx w = sendAction msgctx NoAction
-- handleUpdateWindow cid mstate w = do
  -- state <- readState mstate
--   let conn = state^.dbConn
--   let (Just user') = US.getUserByClientId cid state -- user' is to get latest user on server; if this fails then somehow your connection isn't stored in the state
--   let oldWindow = userWindow user'
--   (flip catch) (badCellsHandler state user') (do
--     let newLocs = getScrolledLocs oldWindow w
--     mcells <- DB.getCells conn $ concatMap finiteRangeToIndices newLocs
--     sendSheetUpdate mid user' $ sheetUpdateFromCells $ catMaybes mcells
--     US.modifyUser (updateWindow w) user' mstate)

-- | If a message is failing to parse from the server, undo the last commit (the one that added
-- the message to the server.) I doubt this fix is completely barlproof, but it keeps data
-- from getting lost and doesn't require us to manually reset the server.
badCellsHandler :: MessageContext -> SomeException -> IO ()
badCellsHandler msgctx e = do
  let src = messageCommitSource msgctx
  putsError src $ "Error while fetching cells: " ++ (show e)
  DT.undo (msgctx^.dbConnection) src
  puts "Reverted last commit."

handleGet :: MessageContext -> [ASIndex] -> IO ()
handleGet msgctx locs = do
  mcells <- DB.getCells (msgctx^.dbConnection) locs
  sendAction msgctx $ PassCellsToTest (catMaybes mcells)

handleIsCoupled :: MessageContext -> ASIndex -> IO ()
handleIsCoupled msgctx loc = do 
  mCell <- DB.getCell (msgctx^.dbConnection) loc
  let isCoupled = maybe False (isJust . view cellRangeKey) mCell
  sendAction msgctx $ PassIsCoupledToTest isCoupled

handleClear :: MessageContext -> ASSheetId -> IO ()
handleClear msgctx sid = do
  DC.clearSheet (msgctx^.dbConnection) sid
  broadcastActionTo msgctx [sid] $ ClearSheet sid

handleUndo :: MessageContext -> IO ()
handleUndo msgctx = do
  commit <- DT.undo (msgctx^.dbConnection) (messageCommitSource msgctx)
  let errOrUpdate = maybe (Left TooFarBack) (Right . sheetUpdateFromCommit . flipCommit) commit
  broadcastErrOrUpdate msgctx errOrUpdate

handleRedo :: MessageContext -> IO ()
handleRedo msgctx = do
  commit <- DT.redo (msgctx^.dbConnection) (messageCommitSource msgctx)
  let errOrUpdate = maybe (Left TooFarForwards) (Right . sheetUpdateFromCommit) commit
  broadcastErrOrUpdate msgctx errOrUpdate

-- Drag/autofill
handleDrag :: MessageContext -> ASRange -> ASRange -> IO ()
handleDrag msgctx selRng dragRng = do
  nCells <- IU.getCellsRect (msgctx^.dbConnection) selRng dragRng
  let newCells =  (IU.getMappedFormulaCells selRng dragRng nCells) ++ 
                  (IU.getMappedPatternGroups selRng dragRng nCells)
  errOrUpdate <- DP.runDispatchCycle msgctx newCells DescendantsWithParent id
  broadcastErrOrUpdate msgctx errOrUpdate

handleRepeat :: MessageContext -> Selection -> IO ()
handleRepeat msgctx selection = return () -- do
  -- let conn = state^.dbConn
  -- ServerMessage lastAction lastPayload <- DB.getLastMessage conn (userCommitSource uc)
  -- case lastAction of
  --   Evaluate -> do
  --     let PayloadCL ((Cell l e v ts):[]) = lastPayload
  --         cells = map (\l' -> Cell l' e v ts) (finiteRangeToIndices range)
  --     handleEval uc state (PayloadCL cells)
  --   Copy -> do
  --     let PayloadPaste from to = lastPayload
  --     handleCopy uc state (PayloadPaste from range)
  --   Delete -> handleDelete uc state (PayloadR range)
  --   Undo -> handleRedo uc state
  --   otherwise -> sendToOriginal uc $ failureMessage "Repeat not supported for this action"
  -- temporarily disabling until we implement this for realsies (Alex 12/28)


handleUpdateCondFormatRules :: MessageContext -> [CondFormatRule] -> [CondFormatRuleId] -> IO ()
handleUpdateCondFormatRules msgctx updatedRules deleteRuleIds = do
  let conn = msgctx^.dbConnection
      src = messageCommitSource msgctx
      sid = messageSheetId msgctx
      u   = updateFromLists updatedRules deleteRuleIds
  rulesToDelete <- DB.getCondFormattingRules conn sid deleteRuleIds
  oldRules <- DB.getCondFormattingRulesInSheet conn sid
  let allRulesUpdated = applyUpdate u oldRules
      updatedLocs = concatMap finiteRangeToIndices $ concatMap cellLocs $ union updatedRules rulesToDelete
  cells <- DB.getPossiblyBlankCells conn updatedLocs
  errOrCells <- runEitherT $ conditionallyFormatCells msgctx emptyContext cells allRulesUpdated DP.evalChain
  time <- getASTime
  let errOrCommit = fmap (\cs -> Commit (Diff cs cells) emptyDiff emptyDiff (Diff updatedRules rulesToDelete) time) errOrCells
  either (const $ return ()) (DT.updateDBWithCommit conn src) errOrCommit
  broadcastErrOrUpdate msgctx $ fmap sheetUpdateFromCommit errOrCommit

handleGetBar :: MessageContext -> BarIndex -> IO ()
handleGetBar msgctx bInd = do 
  mBar <- DB.getBar (msgctx^.dbConnection) bInd
  let act = maybe (PassBarToTest $ Bar bInd BP.emptyProps) PassBarToTest mBar
  sendAction msgctx act

-- #needsrefactor Should eventually merge with handleSetProp. 
handleSetBarProp :: MessageContext -> BarIndex -> BarProp -> IO ()
handleSetBarProp msgctx bInd prop = do 
  let conn = msgctx^.dbConnection
      src = messageCommitSource msgctx
  time <- getASTime
  oldPropsAtInd <- maybe BP.emptyProps barProps <$> DB.getBar conn bInd
  let newPropsAtInd = BP.setProp prop oldPropsAtInd
      (oldBar, newBar) = (Bar bInd oldPropsAtInd, Bar bInd newPropsAtInd)
      commit = (emptyCommitWithTime time) { barDiff = Diff { beforeVals = [oldBar], afterVals = [newBar] } } -- #lens
  DT.updateDBWithCommit conn src commit
  broadcastSheetUpdate msgctx $ emptySheetUpdate & barUpdates.newVals .~ [newBar]
