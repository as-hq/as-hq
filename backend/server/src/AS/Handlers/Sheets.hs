module AS.Handlers.Sheets where

import Data.Maybe (catMaybes)
import Database.Redis
import Control.Monad
import Control.Monad.Trans.Either
import qualified Data.Set as Set

import AS.Prelude
import AS.Config.Constants 
import AS.Types.Network
import AS.Types.Locations
import AS.Types.Messages
import AS.Types.DB
import AS.Types.User hiding (userId)
import AS.Types.Window
import AS.Types.Commits

import qualified AS.Kernels.API as Kernels


import AS.Serialize
import AS.Eval.Core
import AS.Reply
import qualified AS.DB.API    as DB
import AS.DB.Users
import AS.DB.Export
import AS.Users

--------------------------------------------------------------------------------

handleGetMyWorkbooks :: MessageContext -> IO ()
handleGetMyWorkbooks msgctx = do
  let conn = msgctx^.dbConnection
      uid = msgctx^.userClient.userId
  refs <- DB.getUserWorkbookRefs conn uid
  sendAction msgctx $ SetMyWorkbooks refs

handleGetOpenedWorkbook :: MessageContext -> IO ()
handleGetOpenedWorkbook msgctx = do
  let conn = msgctx^.dbConnection
      wid  = msgctx^.userClient.userWindow.windowWorkbookId
  fwb <- fromJust <$> DB.getOpenedWorkbook conn wid
  sendAction msgctx $ SetOpenedWorkbook fwb
  -- send eval headers
  let wid = messageWorkbookId msgctx
  headers <- DB.getEvalHeaders conn wid
  sendAction msgctx $ SetEvalHeaders headers

handleNewWorkbook :: MessageContext -> WorkbookName -> IO ()
handleNewWorkbook msgctx name = do
  let conn = msgctx^.dbConnection
      uid = msgctx^.userClient.userId
      state = msgctx^.messageState
  wid <- view workbookId <$> DB.createWorkbook conn uid name
  -- place new workbook under user ownership
  msgctx' <- modifyUser msgctx (& workbookIds %~ Set.insert wid)
  -- update user's workbooks
  handleGetMyWorkbooks msgctx'
  -- open the new workbook
  handleOpenWorkbook msgctx' wid

handleNewSheet :: MessageContext -> SheetName -> IO ()
handleNewSheet msgctx name = do
  let conn = msgctx^.dbConnection
      uid = msgctx^.userClient.userId
  sid <- view sheetId <$> DB.createSheet conn uid name
  -- place new sheet under user ownership
  modifyCurrentWorkbook msgctx (& workbookSheetIds %~ Set.insert sid)
  -- update user's workbook
  handleGetOpenedWorkbook msgctx 
  -- open the new sheet
  handleOpenSheet msgctx sid

-- | When a user wants to rename a sheet, they will send us the new sheet name, 
-- which will contain the new sheetName and the old sheetId. 
-- All we have to then do is modify the SheetId -> Sheet
-- pair in the DB, and send back the updated sheets. 
handleRenameSheet :: MessageContext -> SheetID -> SheetName -> IO ()
handleRenameSheet msgctx sid sname = 
  let conn = msgctx^.dbConnection
  in maybeM 
    (DB.getSheet conn sid)
    (sendFailure msgctx "cannot rename nonexistent sheet") 
    $ \sheet -> do
      DB.setSheet conn (sheet & sheetName .~ sname)
      handleGetOpenedWorkbook msgctx

handleCloneSheet :: MessageContext -> SheetID -> IO ()
handleCloneSheet msgctx sid = do
  let conn = msgctx^.dbConnection
  let uid = msgctx^.userClient.userId
  cloneSid <- cloneSheet conn uid sid
  modifyCurrentWorkbook msgctx (& workbookSheetIds %~ Set.insert cloneSid)
  -- update user's workbook
  handleGetOpenedWorkbook msgctx
  -- open the cloned sheet
  handleOpenSheet msgctx cloneSid

handleOpenWorkbook :: MessageContext -> WorkbookID -> IO ()
handleOpenWorkbook msgctx wid = do
  let conn = msgctx^.dbConnection
      uid = msgctx^.userClient.userId
      state = msgctx^.messageState
  msgctx' <- modifyUser msgctx (& lastOpenWorkbook .~ wid)
  -- update opened workbook
  handleGetOpenedWorkbook msgctx'
  -- notify kernels of opened workbook
  Kernels.openWorkbook conn wid
  -- open the last opened sheet in that workbook
  wb <- fromJust <$> DB.getWorkbook conn wid
  handleOpenSheet msgctx' $ wb^.lastOpenSheet

handleOpenSheet :: MessageContext -> SheetID -> IO ()
handleOpenSheet msgctx sid = do 
  let conn  = msgctx^.dbConnection
      mid   = msgctx^.messageId
  -- get initial sheet data
  cells <- DB.getCellsInSheet conn sid
  bars <- DB.getBarsInSheet conn sid
  rangeDescriptors <- DB.getRangeDescriptorsInSheet conn sid
  condFormatRules <- DB.getCondFormattingRulesInSheet conn sid

  -- update database & state
  modifyCurrentWorkbook msgctx (& lastOpenSheet .~ sid)

  let sheetUpdate = makeSheetUpdateWithNoOldKeys 
                      cells bars rangeDescriptors condFormatRules
  sendAction msgctx $ SetSheetData sid sheetUpdate 

-- This handler is called when a sheet is accessed by a user it is "shared" to.
-- It describes the "acquisition" of a shared sheet. Invoked e.g. when I visit 
-- a link to another user's sheet.
handleAcquireSheet :: MessageContext -> SheetID -> IO ()
handleAcquireSheet msgctx sid = do
  let conn = msgctx^.dbConnection
      wid  = messageWorkbookId msgctx
  wb <- fromJust <$> DB.getWorkbook conn wid
  unless (Set.member sid $ wb^.workbookSheetIds) $ do
    -- designate sheet as shared to user
    modifyCurrentWorkbook msgctx (& workbookSheetIds %~ Set.insert sid)
    -- update user's workbook
    handleGetOpenedWorkbook msgctx
    -- automatically open the acquired sheet
    handleOpenSheet msgctx sid
    -- bring in headers
    newHeaders <- DB.acquireHeaders conn wid sid
    sendAction msgctx $ SetEvalHeaders newHeaders

handleDeleteSheet :: MessageContext -> SheetID -> IO ()
handleDeleteSheet msgctx sid = do
  let uid   = msgctx^.userClient.userId
      conn  = msgctx^.dbConnection
  wb <- modifyCurrentWorkbook msgctx (& workbookSheetIds %~ Set.delete sid)
  -- update user's workbook
  handleGetOpenedWorkbook msgctx
  -- if the last open sheet has changed, open that sheet.
  unless (wb^.lastOpenSheet == msgctx^.userClient.userWindow.windowSheetId) $ 
    handleOpenSheet msgctx (wb^.lastOpenSheet)
  -- actually delete the sheet from database
  DB.deleteSheet conn sid

--------------------------------------------------------------------------------

