module AS.Handlers.Sheets where

import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Database.Redis
import Control.Lens
import Control.Monad.Trans.Either

import AS.Prelude
import AS.Config.Constants 
import AS.Types.Network
import AS.Types.Locations
import AS.Types.Messages
import AS.Types.DB
import AS.Types.User hiding (userId)
import AS.Types.Window
import AS.Types.Commits

import AS.Serialize
import AS.Eval.Core
import AS.Reply
import AS.DB.API
import AS.DB.Users
import AS.DB.Export
import AS.Users
import AS.Config.Settings (headerLangs)

import Control.Monad

handleGetSheets :: MessageContext -> IO ()
handleGetSheets msgctx = do
  let conn = msgctx^.dbConnection
  user <- $fromJust <$> lookupUser conn (msgctx^.userClient.userId)
  let mySids = Set.toList $ user^.sheetIds
  let sharedSids = Set.toList $ user^.sharedSheetIds
  putStrLn $ "got sids in handleGetSheets: " ++ show mySids
  mySheets <- catMaybes <$> multiGet SheetKey dbValToSheet conn mySids
  sharedSheets <- catMaybes <$> multiGet SheetKey dbValToSheet conn sharedSids
  sendAction msgctx $ SetMySheets mySheets sharedSheets

handleNewSheet :: MessageContext -> SheetName -> IO ()
handleNewSheet msgctx name = do
  let conn = msgctx^.dbConnection
      uid = msgctx^.userClient.userId
  sid <- sheetId <$> createSheet conn uid name
  -- place sheet under user ownership
  modifyUser conn uid (& sheetIds %~ Set.insert sid)
  -- update user's list of sheets
  handleGetSheets msgctx 
  -- automatically open the new sheet
  handleOpenSheet msgctx sid

-- | When a user wants to rename a sheet, they will send us the new sheet, which will contain 
-- the new sheetName and the old sheetId. All we have to then do is modify the SheetId -> Sheet
-- pair in the DB, and send back the updated sheets. 
handleRenameSheet :: MessageContext -> ASSheetId -> SheetName -> IO ()
handleRenameSheet msgctx sid sname = do 
  let conn = msgctx^.dbConnection
  sheet <- $fromJust <$> getV conn (SheetKey sid) dbValToSheet
  let newSheet = sheet {sheetName = sname}
  setV conn (SheetKey sid) (SheetValue newSheet)
  handleGetSheets msgctx

handleCloneSheet :: MessageContext -> ASSheetId -> IO ()
handleCloneSheet msgctx sid = do
  let conn = msgctx^.dbConnection
  let uid = msgctx^.userClient.userId
  names <- Set.fromList . map sheetName <$> getUserSheets conn uid
  -- create unique sheet name
  let cloneName name copyNum =  let cur = name ++ " (" ++ show copyNum ++ ")"
                                in if cur `Set.member` names
                                  then cloneName name (copyNum + 1)
                                  else cur
  curName <- sheetName . $fromJust <$> getSheet conn sid
  newSid <- sheetId <$> createSheet conn uid (cloneName curName 1)
  modifyUser conn uid (& sheetIds %~ (Set.insert newSid))
  ex <- cloneData newSid <$> exportSheetData conn sid
  importSheetData conn uid ex
  -- update user sheet records on frontend
  handleGetSheets msgctx
  -- open the cloned sheet
  handleOpenSheet msgctx newSid

handleOpenSheet :: MessageContext -> ASSheetId -> IO ()
handleOpenSheet msgctx sid = do 
  let conn  = msgctx^.dbConnection
      uc    = msgctx^.userClient
      mid   = msgctx^.messageId
  -- get initial sheet data
  cells <- getCellsInSheet conn sid
  bars <- getBarsInSheet conn sid
  rangeDescriptors <- getRangeDescriptorsInSheet conn sid
  condFormatRules <- getCondFormattingRulesInSheet conn sid
  headers <- mapM (getEvalHeader conn sid) headerLangs

  -- update server state
  let newWindow = Window sid (-1, -1) (-1, -1)
  modifyUserClientInState (msgctx^.messageState) (uc^.userSessionId) (& userWindow .~ newWindow) 

  -- update database
  modifyUser conn (uc^.userId) (& lastOpenSheet .~ sid)

  -- pre-evaluate the headers
  mapM_ (runEitherT . evaluateHeader mid) headers
  let sheetUpdate = makeSheetUpdateWithNoOldKeys cells bars rangeDescriptors condFormatRules
  sendAction msgctx $ SetSheetData sid sheetUpdate headers

-- This handler is called whenever a sheet is accessed by a user it is "shared" to.
-- It describes the "acquisition" of a shared sheet. Invoked e.g. when I visit 
-- a link to another user's sheet.
handleAcquireSheet :: MessageContext -> ASSheetId -> IO ()
handleAcquireSheet msgctx sid = do
  let conn = msgctx^.dbConnection
      uid  = msgctx^.userClient.userId
  user <- $fromJust <$> lookupUser conn uid
  unless (Set.member sid $ user^.sheetIds) $ do
    -- designate sheet as shared to user
    modifyUser conn uid (& sharedSheetIds %~ (Set.insert sid))
    -- update user's list of sheets
    handleGetSheets msgctx
    -- automatically open the acquired sheet
    handleOpenSheet msgctx sid

handleDeleteSheet :: MessageContext -> ASSheetId -> IO ()
handleDeleteSheet msgctx sid = do
  let uid = msgctx^.userClient.userId
      conn = msgctx^.dbConnection
  user <- modifyUser conn uid (& sheetIds %~ Set.delete sid)
  -- if the user has no more sheets, give them a default one.
  user' <- if (Set.null $ user^.sheetIds) 
    then do
      sid <- sheetId <$> createSheet conn uid new_sheet_name
      modifyUser conn uid (& sheetIds .~ Set.singleton sid)
    else return user
  -- update the user's list of sheets on frontend
  handleGetSheets msgctx
  -- if the user's last open sheet was the delete one, change it.
  putStrLn $ "user's sheets in handleDeleteSheet: " ++ show (user'^.sheetIds)
  when (user'^.lastOpenSheet == sid) $ do
    let newSheet = $head . Set.elems $ user'^.sheetIds
    modifyUser conn uid (& lastOpenSheet .~ newSheet)
    -- open the new sheet on frontend
    handleOpenSheet msgctx newSheet
  -- actually delete the sheet from database
  deleteSheet conn sid