module AS.Handlers.Sheets where

import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Database.Redis
import Control.Lens
import Control.Monad.Trans.Either

import AS.Prelude
import Prelude()
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
import AS.Users
import AS.Config.Settings (headerLangs)

import Control.Monad


handleGetSheets :: MessageId -> ASUserClient -> ServerState -> IO ()
handleGetSheets mid uc state = do
  let conn = state^.dbConn
  user <- $fromJust <$> lookupUser conn (uc^.userId)
  let mySids = Set.toList $ user^.sheetIds
  let sharedSids = Set.toList $ user^.sharedSheetIds
  mySheets <- catMaybes <$> multiGet SheetKey dbValToSheet conn mySids
  sharedSheets <- catMaybes <$> multiGet SheetKey dbValToSheet conn sharedSids
  sendToOriginal uc $ ClientMessage mid $ SetMySheets mySheets sharedSheets

handleNewSheet :: MessageId -> ASUserClient -> State -> SheetName -> IO ()
handleNewSheet mid uc state name = do
  curState <- readState state
  let conn = curState^.dbConn
  sid <- sheetId <$> createSheet conn (uc^.userId) name
  -- place sheet under user ownership
  modifyUser conn (uc^.userId) (& sheetIds %~ (Set.insert sid))
  -- update user's list of sheets
  handleGetSheets mid uc curState 
  -- automatically open the new sheet
  handleOpenSheet mid uc state sid

handleOpenSheet :: MessageId -> ASUserClient -> State -> ASSheetId -> IO ()
handleOpenSheet mid uc state sid = do 
  conn <- view dbConn <$> readState state

  -- get initial sheet data
  cells <- getCellsInSheet conn sid
  bars <- getBarsInSheet conn sid
  rangeDescriptors <- getRangeDescriptorsInSheet conn sid
  condFormatRules <- getCondFormattingRulesInSheet conn sid
  headers <- mapM (getEvalHeader conn sid) headerLangs

  -- update server state
  let newWindow = Window sid (-1, -1) (-1, -1)
  modifyUserInState state (uc^.userId) (& userWindow .~ newWindow) 

  -- update database
  modifyUser conn (uc^.userId) (& lastOpenSheet .~ sid)

  -- pre-evaluate the headers
  mapM_ (runEitherT . evaluateHeader) headers
  let sheetUpdate = makeSheetUpdateWithNoOldKeys cells bars rangeDescriptors condFormatRules
  sendToOriginal uc $ ClientMessage mid $ SetSheetData sid sheetUpdate headers

-- This handler is called whenever a sheet is accessed by a user it is "shared" to.
-- It describes the "acquisition" of a shared sheet. Invoked e.g. when I visit 
-- a link to another user's sheet.
handleAcquireSheet :: MessageId -> ASUserClient -> State -> ASSheetId -> IO ()
handleAcquireSheet mid uc state sid = do
  curState <- readState state
  let conn = curState^.dbConn
      uid = uc^.userId
  user <- $fromJust <$> lookupUser conn uid
  unless (Set.member sid (user^.sheetIds)) $ do
    -- designate sheet as shared to user
    modifyUser conn uid (& sharedSheetIds %~ (Set.insert sid))
    -- update user's list of sheets
    handleGetSheets mid uc curState
    -- automatically open the acquired sheet
    handleOpenSheet mid uc state sid
