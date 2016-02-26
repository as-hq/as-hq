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
import qualified AS.Users as US
import AS.Config.Settings (headerLangs)


handleGetSheets :: MessageId -> ASUserClient -> ServerState -> IO ()
handleGetSheets mid uc state = do
  let conn = state^.dbConn
  user <- $fromJust <$> lookupUser conn (userId uc)
  let sids = Set.toList $ user^.sheetIds
  sheets <- catMaybes <$> multiGet SheetKey dbValToSheet conn sids
  sharedSheets <- getS SharedSheetsKey dbValToSheet conn 
  sendToOriginal uc $ ClientMessage mid $ SetMySheets $ sheets ++ sharedSheets

handleNewSheet :: MessageId -> ASUserClient -> ServerState -> SheetName -> IO ()
handleNewSheet mid uc state name = do
  let conn = state^.dbConn
  sid <- sheetId <$> createSheet conn name
  associateSheetWithUser conn (userId uc) sid
  handleGetSheets mid uc state -- update user's sheets
  sendToOriginal uc $ ClientMessage mid $ AskOpenSheet sid

handleOpenSheet :: MessageId -> ASUserClient -> State -> ASSheetId -> IO ()
handleOpenSheet mid uc state sid = do 
  -- update state
  conn <- view dbConn <$> readState state
  settings <- view appSettings <$> readState state
  let makeNewWindow (UserClient uid c _ sid) = UserClient uid c startWindow sid
      startWindow = Window sid (Coord (-1) (-1)) (Coord (-1) (-1))
  US.modifyUser makeNewWindow uc state

  -- get initial sheet data
  cells <- getCellsInSheet conn sid
  bars <- getBarsInSheet conn sid
  rangeDescriptors <- getRangeDescriptorsInSheet conn sid
  condFormatRules <- getCondFormattingRulesInSheet conn sid
  headers <- mapM (getEvalHeader conn sid) headerLangs

  -- update lastOpenSheet for the user
  modifyUser conn (userId uc) (\u -> u & lastOpenSheet .~ sid)

  -- pre-evaluate the headers
  mapM (runEitherT . evaluateHeader settings) headers
  let sheetUpdate = makeSheetUpdateWithNoOldKeys cells bars rangeDescriptors condFormatRules
  sendToOriginal uc $ ClientMessage mid $ SetSheetData sid sheetUpdate headers