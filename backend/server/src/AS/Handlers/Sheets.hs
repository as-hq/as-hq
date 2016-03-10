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


handleGetSheets :: MessageId -> ASUserClient -> ServerState -> IO ()
handleGetSheets mid uc state = do
  let conn = state^.dbConn
  user <- $fromJust <$> lookupUser conn (uc^.userId)
  let sids = Set.toList $ user^.sheetIds
  sheets <- catMaybes <$> multiGet SheetKey dbValToSheet conn sids
  sendToOriginal uc $ ClientMessage mid $ SetMySheets sheets 

handleNewSheet :: MessageId -> ASUserClient -> ServerState -> SheetName -> IO ()
handleNewSheet mid uc state name = do
  let conn = state^.dbConn
  sid <- sheetId <$> createSheet conn name
  associateSheetWithUser conn (uc^.userId) sid
  handleGetSheets mid uc state -- update user's sheets
  sendToOriginal uc $ ClientMessage mid $ AskOpenSheet sid

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
