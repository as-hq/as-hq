module AS.DB.Users where

import AS.Prelude
import Prelude()

import AS.Config.Constants (new_sheet_name)
import AS.Serialize (encode, maybeDecode) 

import AS.Types.Network hiding (userId)
import AS.Types.User
import AS.Types.Locations
import AS.Types.Window
import AS.Types.DB

import AS.DB.API

import qualified Data.Set as Set
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import Database.Redis
import qualified Network.WebSockets as WS
import Control.Monad (void)

import Control.Lens hiding (set)

createUserClient :: Connection -> WS.Connection -> ASUserId -> IO ASUserClient
createUserClient dbConn wsConn uid = do
  maybeUser <- lookupUser dbConn uid
  sid <- case maybeUser of 
    Just user -> return $ user^.lastOpenSheet
    Nothing -> do
      user <- createUser dbConn uid
      return $ user^.lastOpenSheet
  seshId <- T.pack . toString <$> nextRandom
  let window = Window sid (Coord (-1) (-1)) (Coord (-1) (-1))
  return $ UserClient uid wsConn window seshId

lookupUser :: Connection -> ASUserId -> IO (Maybe ASUser)
lookupUser conn uid = getV conn (UserKey uid) dbValToUser

associateSheetWithUser :: Connection -> ASUserId -> ASSheetId -> IO ()
associateSheetWithUser conn uid sid = modifyUser conn uid f
  where f u = u & sheetIds %~ (Set.insert sid)

modifyUser :: Connection -> ASUserId -> (ASUser -> ASUser) -> IO ()
modifyUser conn uid f = do
  user <- lookupUser conn uid
  case user of 
    Nothing -> $error "Cannot modify nonexistent user"
    Just u -> setUser conn $ f u

createUser :: Connection -> ASUserId -> IO ASUser
createUser conn uid = do
  sid <- sheetId <$> createSheet conn new_sheet_name
  let user  = User (Set.fromList [sid]) uid sid
  setUser conn user 
  return user

setUser :: Connection -> ASUser -> IO ()
setUser conn user = setV conn (UserKey $ view userId $ user) (UserValue user)

  