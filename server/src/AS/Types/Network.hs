module AS.Types.Network
  ( module AS.Types.Network
  , module AS.Types.Sheets
  ) where

import AS.Types.Sheets 
import AS.Types.Locations
import AS.Types.Commits
import AS.Types.Messages

import AS.Window

import Data.Aeson
import Data.Text
import Data.Time.Clock (getCurrentTime)
import qualified Database.Redis as R
import qualified Network.WebSockets as WS

import Control.Concurrent (MVar)


-- Deals with server and client stuff

----------------------------------------------------------------------------------------------------------------------------------------------
-- State

data ServerState = State {userClients :: [ASUserClient], daemonClients :: [ASDaemonClient], dbConn :: R.Connection, appPort :: Port}
type Port = Int

----------------------------------------------------------------------------------------------------------------------------------------------
-- Clients

type ClientId = Text

class Client c where
  conn :: c -> WS.Connection
  ownerName :: c -> ASUserId
  clientId :: c -> ClientId
  addClient :: c -> ServerState -> ServerState
  removeClient :: c -> ServerState -> ServerState
  handleServerMessage :: c -> MVar ServerState -> ServerMessage -> IO ()

-- the actual implementations of these in UserClient and DaemonClient will appear in Client.hs

----------------------------------------------------------------------------------------------------------------------------------------------
-- User client

data ASUserClient = UserClient { userId :: ASUserId, userConn :: WS.Connection, userWindow :: ASWindow, sessionId :: ClientId }

instance Eq ASUserClient where
  c1 == c2 = (sessionId c1) == (sessionId c2)

userSheetId :: ASUserClient -> ASSheetId
userSheetId (UserClient _ _ (Window sid _ _) _) = sid

userCommitSource :: ASUserClient -> CommitSource
userCommitSource (UserClient uid _ (Window sid _ _) _) = CommitSource sid uid

updateWindow :: ASWindow -> ASUserClient -> ASUserClient
updateWindow w (UserClient uid conn _ sid) = UserClient uid conn w sid

initUser :: WS.Connection -> ASUserId -> ASSheetId -> IO ASUserClient
initUser c uid sid = do
    time <- getCurrentTime
    return $ UserClient uid c (Window sid (Coord (-1) (-1)) (Coord (-1) (-1))) $ pack ((show uid) ++ (show time))

----------------------------------------------------------------------------------------------------------------------------------------------
-- Daemons

data ASDaemonClient = DaemonClient { daemonConn :: WS.Connection, daemonOwner :: ASUserId, daemonLoc :: ASIndex }

instance Eq ASDaemonClient where
  c1 == c2 = (daemonLoc c1) == (daemonLoc c2)

daemonCommitSource :: ASDaemonClient -> CommitSource
daemonCommitSource (DaemonClient _ uid (Index sid _)) = CommitSource sid uid

initDaemonFromMessageAndConn :: WS.Connection ->  ASUserId -> ASIndex -> ASDaemonClient
initDaemonFromMessageAndConn c uid loc = DaemonClient c uid loc
