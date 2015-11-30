{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Network
  ( module AS.Types.Network
  , module AS.Types.Sheets
  ) where

import AS.Types.Sheets 
import AS.Types.Locations
import AS.Types.DB (CommitSource)
import AS.Types.Messages

import AS.Window

import GHC.Generics
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
  handleClientMessage :: c -> MVar ServerState -> ASClientMessage -> IO ()

-- the actual implementations of these in UserClient and DaemonClient will appear in Client.hs

----------------------------------------------------------------------------------------------------------------------------------------------
-- User client

data ASUserClient = UserClient {userId :: ASUserId, userConn :: WS.Connection, userWindow :: ASWindow, sessionId :: ClientId}

instance Eq ASUserClient where
  c1 == c2 = (sessionId c1) == (sessionId c2)

userSheetId :: ASUserClient -> ASSheetId
userSheetId (UserClient _ _ (Window sid _ _) _) = sid

userCommitSource :: ASUserClient -> CommitSource
userCommitSource (UserClient uid _ (Window sid _ _) _) = (sid, uid)

updateWindow :: ASWindow -> ASUserClient -> ASUserClient
updateWindow w (UserClient uid conn _ sid) = UserClient uid conn w sid

initUserFromMessageAndConn :: ASClientMessage -> WS.Connection -> IO ASUserClient
initUserFromMessageAndConn (ClientMessage _ (PayloadInit (ASInitConnection uid sid))) c = do
    time <- getCurrentTime
    return $ UserClient uid c (Window sid (-1,-1) (-1,-1)) $ pack ((show uid) ++ (show time))

----------------------------------------------------------------------------------------------------------------------------------------------
-- Daemons

data ASDaemonClient = DaemonClient {daemonLoc :: ASIndex, daemonConn :: WS.Connection, daemonOwner :: ASUserId}

instance Eq ASDaemonClient where
  c1 == c2 = (daemonLoc c1) == (daemonLoc c2)

initDaemonFromMessageAndConn :: ASClientMessage -> WS.Connection -> ASDaemonClient
initDaemonFromMessageAndConn (ClientMessage _ (PayloadDaemonInit (ASInitDaemonConnection uid loc))) c = DaemonClient loc c uid