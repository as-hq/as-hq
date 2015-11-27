{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Network
  ( module AS.Types.Network
  , module AS.Types.Sheets
  ) where

import AS.Types.Sheets 
import AS.Types.Locations
import AS.Types.DB (CommitSource)
import AS.Types.Messages (ASClientMessage)

import GHC.Generics
import Data.Aeson
import Data.Text
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
  clientId :: c -> ClientId
  clientSheetId :: c -> ASSheetId
  ownerName :: c -> ASUserId
  addClient :: c -> ServerState -> ServerState
  removeClient :: c -> ServerState -> ServerState
  clientCommitSource :: c -> CommitSource
  handleClientMessage :: c -> MVar ServerState -> ASClientMessage -> IO ()

-- the actual implementations of these in UserClient and DaemonClient will appear in Client.hs

----------------------------------------------------------------------------------------------------------------------------------------------
-- User client

data ASUserClient = UserClient {userId :: ASUserId, userConn :: WS.Connection, userWindow :: ASWindow, sessionId :: ClientId}

instance Eq ASUserClient where
  c1 == c2 = (sessionId c1) == (sessionId c2)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Daemons

data ASDaemonClient = DaemonClient {daemonLoc :: ASIndex, daemonConn :: WS.Connection, daemonOwner :: ASUserId}

instance Eq ASDaemonClient where
  c1 == c2 = (daemonLoc c1) == (daemonLoc c2)