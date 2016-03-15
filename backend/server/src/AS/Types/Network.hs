module AS.Types.Network
  ( module AS.Types.Network
  , module AS.Types.Sheets
  ) where

import AS.Prelude
import Prelude()

import AS.Types.Sheets 
import AS.Types.Locations
import AS.Types.Commits
import AS.Types.Messages
import AS.Types.User
import AS.Types.Window

import Data.Aeson
import Data.Text
import Data.Time.Clock (getCurrentTime)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Database.Redis as R
import qualified Network.WebSockets as WS

import Control.Concurrent (MVar, ThreadId)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import Control.Lens hiding ((.=))
import Control.Concurrent (MVar, ThreadId, newMVar, modifyMVar_, takeMVar, readMVar, putMVar, newEmptyMVar)


-- Deals with server and client stuff

----------------------------------------------------------------------------------------------------------------------------------------------
-- Daemons

data ASDaemonClient = DaemonClient { daemonConn :: WS.Connection, daemonOwner :: ASUserId, daemonLoc :: ASIndex }

instance Eq ASDaemonClient where
  c1 == c2 = (daemonLoc c1) == (daemonLoc c2)

daemonCommitSource :: ASDaemonClient -> CommitSource
daemonCommitSource (DaemonClient _ uid (Index sid _)) = CommitSource sid uid

initDaemonFromMessageAndConn :: WS.Connection ->  ASUserId -> ASIndex -> ASDaemonClient
initDaemonFromMessageAndConn c uid loc = DaemonClient c uid loc

----------------------------------------------------------------------------------------------------------------------------------------------
-- State

-- An API for state that abstracts away MVar.
-- Wrapper for MVar Serverstate, and a small API for accessing States. This way,
-- the rest of the code doesn't have to care about whether we're using MVar or TVar
-- or anything else.

data State = State (MVar ServerState)

readState :: State -> IO ServerState
readState (State m) = readMVar m

modifyState_ :: State -> (ServerState -> IO ServerState) -> IO ()
modifyState_ (State m) = modifyMVar_ m

type ThreadMap = M.Map MessageId ThreadId 

data ServerState = ServerState { _userClients :: [ASUserClient]
                          , _daemonClients :: [ASDaemonClient]
                          , _dbConn :: R.Connection
                          , _threads :: ThreadMap
                          , _isDebuggingLog :: Bool}
-- #needsrefactor rename isDebuggingLog to stopLoggingMessages or something. The idea is that in "dev debug"
-- mode, you shouldn't be logging replays. 

emptyServerState :: R.Connection -> ServerState
emptyServerState conn = ServerState [] [] conn M.empty False 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Clients

class Client c where
  clientType :: c -> ClientType
  clientConn :: c -> WS.Connection
  ownerName :: c -> ASUserId
  sessionId :: c -> SessionId
  addClient :: c -> ServerState -> ServerState
  removeClient :: c -> ServerState -> ServerState
  lookupClient :: c -> ServerState -> c
  handleServerMessage :: c -> State -> ServerMessage -> IO ()

-- the actual implementations of these in UserClient and DaemonClient will appear in Client.hs

type Seconds = Int
type Milliseconds = Int

data ClientType = UserType | DaemonType

----------------------------------------------------------------------------------------------------------------------------------------------
-- User client

data ASUserClient = UserClient { _userId :: ASUserId, _userConn :: WS.Connection, _userWindow :: ASWindow, _userSessionId :: SessionId } -- userSessionID uniquely identifies a user client
makeLenses ''ASUserClient

instance Eq ASUserClient where
  c1 == c2 = (c1^.userSessionId) == (c2^.userSessionId)

userSheetId :: ASUserClient -> ASSheetId
userSheetId (UserClient _ _ (Window sid _ _) _) = sid

userCommitSource :: ASUserClient -> CommitSource
userCommitSource (UserClient uid _ (Window sid _ _) _) = CommitSource sid uid

makeLenses ''ServerState