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

import AS.Window

import Data.Aeson
import Data.Text
import Data.Time.Clock (getCurrentTime)
import qualified Data.Map as M
import qualified Database.Redis as R
import qualified Network.WebSockets as WS

import Control.Lens hiding ((.=))
import Control.Concurrent (MVar, ThreadId, newMVar, modifyMVar_, takeMVar, readMVar, putMVar, newEmptyMVar)


-- Deals with server and client stuff

----------------------------------------------------------------------------------------------------------------------------------------------
-- State

-- An API for state that abstracts away MVar.

data State = State (MVar ServerState)

readState :: State -> IO ServerState
readState (State m) = readMVar m

modifyState_ :: State -> (ServerState -> IO ServerState) -> IO ()
modifyState_ (State m) = modifyMVar_ m

data ServerState = ServerState  { _userClients :: [ASUserClient]
                          , _daemonClients :: [ASDaemonClient]
                          , _dbConn :: R.Connection
                          , _appSettings :: AppSettings
                          , _threads :: ThreadMap}
emptyServerState :: R.Connection -> AppSettings -> ServerState
emptyServerState conn settings = ServerState [] [] conn settings M.empty

-- Wrapper for MVar Serverstate, and a small API for accessing States. This way,
-- the rest of the code doesn't have to care about whether we're using MVar or TVar
-- or anything else.

type ThreadMap = M.Map MessageId ThreadId 

data AppSettings = AppSettings  { _backendWsAddress :: WsAddress
                                , _backendWsPort :: Port
                                , _graphDbAddress :: GraphAddress
                                , _pyKernelAddress :: KernelAddress
                                , _redisPort :: Port
                                , _redisHost :: Host
                                , _shouldWriteToConsole :: Bool
                                , _shouldWriteToSlack :: Bool}
                                deriving (Show)

type Host = String
type Port = Int
type GraphAddress = String
type KernelAddress = String
type WsAddress = String

-- default values represent what should happen on localhost; the Environment.json values *should* 
-- all be set remotely. 
instance FromJSON AppSettings where
  parseJSON (Object v) = do
    wsAddr <- v .: "backendWsAddress"
    wsPort <- v .: "backendWsPort"
    graphAddr <- v .: "graphDbAddress_haskell"
    pyAddr <- v .: "pyKernelAddress_haskell"
    redisPort <- v .: "redisPort"
    redisHost <- v .: "redisHost"
    shouldWriteToConsole <- v .: "shouldWriteToConsole"
    shouldWriteToSlack <- v .: "shouldWriteToSlack"
    return $ AppSettings wsAddr wsPort graphAddr pyAddr redisPort redisHost shouldWriteToConsole shouldWriteToSlack
  parseJSON _ = $error "expected environment to be an object"

----------------------------------------------------------------------------------------------------------------------------------------------
-- Clients

type ClientId = Text

class Client c where
  clientType :: c -> ClientType
  clientConn :: c -> WS.Connection
  ownerName :: c -> ASUserId
  clientId :: c -> ClientId
  addClient :: c -> ServerState -> ServerState
  removeClient :: c -> ServerState -> ServerState
  handleServerMessage :: c -> State -> ServerMessage -> IO ()

-- the actual implementations of these in UserClient and DaemonClient will appear in Client.hs

type Seconds = Int
type Milliseconds = Int

----------------------------------------------------------------------------------------------------------------------------------------------
-- User client

data ClientType = User | Daemon

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

makeLenses ''ServerState
makeLenses ''AppSettings
