module AS.Types.Network where

import Data.Aeson
--import Data.Text
import Data.Time.Clock (getCurrentTime)
import Control.Concurrent (MVar, ThreadId)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Control.Concurrent 
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Database.Redis as R
import qualified Network.WebSockets as WS

import AS.Prelude
import AS.Types.Sheets 
import AS.Types.Locations
import AS.Types.Commits
import AS.Types.Messages hiding (LogSource)
import AS.Types.User hiding (userId)
import AS.Types.Window

--------------------------------------------------------------------------------
-- User client

data UserClient = UserClient 
  { _userId :: UserID
  , _userConn :: WS.Connection
  , _userWindow :: Window
  , _userSessionId :: SessionId 
  }
 -- userSessionID uniquely identifies a user client
makeLenses ''UserClient

instance Eq UserClient where
  c1 == c2 = (c1^.userSessionId) == (c2^.userSessionId)

userSheetId :: UserClient -> SheetID
userSheetId uc = uc^.userWindow.windowSheetId

userWorkbookId :: UserClient -> WorkbookID
userWorkbookId uc = uc^.userWindow.windowWorkbookId

userCommitSource :: UserClient -> CommitSource
userCommitSource uc = CommitSource (userSheetId uc) (uc^.userId)


--------------------------------------------------------------------------------
-- Daemons

data ASDaemonClient = DaemonClient 
  { daemonConn :: WS.Connection
  , daemonOwner :: UserID
  , daemonLoc :: ASIndex }

instance Eq ASDaemonClient where
  c1 == c2 = (daemonLoc c1) == (daemonLoc c2)

daemonCommitSource :: ASDaemonClient -> CommitSource
daemonCommitSource (DaemonClient _ uid (Index sid _)) = CommitSource sid uid

--------------------------------------------------------------------------------
-- State

-- An API for state that abstracts away MVar.
-- Wrapper for MVar Serverstate, and a small API for accessing States. This way,
-- the rest of the code doesn't have to care about whether we're using MVar/TVar
-- or anything else.

data State = State (MVar ServerState)

readState :: State -> IO ServerState
readState (State m) = readMVar m

modifyState_ :: State -> (ServerState -> IO ServerState) -> IO ()
modifyState_ (State m) = modifyMVar_' m


type ThreadMap = M.Map MessageId ThreadId 

data ServerState = ServerState 
  { _userClients :: [UserClient]
  , _daemonClients :: [ASDaemonClient]
  , _dbConn :: R.Connection
  , _threads :: ThreadMap
  , _isDebuggingLog :: Bool
  } 
makeLenses ''ServerState

emptyServerState :: R.Connection -> ServerState
emptyServerState conn = ServerState 
  { _userClients = [] 
  , _daemonClients = [] 
  , _dbConn = conn 
  , _threads = M.empty
  , _isDebuggingLog = False
  } 

-- | Applies a (user -> user) function to a user in the server state
modifyUserClientInState :: State -> SessionId -> (UserClient -> UserClient) -> IO ()
modifyUserClientInState state seshId f = modifyState_ state $ \state ->
  return $ state & userClients %~ map (\u -> if (u^.userSessionId == seshId) then (f u) else u)

--------------------------------------------------------------------------------
-- Messages

data MessageContext = MessageContext 
  { _messageState :: State
  , _messageId :: MessageId 
  , _userClient :: UserClient
  , _dbConnection :: R.Connection
  }

makeLenses ''MessageContext

readContextualState :: MessageContext -> IO ServerState
readContextualState = readState . view messageState

messageCommitSource :: MessageContext -> CommitSource
messageCommitSource ctx = CommitSource  { srcSheetId = messageSheetId ctx
                                        , srcUserId = ctx^.userClient.userId
                                        }

messageUserId :: MessageContext -> UserID
messageUserId ctx = ctx^.userClient.userId

messageSheetId :: MessageContext -> SheetID
messageSheetId ctx = ctx^.userClient.userWindow.windowSheetId

messageWorkbookId :: MessageContext -> WorkbookID
messageWorkbookId ctx = ctx^.userClient.userWindow.windowWorkbookId

--------------------------------------------------------------------------------
-- Clients

class Client c where
  clientType :: c -> ClientType
  clientConn :: c -> WS.Connection
  ownerName :: c -> UserID
  sessionId :: c -> SessionId
  addClient :: c -> ServerState -> ServerState
  removeClient :: c -> ServerState -> ServerState
  lookupClient :: c -> ServerState -> c
  handleServerMessage :: c -> State -> ServerMessage -> IO ()
  clientCommitSource :: c -> CommitSource

-- The actual implementations of these in UserClient and DaemonClient 
-- will appear in Client.hs

--------------------------------------------------------------------------------
-- Other

type Seconds = Int
type Milliseconds = Int

data ClientType = UserType | DaemonType

--------------------------------------------------------------------------------

