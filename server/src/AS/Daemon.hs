module AS.Daemon where

import Prelude
import AS.Types.Cell
import AS.Types.Messages
import AS.Types.Network
import AS.Types.CellProps

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception 
import Control.Monad 
import Control.Monad.Loops
import Control.Concurrent 
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson hiding (Success)
import Data.ByteString.Char8 hiding (putStrLn,filter,any,length)
import Data.ByteString.Lazy.Char8 as B hiding (putStrLn,filter,any,length)
import qualified Network.WebSockets as WS

import AS.Config.Settings as S
import AS.Util as U
import Data.List as L

import Control.Monad
import Data.Default
import Data.Maybe
import System.Posix.Daemon

----------------------------------------------------------------------------------------------------------------------------------------------
-- This module handles daemon creation and management
-- NOTE: for now, "Daemon" is something of a misnomer -- it refers specifically to daemons created 
-- for streaming cells (cells that get re-evaluated at regular intervals), not to daemons generally. 

-- | Returns what the daemon named at location is named / would be named if it existed. 
getDaemonName :: ASIndex -> String
getDaemonName loc = (show loc) ++ "daemon"

getConnByLoc :: ASIndex -> MVar ServerState -> IO (Maybe WS.Connection)
getConnByLoc loc state = do 
  (State users daemons _ _) <- readMVar state
  let daemon = L.filter ((==) loc . daemonLoc) daemons
  case daemon of 
    [] -> return Nothing
    d -> return $ Just $ daemonConn $  L.head d 

-- | If it's something like TODAY() + DATE(), figure that out and automatically make the
-- cell stream. 
-- TODO: implement
getStreamPropFromExpression :: ASExpression -> Maybe Stream
getStreamPropFromExpression _ = Nothing

-- | Creates a streaming daemon for this cell if one of the tags is a streaming tag. 
possiblyCreateDaemon :: MVar ServerState -> ASUserId -> ASCell -> IO ()
possiblyCreateDaemon state owner cell@(Cell loc xp val props _) = do 
  let msg = ServerMessage $ Evaluate [EvalInstruction xp loc]
  case getProp StreamInfoProp props of 
    Nothing -> do 
      let maybeTag = getStreamPropFromExpression xp
      case maybeTag of 
        Nothing -> return ()
        Just tag -> createDaemon state tag loc msg
    Just (StreamInfo s) -> createDaemon state s loc msg

-- | Creates a streaming daemon to regularly update the cell at a location. 
-- Does so by creating client that talks to server, pinging it with the regularity 
-- specified by the user. 
createDaemon :: MVar ServerState -> Stream -> ASIndex -> ServerMessage -> IO ()
createDaemon state s loc msg = do -- msg is the message that the daemon will send to the server regularly
  putStrLn $ "POTENTIALLY CREATING A daemon"
  let name = getDaemonName loc
  putStrLn $ "NAME: " ++ (show name)
  running <- isRunning name
  if (running)
    then return ()
    else do 
      runDetached (Just name) def $ do 
        let daemonId = T.pack $ getDaemonName loc
        let initMsg = ServerMessage $ InitializeDaemon daemonId loc
        port <- appPort <$> readMVar state
        WS.runClient S.wsAddress port "/" $ \conn -> do 
          U.sendMessage initMsg conn
          regularlyReEval s loc msg conn -- is an eval message on the cell
      putStrLn $ "DONE WITH createDaemon"

regularlyReEval :: Stream -> ASIndex -> ServerMessage -> WS.Connection -> IO ()
regularlyReEval (Stream src x) loc msg conn = forever $ do 
  U.sendMessage msg conn
  threadDelay (1000*x) -- microseconds to milliseconds

removeDaemon :: ASIndex -> MVar ServerState -> IO ()
removeDaemon loc state = do 
  let name = getDaemonName loc
  running <- isRunning name
  when running $ do 
    mConn <- getConnByLoc loc state
    WS.sendClose (fromJust mConn) ("Bye" :: Text)
    killAndWait name

-- | Replaces state and stream of daemon at loc, if it exists. If not, create daemon at that location. 
-- (Ideal implementation would look more like modifyUser, but this works for now.)
modifyDaemon :: MVar ServerState -> Stream -> ASIndex -> ServerMessage-> IO ()
modifyDaemon state stream loc msg = (removeDaemon loc state) >> (createDaemon state stream loc msg)
