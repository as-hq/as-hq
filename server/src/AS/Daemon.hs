module AS.DaemonClient where

import Prelude
import AS.Types.Core

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

-- DaemonClients
import Control.Monad
import Data.Default
import Data.Maybe
import System.Posix.DaemonClient

----------------------------------------------------------------------------------------------------------------------------------------------
-- This module handles DaemonClient creation and management

-- | Returns what the DaemonClient named at ASLocation is named / would be named if it existed. 
getDaemonClientName :: ASLocation -> String
getDaemonClientName loc = (show loc) ++ "DaemonClient"

getConnByLoc :: ASLocation -> MVar ServerState -> IO (Maybe WS.Connection)
getConnByLoc loc state = do 
  (State users DaemonClients _) <- readMVar state
  let DaemonClient = L.filter (\(DaemonClient l _ _) -> (l == loc)) DaemonClients
  case DaemonClient of 
		[] -> return Nothing
		d -> return $ Just $ DaemonClientConn $  L.head d 

-- | Creates a streaming DaemonClient for this cell if one of the tags is a streaming tag. 
possiblyCreateDaemonClient :: MVar ServerState -> ASUserId -> ASCell -> IO ()
possiblyCreateDaemonClient state owner cell@(Cell loc xp val ts) = do 
  let msg = ClientMessage Evaluate (PayloadC cell)
  case (U.getStreamTag ts) of 
    Nothing -> do 
      let maybeTag = U.getStreamTagFromExpression xp 
      case maybeTag of 
        Nothing -> return ()
        Just tag -> createDaemonClient state tag loc msg
    Just sTag -> createDaemonClient state sTag loc msg

-- | Creates a streaming DaemonClient to regularly update the cell at a location. 
-- Does so by creating client that talks to server, pinging it with the regularity 
-- specified by the user. 
createDaemonClient :: MVar ServerState -> Stream -> ASLocation -> ASClientMessage -> IO ()
createDaemonClient state s loc msg = do -- msg is the message that the DaemonClient will send to the server regularly
  putStrLn $ "POTENTIALLY CREATING A DaemonClient"
  let name = getDaemonClientName loc
  putStrLn $ "NAME: " ++ (show name)
  running <- isRunning name
  if (running)
  	then return ()
  	else do 
      runDetached (Just name) def $ do 
        let DaemonClientId = T.pack $ getDaemonClientName loc
        let initMsg = ClientMessage Acknowledge (PayloadDaemonClientInit (ASInitDaemonClientConnection DaemonClientId loc))
        WS.runClient S.wsAddress S.wsPort "/" $ \conn -> do 
          U.sendMessage initMsg conn
          regularlyReEval s loc msg conn -- is an eval message on the cell
      putStrLn $ "DONE WITH createDaemonClient"

regularlyReEval :: Stream -> ASLocation -> ASClientMessage -> WS.Connection -> IO ()
regularlyReEval (Stream src x) loc msg conn = forever $ do 
  U.sendMessage msg conn
  threadDelay (1000*x) -- microseconds to milliseconds

removeDaemonClient :: ASLocation -> MVar ServerState -> IO ()
removeDaemonClient loc state = do 
  let name = getDaemonClientName loc
  running <- isRunning name
  when running $ do 
    mConn <- getConnByLoc loc state
    WS.sendClose (fromJust mConn) ("Bye" :: Text)
    killAndWait name

-- | Replaces state and stream of DaemonClient at loc, if it exists. If not, create DaemonClient at that location. 
-- (Ideal implementation would look more like modifyUser, but this works for now.)
modifyDaemonClient :: MVar ServerState -> Stream -> ASLocation -> ASClientMessage-> IO ()
modifyDaemonClient state stream loc msg = (removeDaemonClient loc state) >> (createDaemonClient state stream loc msg)
