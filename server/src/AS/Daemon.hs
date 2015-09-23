module AS.Daemon where

import Prelude
import AS.Types

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

-- Daemons
import Control.Monad
import Data.Default
import Data.Maybe
import System.Posix.Daemon

----------------------------------------------------------------------------------------------------------------------------------------------
-- | This module just does daemon creation and management

getDaemonName :: ASLocation -> String
getDaemonName loc = (show loc) ++ ".pid"

getConnByLoc :: ASLocation -> MVar ServerState -> IO (Maybe WS.Connection)
getConnByLoc loc state = do 
  (State users daemons _) <- readMVar state
  let daemon = L.filter (\(ASDaemon l c) -> (l == loc)) daemons
  case daemon of 
		[] -> return Nothing
		d -> return $ Just $ daemonConn $  L.head d 

possiblyCreateDaemon :: ASUser -> MVar ServerState -> ASMessage -> IO ()
possiblyCreateDaemon user state origMsg@(Message _ _ _ (PayloadC (Cell loc xp val ts))) = do 
  case (U.getStreamTag ts) of 
    Nothing -> do 
      let mTag = U.getStreamTagFromExpression xp 
      case mTag of 
        Nothing -> return ()
        Just tag -> addDaemon user state tag loc origMsg
    Just sTag -> addDaemon user state sTag loc origMsg

daemonFunc :: Stream -> ASLocation -> ASMessage -> WS.Connection -> IO ()
daemonFunc (Stream src x) loc msg conn = forever $ do 
  WS.sendTextData conn (encode msg)
  threadDelay (1000*x)
  
addDaemon :: ASUser -> MVar ServerState -> Stream -> ASLocation -> ASMessage -> IO ()
addDaemon user state s loc msg = do 
  putStrLn $ "POTENTIALLY CREATING A DAEMON"
  let name = getDaemonName loc
  putStrLn $ "NAME: " ++ (show name)
  running <- isRunning name
  if (running)
  	then return ()
  	else do 
      runDetached (Just name) def $ do 
        let pId = messageUserId msg
        let initMsg = Message pId Acknowledge NoResult (PayloadDaemonInit (ASInitDaemonConnection pId loc))
        WS.runClient S.wsAddress S.wsPort "/" $ \conn -> do 
          WS.sendTextData conn (encode initMsg)
          daemonFunc s loc msg conn
      putStrLn $ "DONE WITH ADD DAEMON"
       
  						
removeDaemon :: ASLocation -> MVar ServerState -> IO ()
removeDaemon loc state = do 
  let name = getDaemonName loc
  running <- isRunning name
  when running $ do 
    mConn <- getConnByLoc loc state
    WS.sendClose (fromJust mConn) ("Bye" :: Text)
    killAndWait name

modifyDaemon :: ASUser -> MVar ServerState -> Stream -> ASLocation -> ASMessage-> IO ()
modifyDaemon user state stream loc msg = (removeDaemon loc state) >> (addDaemon user state stream loc msg)

