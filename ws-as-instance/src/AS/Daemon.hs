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

daemonExists :: ASLocation -> MVar ServerState -> IO Bool
daemonExists loc state = do 
	(State us ls) <- readMVar state
	return $ L.elem loc (L.map fst ls)

getConnByLoc :: ASLocation -> MVar ServerState -> IO (Maybe WS.Connection)
getConnByLoc loc state = do 
	(State us ls)<- readMVar state
	let result = L.find (\(a,b)-> a == loc) ls
	case result of 
		Nothing -> return Nothing
		Just (l,c) -> return $ Just c

removeLocFromState :: ASLocation -> MVar ServerState -> IO ()
removeLocFromState loc state = liftIO $ modifyMVar_ state $ \(State us ls) -> 
	return $ State us (L.filter (\(a,b) -> (a /= loc)) ls)


possiblyCreateDaemon :: ASUser -> MVar ServerState -> ASMessage -> IO ()
possiblyCreateDaemon user state origMsg@(Message _ _ _ (PayloadC (Cell loc xp val ts))) = do 
	case (U.getStreamTag ts) of 
	    Nothing -> do 
	      let mTag = U.getStreamTagFromExpression xp 
	      case mTag of 
	        Nothing -> return ()
	        Just tag -> addDaemon user state tag loc origMsg
	    Just sTag -> addDaemon user state sTag loc origMsg
	      
addDaemon :: ASUser -> MVar ServerState -> Stream -> ASLocation -> ASMessage -> IO ()
addDaemon user state (Stream src x) loc msg = do 
  putStrLn $ "POTENTIALLY CREATING A DAEMON"
  let name = getDaemonName loc
  exists <- daemonExists loc state
  if exists
  	then return ()
  	else do 
  		-- | Use the same message id so that the "real" client can be traced
  		let initMsg = Message (messageUserId msg) Acknowledge NoResult (PayloadInit (ASInitConnection (T.pack name)))
  		WS.runClient S.wsAddress S.wsPort "/" $ \conn -> do 
  			-- | Add location to list of streaming locations
  			(State users locs) <- readMVar state
  			liftIO $ modifyMVar_ state (\(State us ls) -> return $ State us ((loc,conn):ls))
  			WS.sendTextData conn (encode initMsg)
  			initBool <- daemonExists loc state
  			putStrLn $ "NEW CLIENT SENT INITIAL MESSAGE WITH INITIAL LOOP BOOL: " ++ (show initBool)
  			WS.sendTextData conn (encode msg) -- needed
  			runDetached (Just name) def $ whileM_ (daemonExists loc state) $ do 
  				threadDelay (1000*x)
  				WS.sendTextData conn (encode msg)
  			-- | At this point the streaming has been stopped, so send close message to server
  			putStrLn $ "NOT RUNNING DAEMON ANYMORE"
  			WS.sendClose conn ("Bye!" :: Text)
				

removeDaemon :: ASLocation -> MVar ServerState -> IO ()
removeDaemon loc state = do 
	putStrLn $ "POTENTIALLY REMOVING A DAEMON"
	exists <- daemonExists loc state 
	if exists
		then do 
			kill (getDaemonName loc)
			mConn <- getConnByLoc loc state
			--  | Send close message to server
			WS.sendClose (fromJust mConn) ("Bye!" :: Text)
			-- | Removing the location from list of daemon locations will end the whileM_ in addDaemon
			removeLocFromState loc state 
			bool <- daemonExists loc state
			putStrLn $ "KILLED BOOL: " ++ (show bool)
		else return ()

modifyDaemon :: ASUser -> MVar ServerState -> Stream -> ASLocation -> ASMessage-> IO ()
modifyDaemon user state stream loc msg = (removeDaemon loc state) >> (addDaemon user state stream loc msg)

