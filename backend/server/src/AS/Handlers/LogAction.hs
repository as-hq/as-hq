module AS.Handlers.LogAction where

import Control.Lens 
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 as BC (unpack)

import AS.Types.Messages
import AS.Types.Network
import AS.Types.DB

import qualified AS.DB.Users as DU
import qualified AS.Reply as R

-- Add a log to a list in the DB (userId, sessionId) -> LogData
handleLog :: Bool -> ASUserClient -> ServerState -> String -> IO ()
handleLog isAction user state fAction = do 
	let conn = state^.dbConn
	let logSource = LogSource (user^.userId) (user^.userSessionId)
	unless (state^.isDebuggingLog) $ addL conn (LogKey logSource) $ LogValue $ LogData fAction isAction 

handleLogMessage :: ASUserClient -> ServerState -> ByteString -> IO ()
handleLogMessage user state str = handleLog False user state $ BC.unpack str

handleLogAction :: ASUserClient -> ServerState -> String -> IO ()
handleLogAction = handleLog True 

-- Given a logSource, get a list of all LogData's associated with that source (frontend actions and messages)
handleGetSessionLogs :: MessageId -> ASUserClient -> ServerState -> LogSource -> IO ()
handleGetSessionLogs mid user state logSource = do 
	let conn = state^.dbConn 
	logData <- getL conn (LogKey logSource) dbValToLogData
	R.sendToOriginal user $ ClientMessage mid $ SessionLog logData

-- Modify the state to isDebuggingLog = True, which will stop the above log functions from modifying the DB
handleStopLoggingActions :: State -> IO ()
handleStopLoggingActions s = modifyState_ s (return . (isDebuggingLog .~ True))

startLoggingActions :: State -> IO ()
startLoggingActions s = modifyState_ s (return . (isDebuggingLog .~ False))

handleGetAllSessions :: MessageId -> ASUserClient -> IO ()
handleGetAllSessions mid user = do 
	sessions <- DU.getAllSessions 
	R.sendToOriginal user $ ClientMessage mid $ AllSessions sessions

