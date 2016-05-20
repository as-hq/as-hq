module AS.Handlers.LogAction where

import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 as BC (unpack)

import AS.Prelude
import AS.Types.Network
import AS.Types.Messages
import AS.Types.DB

import qualified AS.DB.Users as DU
import qualified AS.Reply as R

-- Add a log to a list in the DB (userId, sessionId) -> LogData
handleLog :: MessageContext -> Bool -> String -> IO ()
handleLog msgctx isAction fAction = do 
  let conn = msgctx^.dbConnection
  let uc = msgctx^.userClient
  let logSource = LogSource (uc^.userId) (uc^.userSessionId)
  st <- readContextualState msgctx
  unless (st^.isDebuggingLog) $ addL conn (LogKey logSource) $ LogValue $ LogData fAction isAction 

handleLogMessage :: MessageContext -> ByteString -> IO ()
handleLogMessage msgctx str = handleLog msgctx False $ BC.unpack str

handleLogAction :: MessageContext -> String -> IO ()
handleLogAction msgctx = handleLog msgctx True 

-- Given a logSource, get a list of all LogData's associated with that source (frontend actions and messages)
handleGetSessionLogs :: MessageContext -> LogSource -> IO ()
handleGetSessionLogs msgctx logSource = do 
  logData <- getL (msgctx^.dbConnection) (LogKey logSource) dbValToLogData
  R.sendAction msgctx $ SessionLog logData

-- Modify the state to isDebuggingLog = True, which will stop the above log functions from modifying the DB
handleStopLoggingActions :: State -> IO ()
handleStopLoggingActions s = modifyState_ s (return . (isDebuggingLog .~ True))

startLoggingActions :: State -> IO ()
startLoggingActions s = modifyState_ s (return . (isDebuggingLog .~ False))

handleGetAllSessions :: MessageContext -> IO ()
handleGetAllSessions msgctx = do 
  sessions <- DU.getAllSessions 
  R.sendAction msgctx $ AllSessions sessions

