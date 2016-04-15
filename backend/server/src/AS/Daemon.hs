{-# LANGUAGE OverloadedStrings #-}
module AS.Daemon where

import AS.Prelude

import AS.Config.Constants
import AS.Types.Cell
import AS.Types.Messages
import AS.Types.Network
import AS.Types.CellProps
import AS.Types.User

import Control.Exception 
import Control.Monad 
import Control.Monad.Loops
import Control.Concurrent 
import Control.Monad.IO.Class (liftIO)

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson hiding (Success)
import qualified Network.WebSockets as WS

import AS.Config.Settings as S
import AS.Util as U

import Control.Monad
import Control.Lens hiding ((.=))
import Data.Default
import System.Posix.Daemon

----------------------------------------------------------------------------------------------------------------------------------------------
-- This module handles daemon creation and management
-- NOTE: for now, "Daemon" is something of a misnomer -- it refers specifically to daemons created 
-- for streaming cells (cells that get re-evaluated at regular intervals), not to daemons generally. 

-- | Returns what the daemon named at location is named / would be named if it existed. 
getDaemonName :: ASIndex -> String
getDaemonName loc = (show loc) ++ "daemon"

getConnByLoc :: ASIndex -> ServerState -> IO (Maybe WS.Connection)
getConnByLoc loc state = do 
  let daemon = filter ((==) loc . daemonLoc) (state^.daemonClients)
  case daemon of 
    [] -> return Nothing
    d -> return $ Just $ daemonConn $ $head d 

-- | If it's something like TODAY() + DATE(), figure that out and automatically make the
-- cell stream. 
-- TODO: implement
getStreamPropFromExpression :: ASExpression -> Maybe Stream
getStreamPropFromExpression _ = Nothing

-- | Creates a streaming daemon for this cell if one of the tags is a streaming tag. 
possiblyCreateDaemon :: ServerState -> ASUserId -> ASCell -> IO ()
possiblyCreateDaemon state owner cell = 
  let xp = cell^.cellExpression
      loc = cell^.cellLocation
      msg = ServerMessage daemon_message_id $ Evaluate [EvalInstruction xp loc] -- this ServerMessage has no origin message Id, so give it a default
  in case getProp StreamInfoProp (cell^.cellProps) of 
    Nothing -> case (getStreamPropFromExpression xp) of 
      Nothing -> return ()
      Just tag -> createDaemon state tag loc msg
    Just (StreamInfo s) -> createDaemon state s loc msg

-- | Creates a streaming daemon to regularly update the cell at a location. 
-- Does so by creating client that talks to server, pinging it with the regularity 
-- specified by the user. 
-- not supported right now (anand 3/7) 
createDaemon :: ServerState -> Stream -> ASIndex -> ServerMessage -> IO ()
createDaemon state s loc msg = $undefined

removeDaemon :: ASIndex -> ServerState -> IO ()
removeDaemon loc state = do 
  let name = getDaemonName loc
  running <- isRunning name
  when running $ do 
    mConn <- getConnByLoc loc state
    WS.sendClose ($fromJust mConn) ("Bye" :: Text)
    killAndWait name

-- | Replaces state and stream of daemon at loc, if it exists. If not, create daemon at that location. 
-- (Ideal implementation would look more like modifyUser, but this works for now.)
modifyDaemon :: ServerState -> Stream -> ASIndex -> ServerMessage-> IO ()
modifyDaemon state stream loc msg = (removeDaemon loc state) >> (createDaemon state stream loc msg)
