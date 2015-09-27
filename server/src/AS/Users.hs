module AS.Users where

import Prelude
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import qualified Data.Text as T 
import qualified Network.WebSockets as WS
import Data.Aeson hiding (Success)
import AS.Types.Core

-------------------------------------------------------------------------------------------------------------------------
-- Users management 

-- | Get current list of user clients from the server
getUsers :: ServerState -> [ASUser]
getUsers (State us _ _) = us

-- | Checks 
userIdExists :: ASUserId -> ServerState -> Bool
userIdExists uid state = L.elem uid (L.map userId (getUsers state))

getUserByClientId :: ClientId -> ServerState -> Maybe ASUser
getUserByClientId sid (State allUsers _ _) = case (filter (\c -> (sessionId c == sid)) (allUsers)) of
  [] -> Nothing
  l -> Just $ L.head l

-- | Applies a (user -> user) function to a user in the server state
modifyUser :: (ASUser -> ASUser) -> ASUser -> MVar ServerState -> IO ()
modifyUser func user state = modifyMVar_ state $ \(State users DaemonClients conn) ->
  do 
    let users' = flip map users (\u -> if (u == user) then (func u) else u)
    return $ State users' DaemonClients conn 