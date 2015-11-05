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

-- | Checks 
userIdExists :: ASUserId -> ServerState -> Bool
userIdExists uid state = L.elem uid (L.map userId (userClients state))

getUserByClientId :: ClientId -> ServerState -> Maybe ASUserClient
getUserByClientId sid (State allUsers _ _ _) = case (filter (\c -> (sessionId c == sid)) (allUsers)) of
  [] -> Nothing
  l -> Just $ L.head l

-- | Applies a (user -> user) function to a user in the server state
modifyUser :: (ASUserClient -> ASUserClient) -> ASUserClient -> MVar ServerState -> IO ()
modifyUser func user state = modifyMVar_ state $ \(State users daemons conn port) ->
  do 
    let users' = flip map users (\u -> if (u == user) then (func u) else u)
    return $ State users' daemons conn port