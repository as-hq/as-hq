module AS.Users where

import Prelude
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import qualified Data.Text as T 
import qualified Network.WebSockets as WS
import Data.Aeson hiding (Success)

import Database.Redis hiding (decode, Message)

import AS.Types
import AS.Util as U
import AS.Config.Settings as S
import AS.DB.API as DB

-------------------------------------------------------------------------------------------------------------------------
-- Users management 

-- | Get current list of user clients from the server
getUsers :: ServerState -> [ASUser]
getUsers (State us _ _) = us

-- | Checks 
userIdExists :: ASUserId -> ServerState -> Bool
userIdExists uid state = L.elem uid (L.map userId (getUsers state))

-- | Assumes that user exists
getUserById :: ASUserId -> ServerState -> Maybe ASUser
getUserById uid (State allUsers _ _) = case (filter (\c -> (userId c == uid)) (allUsers)) of
  [] -> Nothing
  l -> Just $ L.head l

-- | Applies a (user -> user) function to a user in the server state
modifyUser :: (ASUser -> ASUser) -> ASUser -> MVar ServerState -> IO ()
modifyUser func user state = modifyMVar_ state $ \(State users daemons conn) ->
  do 
    let users' = flip map users (\u -> if (u == user) then (func u) else u)
    return $ State users' daemons conn 