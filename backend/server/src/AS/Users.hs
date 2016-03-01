module AS.Users where

import Data.Aeson hiding (Success)
import Data.Aeson.Lens (key)
import Network.Wreq
import Control.Monad.IO.Class (liftIO)
import Control.Lens hiding ((.=))
import qualified Data.List as L
import qualified Data.Text as T 
import qualified Data.ByteString.Char8 as B
import qualified Network.WebSockets as WS

import Prelude()
import AS.Prelude
import AS.Config.Settings (google_token_verify_url, google_client_id)
import AS.Types.User hiding (userId)
import AS.Types.Network 
import AS.Types.Messages

-------------------------------------------------------------------------------------------------------------------------
-- Authentication

authenticateUser :: AuthStrategy -> IO (Either String ASUserId)
authenticateUser strat = case strat of 
  GoogleAuth token -> do
    r <- get $ google_token_verify_url ++ (T.unpack token)
    let appClientId = r ^? responseBody . key "aud"
    case appClientId of 
      Just (String clientId) -> do
        -- use the user's email as the unique user identifier
        let uid = (\(String t) -> t) <$> r ^? responseBody . key "email"
        return $ if (T.unpack clientId) /= google_client_id
          then Left "received incorrect app client id"
          else case uid of 
            Just uid -> Right uid
            Nothing -> Left "auth response did not have email field"
      _ -> return $ Left "received null app client id"
  TestAuth -> return $ Right "test_user_id" -- when running tests, no authentication performed.
  
-------------------------------------------------------------------------------------------------------------------------
-- Users management 

-- | Checks 
userIdExists :: ASUserId -> ServerState -> Bool
userIdExists uid state = L.elem uid (map userId (state^.userClients))

getUserBySessionId :: SessionId -> ServerState -> Maybe ASUserClient
getUserBySessionId seshId state = case (filter (\c -> (userSessionId c == seshId)) (state^.userClients)) of
  [] -> Nothing
  l -> Just $ $head l

-- | Applies a (user -> user) function to a user in the server state
modifyUser :: (ASUserClient -> ASUserClient) -> ASUserClient -> State -> IO ()
modifyUser func user state = modifyState_ state $ \state ->
  do 
    let users' = flip map (state^.userClients) (\u -> if (u == user) then (func u) else u)
    return $ state & userClients .~ users'
