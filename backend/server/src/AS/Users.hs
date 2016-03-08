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
import AS.Config.Settings (google_token_verify_url, google_client_id, getWhitelistedUsers)
import AS.Types.User hiding (userId)
import AS.Types.Network 
import AS.Types.Messages

import Control.Exception

-------------------------------------------------------------------------------------------------------------------------
-- Authentication

authenticateUser :: AuthStrategy -> IO (Either String ASUserId)
authenticateUser strat = case strat of 
  GoogleAuth token -> handle onException $ do
    putStrLn $ "Received user id token: " ++ (T.unpack token)
    r <- get $ google_token_verify_url ++ (T.unpack token)
    let appClientId = r ^? responseBody . key "aud"
    case appClientId of 
      Just (String clientId) -> do
        -- use the user's email as the unique user identifier
        let uid = (\(String t) -> t) <$> r ^? responseBody . key "email"
        if (T.unpack clientId) /= google_client_id
          then return $ Left "received incorrect app client id"
          else case uid of 
            Just uid -> do 
              whitelist <- getWhitelistedUsers
              return $ if uid `elem` whitelist
                then Right uid
                else Left "You have not yet received an invitation to use AlphaSheets"
            Nothing -> return $ Left "auth response did not have email field"
      _ -> return $ Left "received null app client id"
    where
      onException :: SomeException -> IO (Either String ASUserId)
      onException e = return $ Left "connection failure"
  TestAuth -> return $ Right "test_user_id" -- when running tests, no authentication performed.
-------------------------------------------------------------------------------------------------------------------------
-- Users management 

-- | Checks 
userIdExists :: ASUserId -> ServerState -> Bool
userIdExists uid state = L.elem uid (map (view userId) (state^.userClients))

getUserBySessionId :: SessionId -> ServerState -> Maybe ASUserClient
getUserBySessionId seshId state = case (filter (\uc -> (uc^.userSessionId == seshId)) (state^.userClients)) of
  [] -> Nothing
  l -> Just $ $head l

-- | Applies a (user -> user) function to a user in the server state
modifyUserInState :: State -> ASUserId -> (ASUserClient -> ASUserClient) -> IO ()
modifyUserInState state uid f = modifyState_ state $ \state ->
  return $ state & userClients %~ map (\u -> if (u^.userId == uid) then (f u) else u)
