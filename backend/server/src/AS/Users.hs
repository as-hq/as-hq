module AS.Users where

import Data.Aeson hiding (Success)
import Data.Aeson.Lens (key)
import Network.Wreq
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import qualified Data.Text as T 
import qualified Data.ByteString.Char8 as B
import qualified Network.WebSockets as WS

import AS.Prelude
import AS.Util
import AS.Logging
import AS.Config.Settings (google_token_verify_url, google_client_id, getWhitelistedUsers)
import AS.Types.User hiding (userId)
import AS.Types.Network 
import AS.Types.Messages

import AS.DB.Users (produceUser)

import Control.Exception

import Database.Redis (Connection)

-------------------------------------------------------------------------------------------------------------------------
-- Authentication

authenticateUser :: Connection -> AuthStrategy -> IO (Either String User)
authenticateUser conn strat = case strat of 
  GoogleAuth token -> handleAny (const . return $ Left "connection failure") $ do
    putsObj "Received user id token: " token
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
              if uid `elem` whitelist
                then Right <$> produceUser conn uid
                else return $ Left "You have not yet received an invitation to use AlphaSheets"
            Nothing -> return $ Left "auth response did not have email field"
      _ -> return $ Left "received null app client id"
  -- a randomly generated, unique user id. Ensures that a publicly-referred user has access only to the sheet she was referred to.
  PublicAuth -> return . Right =<< produceUser conn =<< return . T.pack =<< getUUID 
  -- when running tests, no authentication performed.
  TestAuth -> Right <$> produceUser conn "test_user_id" 
-------------------------------------------------------------------------------------------------------------------------
-- Users management 

-- | Checks 
userIdExists :: UserID -> ServerState -> Bool
userIdExists uid state = L.elem uid (map (view userId) (state^.userClients))

getUserClientBySessionId :: SessionId -> ServerState -> Maybe UserClient
getUserClientBySessionId seshId state = headMay $ 
  filter ((== seshId) . view userSessionId) (state^.userClients)

-- | Applies a (user -> user) function to a user in the server state
modifyUserClientInState :: State -> SessionId -> (UserClient -> UserClient) -> IO ()
modifyUserClientInState state seshId f = modifyState_ state $ \state ->
  return $ state & userClients %~ map (\u -> if (u^.userSessionId == seshId) then (f u) else u)
