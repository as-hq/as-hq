{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module AS.Handlers.BugReport where

import Data.Time.Clock.POSIX
import Text.Printf
import Crypto.Hash.SHA1
import Network.HTTP.Conduit
import Data.Aeson
import Control.Lens
import Control.Monad
import Database.Redis 
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import AS.Prelude
import AS.Types.Network
import AS.Types.Messages
import AS.Types.User hiding (userId)
import AS.Types.Commits
import AS.Logging

import AS.DB.Users

------------------------------------------------------------------------------------------------------------------------------------------
-- Maniphest parameters

maniphestCert :: String
maniphestCert = "qgk4ajaoqvhjbb6myeehlhxtlsghwf3nc4re7ltul5kfhmtn6yuerdrh2i77ggw4c7ojvsjspfy4hta4zxd6vemunwi44viufe4iptknxc4mmponzsagsm3pkdv72fy6n3wpk4lzbm26lzopkychngovoifkab7arldizct5fbjy3zfvd32mklfs537bllsz5qyqn6rilkdflklhoxgi5sfa6vhj3yvymxc4ozysnpqvh5uqigbhqf74o626cfq"

maniphestUser :: String
maniphestUser = "RITESH"

maniphestURL :: String
maniphestURL = "https://alphasheets.phacility.com"

------------------------------------------------------------------------------------------------------------------------------------------
-- Auth functions

-- | ByteString to Hex. Not particularly efficient, but should be OK. 
hex :: B.ByteString -> String
hex = concatMap (printf "%02x") . B.unpack

-- | Compute the number of seconds since epoch (UTC). Used as token for maniphest. 
getTimeToken :: IO Int
getTimeToken = round `fmap` getPOSIXTime 

-- | Compute the SHA1 hash of the time since epoch + cert, and return the hexdigest of that. 
-- Used as signature for maniphest.
getSignature :: IO String
getSignature = do 
  time <- getTimeToken
  return $ hex $ hash $ BC.pack $ show time ++ maniphestCert

------------------------------------------------------------------------------------------------------------------------------------------
-- Connecting to conduit (for Phabricator)

data ConduitConnectParams = ConduitConnectParams {
  client :: String,
  clientVersion :: Int, 
  clientDescription :: String, 
  user :: String,
  host :: String,
  authToken :: Int,
  authSignature :: String
} deriving (Generic, Show)

data ConduitConnectResponse = ConduitConnectResponse {
  result :: ConduitConnectResult,
  error_code :: Maybe Int, 
  error_info :: Maybe String
} deriving (Generic, Show)

data ConduitConnectResult = ConduitConnectResult {
  connectionID :: ConduitConnectionID, 
  sessionKey :: ConduitSessionKey, 
  userPHID :: String
} deriving (Generic, Show)

type ConduitSessionKey = String
type ConduitConnectionID = Int

instance ToJSON ConduitConnectParams
instance FromJSON ConduitConnectResult
instance FromJSON ConduitConnectResponse

defaultConduitConnectParams :: IO ConduitConnectParams
defaultConduitConnectParams = do 
  token <- getTimeToken
  sig <- getSignature
  return $ ConduitConnectParams "AS" 0 "AS" maniphestUser maniphestURL token sig

-- Given an API endpoint and the body as a list of tuples, make an HTTP post request and return the body
makePostRequest :: String -> [(B.ByteString, B.ByteString)] -> IO BL.ByteString
makePostRequest endpt body = do 
  initReq <- parseUrl $ maniphestURL ++ endpt
  let req = urlEncodedBody body initReq
  manager <- newManager tlsManagerSettings
  response <- httpLbs req manager
  return $ responseBody response

-- | Connect to conduit using authentication and the conduit.connect endpoint
-- Need to convert Aeson's lazy bytestring into a strict one, but this inefficiency shouldn't matter.
conduitConnect :: IO (Maybe ConduitConnectResponse)
conduitConnect = do 
  params <- defaultConduitConnectParams
  let body = [("params", BL.toStrict $ encode params), ("output", "json"), ("__conduit__", "True")]
  body <- makePostRequest "/api/conduit.connect" body
  return $ decode' body

------------------------------------------------------------------------------------------------------------------------------------------
-- Creating a maniphest task

type TaskTitle = String
type TaskDesc = String

-- | Produce the conduit JSON needed to make a createtask post request. Cannot create a type because the sessionKey field
-- would be overloaded (will be fixed in a newer GHC).
-- Given the sessionKey, connectionId, and the title/description of the task
taskToJSON :: ConduitSessionKey -> ConduitConnectionID -> TaskTitle -> TaskDesc -> String
taskToJSON key id title desc = "{\"title\":\"" ++ title ++ "\", \"description\":\"" ++ desc ++ "\", \"__conduit__\":" ++ conduit ++ "}"
  where conduit = "{\"sessionKey\":\"" ++ key ++ "\", \"connectionID\":"  ++ (show id) ++ "}" 

-- | Get the description of the task from the userClient (sessionId, userId, last sheet). This is useful info for debugging.
getDescription :: ASUserClient -> Connection -> IO TaskDesc
getDescription uc conn = do 
  maybeUser <- lookupUser conn $ uc^.userId
  let uid = T.unpack $ uc^.userId
  let sid = T.unpack $ uc^.userSessionId
  sheet <- case maybeUser of 
    Nothing -> return "No last sheet found"
    Just u  -> return $ T.unpack $ u^.lastOpenSheet
  return $ "User ID: " ++ uid ++ ", Session ID: " ++ sid ++ ", Sheet: " ++ sheet ++ "."

-- | Create a maniphest task by posting to /api/maniphest.createtask. 
createTask :: ConduitSessionKey -> ConduitConnectionID -> TaskTitle -> TaskDesc -> IO ()
createTask key id title desc = do 
  let task = taskToJSON key id title desc
  let body = [("params", BC.pack task), ("output", "json")]
  void $ makePostRequest "/api/maniphest.createtask" body

-- | On receiving a bug report, we log the bug report in server logs, make a maniphest task, 
-- and then send an ack back to frontend
handleBugReport :: ASUserClient -> ServerState -> String -> IO ()
handleBugReport uc state title = do
  putsBugReport (userCommitSource uc) title
  connectResp <- conduitConnect
  case connectResp of 
    Nothing -> return ()
    Just resp -> do 
      desc <- getDescription uc $ state^.dbConn
      createTask (sessionKey $ result resp) (connectionID $ result resp) title desc
  WS.sendTextData (uc^.userConn) ("ACK" :: T.Text)