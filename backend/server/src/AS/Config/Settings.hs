-- these optimizations prevent global variable configuration in 
-- Config/Settings.hs due to CSE and floating let's
-- http://stackoverflow.com/questions/19371636/am-i-abusing-unsafeperformio (second answer)
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
                        

module AS.Config.Settings where

import AS.Prelude
import AS.Types.Network
import AS.Types.Logging
import AS.Types.Cell
import AS.Types.User 

import System.Directory
import System.FilePath.Posix
import System.IO.Unsafe (unsafePerformIO)

import Data.Aeson hiding (Success)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BC
import Control.Exception
import Control.Lens hiding ((.=))

import Control.Concurrent.Chan

import GHC.Conc (getNumProcessors, setNumCapabilities)

--------------------------------------------------------------------------------------
-- Types

data AppSettings = AppSettings  { _backendWsAddress' :: String
                                , _backendWsPort' :: Int
                                , _graphDbAddress' :: String
                                , _pyKernelAddress' :: String
                                , _rKernelAddress_server' :: String
                                , _rKernelAddress_client' :: String
                                , _redisPort' :: Int
                                , _redisHost' :: String
                                , _redisPassword' :: Maybe BC.ByteString
                                , _backendLogsOn' :: Bool
                                , _rkernelLogsOn' :: Bool
                                , _slackLogsOn' :: Bool
                                , _diagnosticsPort' :: Int}
                                deriving (Show)

-- default values represent what should happen on localhost; the Environment.json values *should* 
-- all be set remotely. 
instance FromJSON AppSettings where
  parseJSON (Object v) = AppSettings 
    <$> v .: "backendWsAddress"
    <*> v .: "backendWsPort"
    <*> v .: "graphDbAddress_haskell"
    <*> v .: "pyKernelAddress_haskell"
    <*> v .: "rKernelAddress_server"
    <*> v .: "rKernelAddress_client"
    <*> v .: "redisPort"
    <*> v .: "redisHost"
    <*> ((\x -> BC.pack <$> x) <$> (v .:? "redisPassword"))
    <*> v .: "backendLogsOn"
    <*> v .: "rkernelLogsOn"
    <*> v .: "slackLogsOn"
    <*> v .: "diagnosticsPort"

  parseJSON _ = $error "expected environment to be an object"

makeLenses ''AppSettings

--------------------------------------------------------------------------------------
-- unchanging parameters

-- For debugging purposes, the app behaves differently at various points in the code.
-- if you see (if isDebug), behavior forks.
isDebug :: Bool
isDebug = True

largeSearchBound :: Int
largeSearchBound = 1000

ignoredErrorMessages :: [String]
ignoredErrorMessages = ["Thread killed by Warp's timeout reaper", 
                        "receiveloop: timeout (Connection timed out)", 
                        "receiveloop: resource vanished (Connection reset by peer)",
                        "receiveloop: does not exist (No route to host)",
                        "send: resource vanished (Broken pipe)",
                        "ConnectionClosed",
                        "CloseRequest 1000 \"\"",
                        "CloseRequest 1001 \"\""]

headerLangs :: [ASLanguage]
headerLangs = [Python, R] 

heartbeat_interval :: Milliseconds
heartbeat_interval = 1000

server_timeout :: Seconds
server_timeout = 10000

process_message_timeout :: Seconds
process_message_timeout = 3

log_truncate_length :: Int
log_truncate_length = 400

google_token_verify_url :: String
google_token_verify_url = "https://www.googleapis.com/oauth2/v3/tokeninfo?id_token="

google_client_id :: String
google_client_id = "347875438909-e81ep6ofitkq4deio3kagakpr5ujeh20.apps.googleusercontent.com"

-- #needsrefactor should move to Environment.hs, or something
shouldPreprocess :: Bool
shouldPreprocess = False

images_dir      = "../server/static/images/"
eval_dir        = "eval_files/"
env_path        = ".." </> "Environment.json"
server_logs_dir = "./server_logs/"
rkernel_logs_dir = "./rkernel_logs/"
static_dir      = "static/"
whitelist_path  = ".." </> "email_whitelist.txt"

--------------------------------------------------------------------------------------
-- configuration-dependent parameters

alphaMain a = initializeSettings >> a

writeSetting :: IORef a -> a -> IO ()
writeSetting = writeIORef

getSetting :: IORef a -> IO a
getSetting = readIORef

getLogger :: IO Logger
getLogger = $fromJust <$> getSetting logger

-- !!!!!!MUST BE CALLED UPON APP START!!!!!!
initializeSettings :: IO ()
initializeSettings = do
  procs <- (max 1 . (-1 +)  <$> getNumProcessors)
  setNumCapabilities procs
  putStrLn $ "[INFO] using # cores : " ++ show procs

  appSettings <- getRuntimeSettings 
  appDir <- getCurrentDirectory <++> return "/"

  writeSetting appDirectory appDir
  writeSetting slackLogsOn (appSettings^.slackLogsOn')
  writeSetting backendLogsOn (appSettings^.backendLogsOn')
  writeSetting rkernelLogsOn (appSettings^.rkernelLogsOn')
  writeSetting graphAddress (appSettings^.graphDbAddress')
  writeSetting pykernelAddress (appSettings^.pyKernelAddress')
  writeSetting rkernelAddress_server (appSettings^.rKernelAddress_server')
  writeSetting rkernelAddress_client (appSettings^.rKernelAddress_client')
  writeSetting serverHost (appSettings^.backendWsAddress')
  writeSetting serverPort (appSettings^.backendWsPort')
  writeSetting dbHost (appSettings^.redisHost')
  writeSetting dbPort (appSettings^.redisPort')
  writeSetting dbPassword (appSettings^.redisPassword')
  writeSetting ekgPort (appSettings^.diagnosticsPort')

  -- init logger
  chan <- newChan
  writeSetting logger (Just chan) 

  putStrLn . ("[CONFIG] appDirectory : " ++) . show =<< getSetting appDirectory
  putStrLn . ("[CONFIG] slackLogsOn : " ++) . show =<< getSetting slackLogsOn
  putStrLn . ("[CONFIG] backendLogsOn : " ++) . show =<< getSetting backendLogsOn
  putStrLn . ("[CONFIG] rkernelLogsOn : " ++) . show =<< getSetting rkernelLogsOn
  putStrLn . ("[CONFIG] graphAddress : " ++) . show =<< getSetting graphAddress
  putStrLn . ("[CONFIG] pykernelAddress : " ++) . show=<< getSetting pykernelAddress
  putStrLn . ("[CONFIG] rkernelAddress_server : " ++) . show =<< getSetting rkernelAddress_server
  putStrLn . ("[CONFIG] rkernelAddress_client : " ++) . show =<< getSetting rkernelAddress_client
  putStrLn . ("[CONFIG] serverHost : " ++) . show =<< getSetting serverHost
  putStrLn . ("[CONFIG] serverPort : " ++) . show =<< getSetting serverPort
  putStrLn . ("[CONFIG] dbHost : " ++) . show =<< getSetting dbHost
  putStrLn . ("[CONFIG] dbPort : " ++) . show =<< getSetting dbPort
  putStrLn . ("[CONFIG] dbPassword : " ++) . show =<< getSetting dbPassword 
  putStrLn . ("[CONFIG] diagnosticsPort : " ++) . show =<< getSetting ekgPort 
  return ()

appDirectory :: IORef String
appDirectory = declareGlobal "SETTING_NOT_INITIALIZED"

slackLogsOn :: IORef Bool
slackLogsOn = declareGlobal False

backendLogsOn :: IORef Bool
backendLogsOn = declareGlobal False

rkernelLogsOn :: IORef Bool
rkernelLogsOn = declareGlobal False

graphAddress :: IORef String
graphAddress = declareGlobal "SETTING_NOT_INITIALIZED"

pykernelAddress :: IORef String
pykernelAddress = declareGlobal "SETTING_NOT_INITIALIZED"

rkernelAddress_server :: IORef String
rkernelAddress_server  = declareGlobal "SETTING_NOT_INITIALIZED"

rkernelAddress_client :: IORef String
rkernelAddress_client  = declareGlobal "SETTING_NOT_INITIALIZED"

serverHost :: IORef String
serverHost = declareGlobal "SETTING_NOT_INITIALIZED"

serverPort :: IORef Int
serverPort = declareGlobal (-1)

dbHost :: IORef String
dbHost = declareGlobal "SETTING_NOT_INITIALIZED"

dbPort :: IORef Int
dbPort = declareGlobal (-1)

dbPassword :: IORef (Maybe BC.ByteString)
dbPassword = declareGlobal Nothing

ekgPort :: IORef Int
ekgPort = declareGlobal (-1)

logger :: IORef (Maybe Logger)
logger = declareGlobal Nothing

--------------------------------------------------------------------------------------
-- private helpers

declareGlobal :: a -> IORef a
{-# NOINLINE declareGlobal #-}
declareGlobal x = unsafePerformIO $ newIORef x

getRuntimeSettings :: IO AppSettings
getRuntimeSettings = catchAny readEnvironment onException
  where 
    readEnvironment = do
      mainDir <- getCurrentDirectory
      env <- B.readFile $ mainDir </> env_path
      case eitherDecode env of 
        Right settings -> return settings
        Left err -> $error $ "couldn't decode environment file, because: " ++ err
    onException :: SomeException -> IO AppSettings
    onException e = $error $ "decoding Environment failed with error: " ++ show e 

getWhitelistedUsers :: IO [ASUserId]
getWhitelistedUsers = catchAny getWhitelistedUsers' onException
  where 
    getWhitelistedUsers' = do
      mainDir <- getCurrentDirectory
      whitelist <- T.readFile $ mainDir </> whitelist_path
      return $ T.lines whitelist
    onException :: SomeException -> IO [ASUserId]
    onException e = $error $ "opening whitelist file failed with error: " ++ show e 