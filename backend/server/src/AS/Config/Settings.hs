module AS.Config.Settings where

import AS.Prelude
import AS.Types.Network
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

import GHC.Conc (getNumProcessors, setNumCapabilities)

--------------------------------------------------------------------------------------
-- Types

data AppSettings = AppSettings  { _backendWsAddress :: String
                                , _backendWsPort :: Int
                                , _graphDbAddress :: String
                                , _pyKernelAddress :: String
                                , _rKernelAddress_server :: String
                                , _rKernelAddress_client :: String
                                , _redisPort :: Int
                                , _redisHost :: String
                                , _redisPassword :: Maybe BC.ByteString
                                , _shouldWriteToConsole :: Bool
                                , _shouldWriteToSlack :: Bool}
                                deriving (Show)

-- default values represent what should happen on localhost; the Environment.json values *should* 
-- all be set remotely. 
instance FromJSON AppSettings where
  parseJSON (Object v) = do
    wsAddr <- v .: "backendWsAddress"
    wsPort <- v .: "backendWsPort"
    graphAddr <- v .: "graphDbAddress_haskell"
    pyAddr <- v .: "pyKernelAddress_haskell"
    rAddr_server <- v .: "rKernelAddress_server"
    rAddr_client <- v .: "rKernelAddress_client"
    redisPort <- v .: "redisPort"
    redisHost <- v .: "redisHost"
    redisPassword <- v .:? "redisPassword"
    shouldWriteToConsole <- v .: "shouldWriteToConsole"
    shouldWriteToSlack <- v .: "shouldWriteToSlack"
    return $ AppSettings 
              wsAddr 
              wsPort 
              graphAddr 
              pyAddr 
              rAddr_server
              rAddr_client
              redisPort 
              redisHost 
              (BC.pack <$> redisPassword) 
              shouldWriteToConsole 
              shouldWriteToSlack
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
                        "send: resource vanished (Broken pipe)",
                        "CloseRequest 1001 \"\""]

headerLangs :: [ASLanguage]
headerLangs = [Python, R] 

heartbeat_interval :: Milliseconds
heartbeat_interval = 1000

process_message_timeout :: Int
process_message_timeout = 3 -- seconds

google_token_verify_url :: String
google_token_verify_url = "https://www.googleapis.com/oauth2/v3/tokeninfo?id_token="

google_client_id :: String
google_client_id = "347875438909-e81ep6ofitkq4deio3kagakpr5ujeh20.apps.googleusercontent.com"

images_dir = "static/images/"
eval_dir   = "eval_files/"
env_path   = ".." </> "Environment.json"
log_dir    = "logs/"
static_dir = "static/"
whitelist_path = ".." </> "email_whitelist.txt"


--------------------------------------------------------------------------------------
-- configuration-dependent parameters

alphaMain a = initializeSettings >> a

getSetting :: IORef a -> IO a
getSetting = readIORef

-- !!!!!!MUST BE CALLED UPON APP START!!!!!!
initializeSettings :: IO ()
initializeSettings = do
  setNumCapabilities =<< getNumProcessors

  appSettings <- getRuntimeSettings 
  appDir <- getCurrentDirectory

  writeIORef appDirectory appDir
  writeIORef shouldLogSlack (appSettings^.shouldWriteToSlack)
  writeIORef shouldLogConsole (appSettings^.shouldWriteToConsole)
  writeIORef graphAddress (appSettings^.graphDbAddress)
  writeIORef pykernelAddress (appSettings^.pyKernelAddress)
  writeIORef rkernelAddress_server (appSettings^.rKernelAddress_server)
  writeIORef rkernelAddress_client (appSettings^.rKernelAddress_client)
  writeIORef serverHost (appSettings^.backendWsAddress)
  writeIORef serverPort (appSettings^.backendWsPort)
  writeIORef dbHost (appSettings^.redisHost)
  writeIORef dbPort (appSettings^.redisPort)
  writeIORef dbPassword (appSettings^.redisPassword)

  putStrLn . ("[CONFIG] appDirectory : " ++) . show =<< getSetting appDirectory
  putStrLn . ("[CONFIG] shouldLogSlack : " ++) . show =<< getSetting shouldLogSlack
  putStrLn . ("[CONFIG] shouldLogConsole : " ++) . show =<< getSetting shouldLogConsole
  putStrLn . ("[CONFIG] graphAddress : " ++) . show =<< getSetting graphAddress
  putStrLn . ("[CONFIG] pykernelAddress : " ++) . show=<< getSetting pykernelAddress
  putStrLn . ("[CONFIG] rkernelAddress_server : " ++) . show =<< getSetting rkernelAddress_server
  putStrLn . ("[CONFIG] rkernelAddress_client : " ++) . show =<< getSetting rkernelAddress_client
  putStrLn . ("[CONFIG] serverHost : " ++) . show =<< getSetting serverHost
  putStrLn . ("[CONFIG] serverPort : " ++) . show =<< getSetting serverPort
  putStrLn . ("[CONFIG] dbHost : " ++) . show =<< getSetting dbHost
  putStrLn . ("[CONFIG] dbPort : " ++) . show =<< getSetting dbPort
  putStrLn . ("[CONFIG] dbPassword : " ++) . show =<< getSetting dbPassword 
  return ()

appDirectory :: IORef String
appDirectory = declareGlobal "string"

shouldLogSlack :: IORef Bool
shouldLogSlack = declareGlobal True

shouldLogConsole :: IORef Bool
shouldLogConsole = declareGlobal True

graphAddress :: IORef String
graphAddress = declareGlobal "string"

pykernelAddress :: IORef String
pykernelAddress = declareGlobal "string"

rkernelAddress_server :: IORef String
rkernelAddress_server  = declareGlobal "string"

rkernelAddress_client :: IORef String
rkernelAddress_client  = declareGlobal "string"

serverHost :: IORef String
serverHost = declareGlobal "string"

serverPort :: IORef Int
serverPort = declareGlobal 666

dbHost :: IORef String
dbHost = declareGlobal "string"

dbPort :: IORef Int
dbPort = declareGlobal 666

dbPassword :: IORef (Maybe BC.ByteString)
dbPassword = declareGlobal Nothing

--------------------------------------------------------------------------------------
-- private helpers

{-# NOINLINE declareGlobal #-}
declareGlobal :: a -> IORef a
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