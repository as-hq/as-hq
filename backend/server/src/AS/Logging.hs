module AS.Logging where

import System.Directory
import Data.Aeson
import Control.Monad.Trans.Class (lift)
import Data.Time.Clock (getCurrentTime)
import Control.Exception (catch, SomeException)
import Control.Monad.State hiding (when)
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.Wreq as Wreq

import AS.Prelude
import AS.Types.Eval
import AS.Types.Commits
import AS.Types.Graph
import AS.Types.Logging
import AS.Config.Settings as S

-----------------------------------------------------------------------------------------------------------------------------
-- Exposed API
-- | All logging functions respect the `backendLogsOn` setting 
-- | unless otherwise specified.

-- | Writes to the console and main logfile with a timestamp. 
puts :: (MonadIO m) => String -> m ()
puts = liftIO . whenLogging . sendToLogger . Log MainLog

-- | Writes to the console only. 
putsConsole :: (MonadIO m) => String -> m ()
putsConsole = liftIO . sendToLogger . Log MainLog

-- | Use this function only when absolutely necessary; `show` is computed on the main thread
putsObj :: (Show a, MonadIO m) => String -> a -> m ()
putsObj msg obj = puts $ "{" ++ msg ++ "}\n" ++ show obj

-- | Writes to the console and error logfile with a timestamp. 
-- | Does not respect the `backendLogsOn` setting.
putsError :: (MonadIO m) => CommitSource -> String -> m ()
putsError src x = liftIO $ do
  let x' = "[ERROR]: " ++ x
  puts x'
  putsSheet (srcSheetId src) x'
  sendToLogger $ Log (ErrorLog src) x

-- | Writes to the console and bugreport logfile with a timestamp. 
-- | Does not respect the `backendLogsOn` setting.
putsBugReport :: (MonadIO m) => CommitSource -> String -> m ()
putsBugReport src x = liftIO $ whenLogging $ do
  let x' = "[BugReport]: " ++ x
  puts x'
  putsSheet (srcSheetId src) x'
  sendToLogger $ Log (BugLog src) x

-- | Writes to the console and sheet logfile with a timestamp. 
putsSheet :: (MonadIO m) => SheetID -> String -> m ()
putsSheet sid x = liftIO $ whenLogging $ 
  sendToLogger $ Log (SheetLog sid) x

-- | Logger loop
runLogger :: IO ()
runLogger = do
  t <- getTime
  let rootDir = server_logs_dir ++ t ++ "/"
  createDirectoryIfMissing True rootDir
  log <- getLogger
  putStrLn "Started logger."
  evalStateT (go rootDir log) newLoggerState
  where 
    go :: FilePath -> Logger -> StateT LoggerState IO ()
    go rootDir log = do
      msg <- liftIO $ readChan log
      st <- get
      case msg of 
        Log ConsoleLog txt -> do
          liftIO . putStrLn $ truncated txt
          go rootDir log 
        Log src txt -> do case src `M.lookup` (st^.handles) of 
                            Nothing -> initLog rootDir src >>= 
                                          \h -> liftIO (writeLog h src txt)
                            Just h  -> liftIO (writeLog h src txt)
                          go rootDir log
        CloseLog -> liftIO $ mapM_ hClose $ 
                      M.elems (st^.handles)


closeLog :: (MonadIO m) => m ()
closeLog = liftIO $ sendToLogger CloseLog

-----------------------------------------------------------------------------------------------------------------------------
-- Logger

whenLogging :: IO () -> IO ()
whenLogging f = do
  doIt <- getSetting backendLogsOn
  when doIt f

sendToLogger :: LogMessage -> IO ()
sendToLogger msg = do
  log <- getLogger
  writeChan log msg

initLog :: FilePath -> LogSource -> StateT LoggerState IO Handle
initLog rootDir src = do
  handle <- liftIO $ openFile (getLogPath rootDir src) AppendMode
  modify (& handles %~ M.insert src handle)
  return handle

getLogPath :: FilePath -> LogSource -> FilePath
getLogPath rootDir src = rootDir ++ case src of 
  MainLog            -> "[main]"
  SheetLog sid       -> "[sheet]" ++ T.unpack sid
  ErrorLog commitSrc -> "[errors]" ++ show commitSrc
  BugLog commitSrc   -> "[bugreports]" ++ show commitSrc
  ConsoleLog         -> error "invariant violation: no log file for ConsoleLog"

writeLog :: Handle -> LogSource -> String -> IO ()
writeLog h src x = do
  t <- getTime
  let ts = "[" ++ t ++ "]"
  putStrLn ts
  putStrLn $ truncated x
  hPutStrLn h ts
  hPutStrLn h x 
  hFlush h 
  when (slackable x) $ 
    case src of 
      ErrorLog _ -> logSlack x
      BugLog _   -> logSlack x
      _ -> return ()

newLoggerState :: LoggerState
newLoggerState = LoggerState { _handles = M.empty }

-----------------------------------------------------------------------------------------------------------------------------
-- Utils

truncated :: String -> String
truncated = take log_truncate_length

-- Gets date in format 2015-12-02 20:44:24.515
getTime :: IO String
getTime = getCurrentTime >>= (return . (take 23) . show)

slackable :: String -> Bool
slackable x = not . any same $ map (zip x) unslackable
  where 
    same = all (\(x,y) -> x == y)
    unslackable :: [String]
    unslackable =  
      [ "SERVER ERROR: unable to decode message"
      ]

logSlack :: String -> IO ()
logSlack msg = forkIO_ $ do
  appShouldLog <- getSetting slackLogsOn
  let shouldIgnore = msg `elem` S.ignoredErrorMessages
  when (appShouldLog && not shouldIgnore) $ 
    Wreq.post webhookUrl payload
  where
    webhookUrl = "https://hooks.slack.com/services/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"
    payload = object [
        "channel" .= ("#bugreports" :: String)
      , "username" .= ("ErrorBot" :: String)
      , "icon_emoji" .= ("ghost" :: String)
      , "attachments" .= [object [
          "color" .= ("danger" :: String)
        , "text" .= msg
      ]]]