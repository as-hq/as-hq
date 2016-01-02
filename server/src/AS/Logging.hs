module AS.Logging 
  ( printWithTime
  , printWithTimeT
  , printList2
  , printListT2
  , printObj
  , printObjT
  , printDebug
  , printDebugT
  , logServerMessage
  , logError
  , logBugReport
  ) where

import AS.Types.Eval
import AS.Types.Commits
import AS.Types.DB
import AS.Config.Paths
import AS.Config.Settings 

import qualified Data.Text as T
import Control.Monad.Trans.Class (lift)
import Data.Time.Clock (getCurrentTime)
import Control.Exception (catch, SomeException)

truncateLength :: Int
truncateLength = 2000

truncated :: String -> String
truncated str
  | length str < truncateLength = str 
  | otherwise = (take truncateLength str) ++ ("... [Truncated]")

-- Gets date in format 2015-12-02 20:44:24.515
getTime :: IO String
getTime = getCurrentTime >>= (return . (take 23) . show)

-- | Gets date in format 2015-12-02
getDate :: IO String
getDate = getCurrentTime >>= (return . (take 10) . show)

appendFile' :: String -> String -> IO ()
appendFile' fname msg = catch (appendFile fname msg) (\e -> putStrLn $ ("Error writing to log: " ++ show (e :: SomeException)))

printWithTime :: String -> IO ()
printWithTime str = if isBenchmark 
  then return ()
  else do
    time <- getTime
    date <- getDate
    let disp = "[" ++ time ++ "] " ++ str
    putStrLn ((truncated disp) ++ "\n")
    logDir <- getServerLogDir
    appendFile' (logDir ++ "console_log" ++ date) ('\n':'\n':disp)

printWithTimeT :: String -> EitherTExec ()
printWithTimeT = lift . printWithTime

writeToASLog :: String -> String -> IO ()
writeToASLog logRootName msg = do
  logDir <- getServerLogDir
  date <- getDate
  let logPath = logDir ++ logRootName ++ date
  appendFile' logPath ('\n':msg)

writeToASLogWithMetadata :: String -> String -> CommitSource -> IO ()
writeToASLogWithMetadata logRootName msg (CommitSource sid uid) = do
  time <- getTime
  let sid' = T.unpack sid
      uid' = T.unpack uid
      logMsg = msg ++ '\n':sid' ++ '\n':uid' ++ '\n':'#':time
  writeToASLog logRootName logMsg

serverLogsRoot :: String
serverLogsRoot = "server_log_"

logServerMessage :: String -> CommitSource -> IO ()
logServerMessage msg src = do 
  -- master log
  writeToASLogWithMetadata serverLogsRoot msg src
  -- log for just the sheet
  writeToASLogWithMetadata (serverLogsRoot ++ T.unpack (srcSheetId src)) msg src

logBugReport :: String -> CommitSource -> IO ()
logBugReport bugReport src = writeToASLogWithMetadata "bug_reports" ('#':bugReport) src

logError :: String -> CommitSource -> IO ()
logError err (CommitSource sid uid) = do 
  time <- getTime
  let sid' = T.unpack sid
      uid' = T.unpack uid
      logMsg = "#ERROR: " ++ err ++ '\n':'#':sid' ++ ',':uid' ++ "\n#" ++ time
  writeToASLog serverLogsRoot logMsg
  writeToASLog (serverLogsRoot ++ sid') logMsg
  printWithTime logMsg

printObj :: (Show a) => String -> a -> IO ()
printObj disp obj = printWithTime (disp ++ ": " ++ (show $ seq () obj))
-- the seq is necessary so that the object gets evaluated before the time does in printWithTime. 

printList2 :: (Show2 a) => String -> [a] -> IO ()
printList2 disp l = printWithTime (disp ++ ": " ++ (show $ map show2 $ seq () l))

printObjT :: (Show a) => String -> a -> EitherTExec () 
printObjT disp obj = lift (printObj disp obj)

printListT2 :: (Show2 a) => String -> [a] -> EitherTExec ()
printListT2 disp l = lift $ printList2 disp l

-- | For debugging purposes
printDebug :: (Show a) => String -> a -> IO ()
printDebug name obj = putStrLn ("\n\nDEBUG\n============================================================\n" ++ name ++ ": " ++ (show $ seq () obj) ++ "\n============================================================\n\n")

printDebugT :: (Show a) => String -> a -> EitherTExec ()
printDebugT name obj = lift (printDebug name obj)
