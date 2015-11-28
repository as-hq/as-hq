module AS.Logging 
  ( printWithTime
  , printWithTimeT
  , printObj
  , printObjT
  , printDebug
  , printDebugT
  , writeToLog
  , writeErrToLog
  , logBugReport
  ) where

import AS.Types.Eval
import AS.Types.Commits
import AS.Config.Paths

import qualified Data.Text as T
import Control.Monad.Trans.Class (lift)
import Data.Time.Clock (getCurrentTime)
import Control.Exception (catch, SomeException)

truncated :: String -> String
truncated str
  | length str < 500 = str 
  | otherwise = (take 500 str) ++ ("... [Truncated]")

getTime :: IO String
getTime = do 
  t <- getCurrentTime
  return $ take 23 $ show t

appendFile' :: String -> String -> IO ()
appendFile' fname msg = catch (appendFile fname msg) (\e -> putStrLn $ ("Error writing to log: " ++ show (e :: SomeException)))

printWithTime :: String -> IO ()
printWithTime str = do
  time <- getTime
  let disp = "[" ++ time ++ "] " ++ str
  putStrLn ((truncated disp) ++ "\n")
  logDir <- getServerLogDir
  appendFile' (logDir ++ "console_log") ('\n':disp)

printWithTimeT :: String -> EitherTExec ()
printWithTimeT = lift . printWithTime

writeToLog :: String -> CommitSource -> IO ()
writeToLog str (sid, uid) = do 
  -- first, write to master to log
  logDir <- getServerLogDir
  time <- getTime
  let sid' = T.unpack sid
      uid' = T.unpack uid
      loggedStr = '\n':str ++ "\n" ++ sid' ++ "\n" ++ uid' ++ "\n#" ++ time
      logPath = logDir ++ "server_log"
  appendFile' logPath loggedStr
  -- then write to individual log for the sheet
  let logPath' = logPath ++ sid'
  appendFile' logPath' loggedStr

-- can probably refactor with writeToLog to reduce code duplication
logBugReport :: String -> CommitSource -> IO ()
logBugReport str (sid, uid) = do 
  logDir <- getServerLogDir
  time <- getTime
  let sid' = T.unpack sid
      uid' = T.unpack uid
      loggedStr = '\n':str ++ "\n# " ++ sid' ++ "\n# " ++ uid' ++ "\n#" ++ time
      bugLogPath = logDir ++ "bug_reports"
  appendFile' bugLogPath loggedStr

writeErrToLog :: String -> CommitSource -> IO ()
writeErrToLog str (sid, uid) = do 
  logDir <- getServerLogDir
  time <- getTime
  let sid' = T.unpack sid
      uid' = T.unpack uid
      loggedStr = "\n#ERROR: "++ str ++ "\n# " ++ sid' ++ "\n# " ++ uid' ++ "\n#" ++ time
      logPath = logDir ++ "bug_reports"
  appendFile' logPath loggedStr
  -- then write to individual log for the sheet
  let logPath' = logPath ++ sid'
  appendFile' logPath' loggedStr

printObj :: (Show a) => String -> a -> IO ()
printObj disp obj = printWithTime (disp ++ ": " ++ (show $ seq () obj))
-- the seq is necessary so that the object gets evaluated before the time does in printWithTime. 

printObjT :: (Show a) => String -> a -> EitherTExec () 
printObjT disp obj = lift (printObj disp obj)

-- | For debugging purposes
printDebug :: (Show a) => String -> a -> IO ()
printDebug name obj = putStrLn ("\n\nDEBUG\n============================================================\n" ++ name ++ ": " ++ (show $ seq () obj) ++ "\n============================================================\n\n")

printDebugT :: (Show a) => String -> a -> EitherTExec ()
printDebugT name obj = lift (printDebug name obj)
