module AS.Logging 
  ( printWithTime
  , printWithTimeForced
  , printWithTimeT
  , printList2
  , printListT2
  , printObj
  , printObjForced
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
import AS.Config.Settings as S

import qualified Data.Text as T
import Data.Aeson
import Control.Monad.Trans.Class (lift)
import Data.Time.Clock (getCurrentTime)
import Control.Exception (catch, SomeException)
import Control.Monad (when, void)

import qualified Network.Wreq as Wreq

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
printWithTime = when shouldWritetoConsole . printWithTimeForced

printWithTimeForced :: String -> IO ()
printWithTimeForced str = do
  time <- getTime
  date <- getDate
  let disp = "[" ++ time ++ "] " ++ str
  putStrLn ((truncated disp) ++ "\n")

printWithTimeT :: String -> EitherTExec ()
printWithTimeT = lift . printWithTime

writeToASLog :: String -> String -> IO ()
writeToASLog logRootName msg = do
  date <- getDate
  let logPath = S.log_dir ++ logRootName ++ date
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
  logSlack logMsg
  printWithTime logMsg

logSlack :: String -> IO ()
logSlack msg = void $ Wreq.post webhookUrl payload
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

printObj :: (Show a) => String -> a -> IO ()
printObj = (when shouldWritetoConsole .) . printObjForced

printObjForced :: (Show a) => String -> a -> IO ()
printObjForced disp obj = printWithTimeForced (disp ++ ": " ++ (show $ seq () obj))
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
