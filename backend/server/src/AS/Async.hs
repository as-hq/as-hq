module AS.Async where

import AS.Types.Network

import qualified Network.WebSockets as WS

import qualified Data.Text as T
import qualified Data.Map as M

import Control.Concurrent
import Control.Exception 
import Control.Monad (when, void)

--------------------------------------------------------------------------------------------------------------
-- Timeout 

type TimeoutCallback = IO ()
type SuccessCallback = IO ()

-- | This function will not kill the action passed in; its only purpose is to 
-- execute onTimeout if an action takes longer than the timeout period. 
-- | For example, a user decides to evaluate A1. It runs for more than n seconds, so the
-- user receives a notification asking if he wants to kill it. If the user doesn't 
-- press 'kill' in time, the action will run to completion and the message will be dismissed. 
-- Otherwise, the 'Timeout' message handler will throw an exception to this thread. 
-- GHC #7719 explains why we need uninterruptibleMask_.
timeout :: Seconds -> TimeoutCallback -> SuccessCallback -> IO () -> IO ()
timeout n onTimeout onSuccess f = 
  bracket (forkIOWithUnmask $ \unmask ->
               unmask $ threadDelay (1000 * 1000 * n) >> onTimeout)
          (uninterruptibleMask_ . killThread) 
          (\_ -> f) 
  >> onSuccess

--------------------------------------------------------------------------------------------------------------
-- Heartbeat

forkHeartbeat :: WS.Connection -> Milliseconds -> IO ()
forkHeartbeat conn interval = void $ forkIO (go 1 `catch` dieSilently)
  where
    go i = do
      threadDelay (interval * 1000)
      WS.sendTextData conn ("PING" :: T.Text)
      go (i+1)
    dieSilently e = case fromException e of 
      Just asyncErr -> do
        putStrLn ("Heartbeat error: " ++ show asyncErr)
        throwIO (asyncErr :: AsyncException)
        return ()
      Nothing       -> return ()