module AS.Async where

import AS.Prelude
import AS.Types.Network

import qualified Network.WebSockets as WS

import qualified Data.Text as T
import qualified Data.Map as M

import Control.Concurrent
import Control.Exception 
import Control.Monad (when, void)

--------------------------------------------------------------------------------------------------------------
-- Timeout 

-- | This function will not kill the action passed in; its only purpose is to 
-- execute onTimeout if an action takes longer than the timeout period. 
-- For example, a user decides to evaluate A1. A timer thread is spawned, delays for
--  n seconds, then dies after sending the user a notification asking if he wants to 
-- kill it. If the user doesn't press 'kill' in time, the action will run to 
-- completion and the message will be dismissed. Otherwise, the 'Timeout' message
--  handler will throw an exception to this thread, to halt the action.
-- GHC #7719 explains why we need uninterruptibleMask_.
timeout :: Int -> IO () -> IO () -> IO () -> IO ()
timeout seconds onTimeout onSuccess f = do
  bracket (forkIOWithUnmask $ \unmask ->
               unmask $ threadDelay (1000 * 1000 * seconds) >> onTimeout)
          (uninterruptibleMask_ . killThread) 
          (\_ -> f) 
  onSuccess

--------------------------------------------------------------------------------------------------------------
-- Heartbeat

heartbeat :: WS.Connection -> Milliseconds -> IO ()
heartbeat conn interval = go `catchAny` (const $ return ())
  where
    go = do
      threadDelay (interval * 1000)
      WS.sendTextData conn ("PING" :: T.Text)
      go