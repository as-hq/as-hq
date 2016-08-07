{-# LANGUAGE ScopedTypeVariables #-}

module AS.Kernels.Internal where

import Database.Redis (Connection)
import System.ZMQ4.Monadic
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Applicative
import System.IO
import System.Process
import qualified Data.ByteString as B

import AS.Prelude
import AS.Serialize as Serial
import AS.Config.Settings
import AS.Config.Constants
import AS.DB.API (getAllHeaders)

import AS.Types.Cell hiding (Cell)
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.Network
import AS.Types.Messages
import AS.Kernels.R.Types

--------------------------------------------------------------------------------
-- Evaluation

evalShell :: ASLanguage -> String -> IO String
evalShell lang s = do 
  (_, stdOut, stdErr,hProcess) <- runInteractiveCommand s
  sOutput <- System.IO.hGetContents stdOut
  sErr <- System.IO.hGetContents stdErr
  foldr seq (waitForProcess hProcess) sOutput
  foldr seq (waitForProcess hProcess) sErr
  return $! readOutput lang sOutput sErr

readOutput :: ASLanguage -> String -> String -> String
readOutput lang res err = case err of 
  "" -> res
  otherwise -> case lang of 
      Python -> case res of 
          "" -> err
          otherwise -> res
      OCaml -> err
      otherwise -> err

--------------------------------------------------------------------------------
-- ZMQ message receiving

poke_max = 5
resend_max = 5
poll_timeout = fromIntegral 2000

-- | Keep polling for a response from the kernel. If it's still processing a 
-- reply, then keep polling. If we're not getting responses, then give up after
-- a few poll cycles (given by poke_threshold) and resend the message. Finally
-- give up if a few resends still yields nothing. Returns the decoded response.
--
-- There are messages with and without message id's, and those without message
-- id's have no poke action.
getResponse :: Maybe MessageId -> -- id of message
               ZMQ z () -> -- ZMQ send action
               ZMQ z () -> -- ZMQ poke action
               (rep -> Bool) -> -- how to tell if the response still processing
               rep -> -- default error reply
               (B.ByteString -> Either String rep) -> -- decode
               Socket z Dealer -> -- client
               ZMQ z (Either String rep)
getResponse mMid send poke isStill defErr decode client =  do
  send
  decode <$> receive client


  -- send >> getResponse' send poke 0 0 True client
  -- where
  --   -- Given resend action, poke action,  number of resends, number of poke
  --   -- cycles, and the client, try to extract a response.
  --   getResponse' rsAct pkAct resends cycles lenient client = do
  --     -- The poll timeout (ms) should have the property that a poke can get back
  --     -- within the timeout.
  --     [evts] <- poll poll_timeout [Sock client [In] Nothing]
  --     if In `elem` evts
  --       then do 
  --         reply <- decode <$> receive client
  --         case reply of 
  --           Right r -> if isStill r
  --             then getResponse' rsAct pkAct resends (cycles + 1) True client
  --             else return reply
  --           _ -> return reply
  --       else do 
  --         if resends > resend_max
  --           then return $ Right defErr
  --           else do
  --             if cycles > poke_max && not lenient 
  --               then do 
  --                 rsAct
  --                 getResponse' rsAct pkAct (resends + 1) 0 True client 
  --               else do
  --                 pkAct
  --                 getResponse' rsAct pkAct resends (cycles + 1) False client

--------------------------------------------------------------------------------
-- ZMQ connection

-- | Makes and returns a dealer socket connected to the address given.
connectToKernel :: String -> ZMQ z (Socket z Dealer)
connectToKernel addr = do
  client <- socket Dealer 
  connect client addr
  return client

--------------------------------------------------------------------------------
