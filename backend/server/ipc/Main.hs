{-# LANGUAGE OverloadedStrings #-}

module Main where

import AS.Prelude
import AS.Config.Settings
import AS.Types.Errors    (ASExecError(..))
import AS.Types.Messages  (MessageId)
import Data.Aeson         (eitherDecodeStrict, encode)

import AS.Kernels.Python.Types
import AS.Kernels.Python.Client (getMessageId)

import System.ZMQ4

import Control.Concurrent.MVar
import Control.Concurrent.Async hiding (poll)

import Data.ByteString  (ByteString)
import Data.Map         (Map)
import qualified Data.Map as Map

data ReqId = ReqId Int deriving (Show, Ord, Eq)

type Resp = (ReqId, Either ASExecError KernelReply)

data KernelComms = KernelComms 
  { client :: Socket Dealer
  , inflight :: MVar (Map ReqId (Async Resp))
  , nextReqId :: MVar ReqId
  }

instance Show KernelReply

poke_max = 5
resend_max = 5
poll_timeout = fromIntegral 2000

main :: IO ()
main = alphaMain $ do
  withContext $ \ctx -> 
    withSocket ctx Dealer $ \sock -> do
      comms <- initKernelComms sock
      let mkReq i = EvaluateRequest Cell "message_id" "sheet_id" (show i)

      -- send
      forM_ [1..10] $ \i ->
        submitReq comms (mkReq i)

      -- receive
      forever $ 
        putStrLn . show =<< receiveResp comms

initKernelComms :: Socket Dealer -> IO KernelComms
initKernelComms sock = do
  addr  <- getSetting pykernelAddress 
  connect sock addr
  ifl   <- newMVar $ Map.empty
  reqId <- newMVar $ ReqId 0
  return $ KernelComms sock ifl reqId 

submitReq :: KernelComms -> KernelRequest -> IO ReqId
submitReq comms req = do
  reqId <- modifyMVar (nextReqId comms) $ \(ReqId r) -> 
    let r' = ReqId $ r + 1
    in return (r',r')
  act <- async $ do
    resp <- catchAny (runRequest (client comms) req) $ \e -> 
      return . Left . KernelError $ show e
    return (reqId, resp)
  modifyMVar_ (inflight comms) $  
    return . Map.insert reqId act 
  return reqId

receiveResp :: KernelComms -> IO Resp
receiveResp comms = do
  ifl <- readMVar $ inflight comms
  (act, ret) <- waitAny $ Map.elems ifl
  modifyMVar_ (inflight comms) $ 
    return . Map.filter (/= act) 
  return ret

runRequest :: Socket Dealer -> KernelRequest -> IO (Either ASExecError KernelReply)
runRequest sock msg = 
  readResponse <$> getResponse mid send statusReq isStill defErr sock
  where
    mid       = getMessageId msg
    send      = send' sock [] $ encode msg
    statusReq = 
      whenJust mid $ \i -> 
        send' sock [] $ encode (GetStatusRequest i)
    defErr    = GenericErrorReply "Python Kernel Down!"
    isStill   = (==) StillProcessingReply
    readResponse resp = 
      case resp of 
        Left e -> Left $ KernelError e
        -- this is a top-level kernel error that should throw a "left"
        -- i.e. an API error, network error, or other non-evaluation-related error
        Right (GenericErrorReply e) -> Left $ KernelError e 
        Right r -> Right r

getResponse :: Maybe MessageId -> -- id of message
               IO () -> -- ZMQ send action
               IO () -> -- ZMQ poke action
               (KernelReply -> Bool) -> -- how to tell if the response still processing
               KernelReply -> -- default error reply
               Socket Dealer -> -- client
               IO (Either String KernelReply)
getResponse mMid send poke isStill defErr client =  
  send >> getResponse' send poke 0 0 True client
  where
    -- Given resend action, poke action,  number of resends, number of poke
    -- cycles, and the client, try to extract a response.
    getResponse' rsAct pkAct resends cycles lenient client = do
      -- The poll timeout (ms) should have the property that a poke can get back
      -- within the timeout.
      [evts] <- poll poll_timeout [Sock client [In] Nothing]
      if In `elem` evts
        then do 
          reply <- eitherDecodeStrict <$> receive client
          case reply of 
            Right r -> if isStill r
              then getResponse' rsAct pkAct resends (cycles + 1) True client
              else return reply
            _ -> return reply
        else do 
          if resends > resend_max
            then return $ Right defErr
            else do
              if cycles > poke_max && not lenient 
                then do 
                  rsAct
                  getResponse' rsAct pkAct (resends + 1) 0 True client 
                else do
                  pkAct
                  getResponse' rsAct pkAct resends (cycles + 1) False client
