{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module AS.Kernels.R.Client where

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

import qualified Database.Redis as Redis
import System.ZMQ4.Monadic
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Applicative
import Control.Lens

--------------------------------------------------------------------------------
-- Initialization 

initialize :: Redis.Connection -> IO ()
initialize conn = do
  -- Run all the headers in db to initialize the sheet namespaces
  headers <- getAllHeaders conn R
  forM_ headers $ \h -> do 
    let sid  = h^.evalHeaderSheetId
    let expr = h^.evalHeaderExpr
    runEitherT $ evaluateHeader initialize_message_id sid expr

--------------------------------------------------------------------------------
-- Top level evaluation functions

evaluateWithScope :: EvalScope -> MessageId -> ASSheetId -> 
                     EvalCode -> EitherTExec EvalResult
evaluateWithScope _ _ _ "" = return emptyResult
evaluateWithScope scope mid sid code = do
  (EvaluateReply r) <- runRequest $ EvaluateRequest scope mid sid code
  return r

evaluate :: MessageId -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluate = evaluateWithScope Cell

evaluateHeader :: MessageId -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluateHeader = evaluateWithScope Header

haltMessage :: MessageId -> IO ()
haltMessage = runRequest_ . HaltMessageRequest

clear :: ASSheetId -> IO ()
clear = runRequest_ . ClearRequest

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
               Socket z Dealer -> -- client
               ZMQ z (Either String KernelReply)
getResponse (Just mid) f client = do 
  let pk = send' client [] $ Serial.encodeLazy (GetStatusRequest mid)
  f >> getResponse' f pk 0 0 True client
getResponse Nothing f client = f >> getResponse' f (return ()) 0 0 True client


getResponse' :: ZMQ z () -> -- resend action
                ZMQ z () -> -- poke action
                Int -> -- number of resends so far
                Int -> -- number of poll cycles so far
                Bool -> -- be lenient; still processing reply
                Socket z Dealer -> -- client
                ZMQ z (Either String KernelReply)
getResponse' rsAct pkAct resends cycles lenient client = do
  -- The poll timeout (ms) should have the property that a poke can get back
  -- within the timeout.
  [evts] <- poll poll_timeout [Sock client [In] Nothing]
  if In `elem` evts
    then do 
      reply <- Serial.decode <$> receive client
      case reply of
        (Right StillProcessingReply) -> 
          getResponse' rsAct pkAct resends (cycles + 1) True client
        otherMessage -> return otherMessage
    else do 
      if resends > resend_max
        then return $ Right $ GenericErrorReply "R Kernel down!"
        else do
          if cycles > poke_max && not lenient 
            then do 
              rsAct
              getResponse' rsAct pkAct (resends + 1) 0 True client 
            else do
              pkAct
              getResponse' rsAct pkAct resends (cycles + 1) False client

--------------------------------------------------------------------------------
-- ZMQ message connecting and processing

-- | Connect as a client, send the KernelRequest and get a KernelReply using ZMQ
-- protocols.
runRequest :: KernelRequest -> EitherTExec KernelReply
runRequest msg@(EvaluateRequest _ mid _ _) = do
  resp <- liftIO $ runZMQ $ do
    dealer <- connectToKernel
    getResponse (Just mid) (send' dealer [] $ Serial.encodeLazy msg) dealer
  case resp of 
    Left e -> left $ KernelError e
    -- this is a top-level kernel error that should throw a "left"
    -- i.e. an API error, network error, or other non-evaluation-related error
    Right (GenericErrorReply e) -> left $ KernelError e 
    Right r -> return r

-- | Helper to get message ids
getMessageId :: KernelRequest -> Maybe MessageId
getMessageId (EvaluateRequest _ mid _ _) = Just mid
getMessageId (ClearRequest _) = Nothing
getMessageId (HaltMessageRequest mid) = Just mid
getMessageId (GetStatusRequest mid) = Just mid

-- | Connect to the kernel as a client and send the kernel a message as a 
-- new client, without returning a response. Still attempt persistence.
runRequest_ :: KernelRequest -> IO ()
runRequest_ msg = void $ do
  resp <- runZMQ $ do 
    let maybeMid = getMessageId msg
    dealer <- connectToKernel
    getResponse maybeMid (send' dealer [] $ Serial.encodeLazy msg) dealer
  case resp of
    Right (GenericErrorReply e) -> $error e
    otherwise -> return ()

-- | Makes and returns a dealer socket connected to rkernelAddress_client.
connectToKernel :: ZMQ z (Socket z Dealer)
connectToKernel = do
  addr <- liftIO $ getSetting rkernelAddress_client
  client <- socket Dealer 
  connect client addr
  return client

--------------------------------------------------------------------------------