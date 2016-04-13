{-# LANGUAGE ScopedTypeVariables #-}

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
  (EvaluateReply r) <- handleZMQ $ EvaluateRequest scope mid sid code
  return r

evaluate :: MessageId -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluate = evaluateWithScope Cell

evaluateHeader :: MessageId -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluateHeader = evaluateWithScope Header

haltMessage :: MessageId -> IO ()
haltMessage = handleZMQ_ . HaltMessageRequest

clear :: ASSheetId -> IO ()
clear = handleZMQ_ . ClearRequest

--------------------------------------------------------------------------------
-- ZMQ message receiving

poke_max :: Int
poke_max = 5

resend_max :: Int
resend_max = 5

-- | Keep polling for a response from the kernel. If it's still processing a 
-- reply, then keep polling. If we're not getting responses, then give up after
-- a few poll cycles (given by poke_threshold) and resend the message. Finally
-- give up if a few resends still yields nothing. Returns the decoded response.
getResponse :: MessageId -> -- id of message
               ZMQ z () -> -- ZMQ action to do if poke_max is reached
               Socket z Dealer -> -- client
               ZMQ z (Either String KernelReply)
getResponse mid f = getResponse' f 0 0 True
  where
    getResponse' :: ZMQ z () -> -- resend action
                    Int -> -- number of resends so far
                    Int -> -- number of poll cycles so far
                    Bool -> -- be lenient; still processing reply
                    Socket z Dealer -> -- client
                    ZMQ z (Either String KernelReply)
    getResponse' action resends cycles lenient client = do
      -- The poll timeout (ms) should have the property that a poke can get back
      -- within the timeout.
      [evts] <- poll 2000 [Sock client [In] Nothing]
      if (In `elem` evts) 
        then do 
          reply <- Serial.decode <$> receive client
          liftIO $ putStrLn $ "REPLY " ++ show reply
          case reply of
            (Right StillProcessingReply) -> 
              getResponse' action resends (cycles + 1) True client
            otherMessage -> return otherMessage
        else do 
          if resends > resend_max
            then return $ Right $ GenericErrorReply "R Kernel down!"
            else do
              if cycles > poke_max && not lenient 
                then do 
                  action >> getResponse' action (resends + 1) 0 True client 
                else do
                  send' client [] $ Serial.encodeLazy (PokeRequest mid)
                  getResponse' action resends (cycles + 1) False client

--------------------------------------------------------------------------------
-- ZMQ message connecting and processing

-- | Connect as a client, send the KernelRequest and get a KernelReply using ZMQ
-- protocols.
handleZMQ :: KernelRequest -> EitherTExec KernelReply
handleZMQ msg@(EvaluateRequest _ mid _ _) = do
  resp <- liftIO $ runZMQ $ do
    dealer <- connectToKernel
    send' dealer [] $ Serial.encodeLazy msg
    getResponse mid (send' dealer [] $ Serial.encodeLazy msg) dealer
  case resp of 
    Left e -> left $ KernelError e
    -- this is a top-level kernel error that should throw a "left"
    -- i.e. an API error, network error, or other non-evaluation-related error
    Right (GenericErrorReply e) -> left $ KernelError e 
    Right r -> return r

-- | Connect to the kernel as a client and send the kernel a message as a 
-- new client, without expecting a response.
handleZMQ_ :: KernelRequest -> IO ()
handleZMQ_ msg = runZMQ $ do
  client <- connectToKernel  
  send' client [] $ Serial.encodeLazy msg
  return ()

-- | Makes and returns a dealer socket connected to rkernelAddress_client.
connectToKernel :: ZMQ z (Socket z Dealer)
connectToKernel = do
  addr <- liftIO $ getSetting rkernelAddress_client
  client <- socket Dealer 
  connect client addr
  return client

--------------------------------------------------------------------------------