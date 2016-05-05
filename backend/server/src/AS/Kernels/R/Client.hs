{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module AS.Kernels.R.Client where

import qualified Database.Redis as Redis
import System.ZMQ4.Monadic
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Applicative
import Control.Lens
import Database.Redis (Connection)

import AS.Types.Cell hiding (Cell)
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.Network
import AS.Types.Messages
import AS.Kernels.R.Types

import AS.Prelude
import AS.Serialize as Serial
import AS.Config.Settings
import AS.Config.Constants
import AS.DB.API (getAllHeaders)
import AS.Kernels.Internal

--------------------------------------------------------------------------------
-- Initialization 

initialize :: Connection -> IO ()
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
-- ZMQ run requests

-- | Connect as a client, send the KernelRequest and get a KernelReply using ZMQ
-- protocols. 
runRequest :: KernelRequest -> EitherTExec KernelReply
runRequest msg = do
  resp <- liftIO $ runZMQ $ do
    addr <- liftIO $ getSetting rkernelAddress_client
    dealer <- connectToKernel addr
    let mid       =  getMessageId msg
    let send      = send' dealer [] $ Serial.encodeLazy msg
    let statusReq = produceStatusRequest msg dealer
    let defErr    = GenericErrorReply "R Kernel Down!"
    let isStill   = (==) StillProcessingReply
    getResponse mid send statusReq isStill defErr Serial.decode dealer
  case resp of 
    Left e -> left $ KernelError e
    -- this is a top-level kernel error that should throw a "left"
    -- i.e. an API error, network error, or other non-evaluation-related error
    Right (GenericErrorReply e) -> left $ KernelError e 
    Right r -> return r

-- | Connect to the kernel as a client and send the kernel a message as a 
-- new client, without returning a response. Still attempt persistence.
runRequest_ :: KernelRequest -> IO ()
runRequest_ msg = void $ do
  resp <- runEitherT $ runRequest msg
  case resp of
    Right (GenericErrorReply e) -> $error e
    otherwise -> return ()

-- | Given the request and the client, return the statusReq action to test if
--  the message sent to the kernel is still being processed. 
produceStatusRequest ::  KernelRequest -> Socket z Dealer -> ZMQ z ()
produceStatusRequest req dealer = case getMessageId req of
  Just mid -> send' dealer [] $ Serial.encodeLazy (GetStatusRequest mid )
  _ -> return ()

-- | Helper to get message ids
getMessageId :: KernelRequest -> Maybe MessageId
getMessageId (EvaluateRequest _ mid _ _) = Just mid
getMessageId (ClearRequest _) = Nothing
getMessageId (HaltMessageRequest mid) = Just mid
getMessageId (GetStatusRequest mid) = Just mid

--------------------------------------------------------------------------------

