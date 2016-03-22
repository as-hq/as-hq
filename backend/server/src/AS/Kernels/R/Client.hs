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

-- #some code duplication with the python kernel here.
-- on second thought, this may not be avoidable right now (anand 3/21)

initialize :: Redis.Connection -> IO ()
initialize conn = do
  -- run all the headers in db to initialize the sheet namespaces
  headers <- getAllHeaders conn R
  mapM_ 
    (\h -> runEitherT $ 
      evaluateHeader initialize_message_id (h^.evalHeaderSheetId) (h^.evalHeaderExpr)) 
    headers

evaluate :: MessageId -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluate = evaluateWithScope Cell

evaluateHeader :: MessageId -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluateHeader = evaluateWithScope Header

haltMessage :: MessageId -> IO ()
haltMessage = sendMessage_ . HaltMessageRequest

clear :: ASSheetId -> IO ()
clear = sendMessage_ . ClearRequest

-----------------------------------------------------------------------------------------------------------------------------
-- Helpers

evaluateWithScope :: EvalScope -> MessageId -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluateWithScope _ _ _ "" = return emptyResult
evaluateWithScope scope mid sid code = do
  sendMessage (EvaluateRequest scope mid sid code) >>= \(EvaluateReply r) -> return r

sendMessage :: KernelRequest -> EitherTExec KernelReply
sendMessage msg = do
  resp <- liftIO $ runZMQ $ do
    reqSocket <- connectToKernel
    send' reqSocket [] $ Serial.encodeLazy msg
    Serial.decode <$> receive reqSocket
  case resp of 
    Left e -> left $ KernelError e
    -- this is a top-level kernel error that should throw a "left"
    -- i.e. an API error, network error, or other non-evaluation-related error
    Right (GenericErrorReply e) -> left $ KernelError e 
    Right r -> return r

sendMessage_ :: KernelRequest -> IO ()
sendMessage_ msg = runZMQ $ do
  reqSocket <- connectToKernel  
  send' reqSocket [] $ Serial.encodeLazy msg
  return ()

connectToKernel :: ZMQ z (Socket z Req)
connectToKernel = do
  addr <- liftIO $ getSetting rkernelAddress_client
  reqSocket <- socket Req 
  connect reqSocket addr
  return reqSocket

