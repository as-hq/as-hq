{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module AS.Kernels.Python where

import GHC.Generics
import Data.Maybe (fromJust)
import Data.Aeson 
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Control.Exception (catch, SomeException)
import Control.Lens hiding ((.=))
import System.ZMQ4.Monadic
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Database.Redis (Connection)

import AS.Types.Cell hiding (Cell)
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.Errors
import AS.Types.Sheets
import AS.Types.CondFormat
import AS.Types.Network
import AS.Types.Messages (MessageId)

import AS.Kernels.Internal
import AS.Logging
import AS.Config.Settings
import AS.Config.Constants
import qualified AS.Parsing.Read as R
import AS.Parsing.Show
import AS.Parsing.Common
import AS.DB.API (getEvalHeader, getAllSheets)


-----------------------------------------------------------------------------------------------------------------------------
-- Exposed functions

initialize :: Connection -> IO ()
initialize conn = do
  -- run all the headers in db to initialize the sheet namespaces
  sids <- map sheetId <$> getAllSheets conn
  headers <- mapM (\sid -> getEvalHeader conn sid Python) sids
  mapM_ (\h -> runEitherT $ evaluateHeader initialize_message_id (h^.evalHeaderSheetId) (h^.evalHeaderExpr)) headers

evaluate :: MessageId -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluate = evaluateWithScope Cell

evaluateHeader :: MessageId -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluateHeader = evaluateWithScope Header

-- #needsrefactor Should not hard core errors
evaluateLambdaFormat :: ASSheetId -> LambdaConditionExpr -> ASValue -> EitherTExec FormatResult
evaluateLambdaFormat sid lambdaExpr val = do 
  (mFormatStr, mErr) <- case val of 
    ValueError _ _ -> return (Nothing, Just "can't format cell with error")
    v              -> do 
      let evalExpr = "(" ++ lambdaExpr ++ ")(" ++ showValue Python (CellValue v) ++ ")"
      EvaluateFormatReply f e <- sendMessage $ EvaluateFormatRequest sid evalExpr
      return (f,e)
  return $ case mFormatStr of 
    Just formatStr -> case R.parseFormatValue formatStr of 
      Nothing -> FormatError "Failed to interpret Python result as string." 
      Just format -> FormatSuccess format
    Nothing  -> case mErr of 
      Just err -> FormatError err
      Nothing  -> FormatError "Formatting returned neither value nor error."

haltMessage :: MessageId -> IO ()
haltMessage = sendMessage_ . HaltMessageRequest

clear :: ASSheetId -> IO ()
clear = sendMessage_ . ClearRequest

evaluateSql :: MessageId -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluateSql mid sid code = evaluateWithScope Cell mid sid =<< (liftIO $ formatSqlCode code)

testCell :: ASSheetId -> EvalCode -> IO ()
testCell sid code = printObj "Test evaluate python cell: " =<< (runEitherT $ evaluate sid test_message_id code)

testHeader :: ASSheetId -> EvalCode -> IO ()
testHeader sid code = printObj "Test evaluate python header: " =<< (runEitherT $ evaluateHeader sid test_message_id code)

formatSqlCode :: EvalCode -> IO EvalCode
formatSqlCode code = do
  template <- readFile $ eval_dir ++ "sql/template.py"
  return $ replaceSubstrings template [("#CODE#", code)]

-----------------------------------------------------------------------------------------------------------------------------
-- Helpers

data EvalScope = Header | Cell deriving (Generic)
data KernelMessage = 
    EvaluateRequest { scope :: EvalScope, evalMessageId :: MessageId, envSheetId :: ASSheetId, code :: String } 
  | EvaluateFormatRequest { envSheetId :: ASSheetId, code :: String }
  | GetStatusRequest ASSheetId
  | AutocompleteRequest { envSheetId' :: ASSheetId, completeString :: String }
  | ClearRequest ASSheetId
  | HaltMessageRequest MessageId
  deriving (Generic)

data KernelResponse = 
    EvaluateReply { value :: Maybe String, evalError :: Maybe String, display :: Maybe String } 
  | EvaluateFormatReply { formatValue :: Maybe String, formatError :: Maybe String }
  | GetStatusReply -- TODO
  | AutocompleteReply -- TODO
  | GenericSuccessReply
  | GenericErrorReply String
  deriving (Generic)

instance ToJSON EvalScope

instance ToJSON KernelMessage where
  toJSON msg = case msg of 
    EvaluateRequest scope mid sid code -> object  [ "type" .= ("evaluate" :: String)
                                                  , "scope" .= scope
                                                  , "message_id" .= mid
                                                  , "sheet_id" .= sid
                                                  , "code" .= code]

    EvaluateFormatRequest sid code -> object  [ "type" .= ("evaluate_format" :: String)
                                              , "sheet_id" .= sid
                                              , "code" .= code]

    GetStatusRequest sid -> object  [ "type" .= ("get_status" :: String)
                                    , "sheet_id" .= sid]

    AutocompleteRequest sid str -> object [ "type" .= ("autocomplete" :: String)
                                          , "sheet_id" .= sid
                                          , "complete_str" .= str]

    ClearRequest sid -> object [ "type" .= ("clear" :: String)
                               , "sheet_id" .= sid]

    HaltMessageRequest mid -> object [ "type" .= ("halt_message" :: String)
                                     , "message_id" .= mid]

instance FromJSON KernelResponse where
  parseJSON (Object v) = do
    val <- v .: "type" :: (Parser String)
    case val of 
      "evaluate" -> EvaluateReply <$> v .:? "value" <*> v .:? "error" <*> v .:? "display"
      "evaluate_format" -> EvaluateFormatReply <$> v .:? "value" <*> v .:? "error"
      "get_status" -> return GetStatusReply -- TODO
      "autocomplete" -> return AutocompleteReply -- TODO
      "generic_success" -> return GenericSuccessReply
      "generic_error" -> GenericErrorReply <$> v .: "error"

evaluateWithScope :: EvalScope -> MessageId -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluateWithScope _ _ _ "" = return emptyResult
evaluateWithScope scope mid sid code = do
  (EvaluateReply v err disp) <- sendMessage $ EvaluateRequest scope mid sid code
  case v of 
    Nothing -> case err of 
      Just e -> return $ EvalResult (CellValue $ ValueError e "") disp
      Nothing -> return $ EvalResult (CellValue NoValue) disp
    Just v -> do
      cval <- hoistEither $ R.parseValue Python v
      return $ EvalResult cval disp

sendMessage :: KernelMessage -> EitherTExec KernelResponse
sendMessage msg = do
  resp <- liftIO $ runZMQ $ do
    reqSocket <- connectToKernel
    send' reqSocket [] $ encode msg
    eitherDecodeStrict <$> receive reqSocket
  case resp of 
    Left e -> left $ KernelError e
    -- this is a top-level kernel error that should throw a "left"
    -- i.e. an API error, network error, or other non-evaluation-related error
    Right (GenericErrorReply e) -> left $ KernelError e 
    Right r -> return r

sendMessage_ :: KernelMessage -> IO ()
sendMessage_ msg = runZMQ $ do
  reqSocket <- connectToKernel  
  send' reqSocket [] $ encode msg
  return ()

connectToKernel = do
  addr <- liftIO $ getSetting pykernelAddress
  reqSocket <- socket Req 
  connect reqSocket addr
  return reqSocket
