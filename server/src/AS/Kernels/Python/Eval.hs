{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module AS.Kernels.Python.Eval where

import GHC.Generics

import AS.Kernels.Internal
import AS.Kernels.LanguageUtils
import AS.Kernels.Python.Pyfi

import AS.Logging
import AS.Config.Settings
import qualified AS.Parsing.Read as R

import AS.Types.Cell hiding (Cell)
import AS.Types.Eval
import AS.Types.EvalHeader
import AS.Types.Errors
import AS.Types.Sheets
import AS.Types.CondFormat
import AS.Types.Network

import qualified AS.DB.API as DB

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

---------------------------------------------------------------------------------
-- Exposed functions

initialize :: KernelAddress -> Connection -> IO ()
initialize addr conn = do
  -- run all the headers in db to initialize the sheet namespaces
  sids <- map sheetId <$> DB.getAllSheets conn
  headers <- mapM (\sid -> DB.getEvalHeader conn sid Python) sids
  mapM_ (\h -> runEitherT $ evaluateHeader addr (h^.evalHeaderSheetId) (h^.evalHeaderExpr)) headers

evaluate :: KernelAddress -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluate addr = evaluateWithScope addr Cell

evaluateHeader :: KernelAddress -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluateHeader addr = evaluateWithScope addr Header

evaluateFormat :: KernelAddress -> ASSheetId -> EvalCode -> EitherTExec FormatResult
evaluateFormat addr sid code = do
  (EvaluateFormatReply v e) <- sendMessage addr $ EvaluateFormatRequest sid code
  return $ case v of 
    Just val -> FormatSuccess $ R.parseFormatValue val
    Nothing  -> case e of 
      Just err -> FormatError err
      Nothing  -> FormatError "Formatting returned neither value nor error."

clear :: KernelAddress -> ASSheetId -> IO ()
clear addr = (sendMessage_ addr) . ClearRequest

-- SQL has not been updated to use the new kernel yet, because it doesn't seem super urgent rn.
-- #anand 1/1/16
evaluateSql :: String -> String -> EitherTExec EvalResult
evaluateSql _ "" = return emptyResult
evaluateSql header str = do
    validCode <- formatCode header SQL str
    if isDebug
        then lift $ writeExecFile SQL validCode
        else return ()
    v <- execWrappedCode validCode
    return $ EvalResult v Nothing

testCell :: KernelAddress -> ASSheetId -> EvalCode -> IO ()
testCell addr sid code = printObj "Test evaluate python cell: " =<< (runEitherT $ evaluate addr sid code)

testHeader :: KernelAddress -> ASSheetId -> EvalCode -> IO ()
testHeader addr sid code = printObj "Test evaluate python header: " =<< (runEitherT $ evaluateHeader addr sid code)

---------------------------------------------------------------------------------
-- Helpers

data EvalScope = Header | Cell deriving (Generic)
data KernelMessage = 
    EvaluateRequest { scope :: EvalScope, envSheetId :: ASSheetId, code :: String } 
  | EvaluateFormatRequest { envSheetId :: ASSheetId, code :: String }
  | GetStatusRequest ASSheetId
  | AutocompleteRequest { envSheetId' :: ASSheetId, completeString :: String }
  | ClearRequest ASSheetId
  deriving (Generic)

data KernelResponse = 
    EvaluateReply { value :: Maybe String, evalError :: Maybe String, display :: Maybe String } 
  | EvaluateFormatReply { formatValue :: Maybe String, formatError :: Maybe String }
  | GetStatusReply -- TODO
  | AutocompleteReply -- TODO
  | ClearReply Bool
  | ErrorReply String
  deriving (Generic)

instance ToJSON EvalScope

instance ToJSON KernelMessage where
  toJSON msg = case msg of 
    EvaluateRequest scope sid code -> object  [ "type" .= ("evaluate" :: String)
                                              , "scope" .= scope
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

instance FromJSON KernelResponse where
  parseJSON (Object v) = do
    val <- v .: "type" :: (Parser String)
    case val of 
      "evaluate" -> EvaluateReply <$> v .:? "value" <*> v .:? "error" <*> v .:? "display"
      "evaluate_format" -> EvaluateFormatReply <$> v .:? "value" <*> v .:? "error"
      "get_status" -> return GetStatusReply -- TODO
      "autocomplete" -> return AutocompleteReply -- TODO
      "clear" -> ClearReply <$> v .: "success"
      "error" -> ErrorReply <$> v .: "error"

evaluateWithScope :: KernelAddress -> EvalScope -> ASSheetId -> EvalCode -> EitherTExec EvalResult
evaluateWithScope _ _ _ "" = return emptyResult
evaluateWithScope addr scope sid code = do
  (EvaluateReply v err disp) <- sendMessage addr $ EvaluateRequest scope sid code
  case v of 
    Nothing -> case err of 
      Just e -> return $ EvalResult (CellValue $ ValueError e "") disp
      Nothing -> return $ EvalResult (CellValue NoValue) disp
    Just v -> do
      cval <- hoistEither $ R.parseValue Python v
      return $ EvalResult cval disp

sendMessage :: KernelAddress -> KernelMessage -> EitherTExec KernelResponse
sendMessage addr msg = do
  resp <- liftIO $ runZMQ $ do
    reqSocket <- connectToKernel addr
    send' reqSocket [] $ encode msg
    eitherDecodeStrict <$> receive reqSocket
  case resp of 
    Left e -> left $ KernelError e
    -- this is a top-level kernel error that should throw a "left"
    -- i.e. an API error, network error, or other non-evaluation-related error
    Right (ErrorReply e) -> left $ KernelError e 
    Right r -> return r

sendMessage_ :: KernelAddress -> KernelMessage -> IO ()
sendMessage_ addr msg = runZMQ $ do
  reqSocket <- connectToKernel addr 
  send' reqSocket [] $ encode msg
  return ()

connectToKernel addr = do
  reqSocket <- socket Req 
  connect reqSocket addr
  return reqSocket
----------------------------------------------------------------------------------------------------------------------------------------------
-- Old code that should go away when SQL is updated/overhauled/rewritten

execWrappedCode :: String -> EitherTExec CompositeValue
execWrappedCode evalCode = do
    result <- lift $ pyfiString evalCode
    case result of 
      Right result' -> hoistEither $ R.parseValue Python result'
      Left e -> return e

pyfiString :: String -> IO (Either CompositeValue String)
pyfiString evalStr = catch (Right <$> execString) whenCaught
    where
        execString = defVV (evalStr ++ pyString) ("Hello" :: String)
        whenCaught :: SomeException -> IO (Either CompositeValue String)
        whenCaught e = return . Left . CellValue $ ValueError (show e) "Syntax error."

pyString :: String
pyString = [str|
def export(x=None):
    return result
|]