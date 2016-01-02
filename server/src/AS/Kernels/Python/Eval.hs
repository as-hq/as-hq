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
import AS.Types.Errors
import AS.Types.Sheets

import qualified AS.DB.API as DB
import qualified AS.DB.Eval as DE

import Data.Maybe (fromJust)
import Data.Aeson 
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

import Control.Exception (catch, SomeException)
import System.ZMQ4.Monadic

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Database.Redis (Connection)

---------------------------------------------------------------------------------
-- Exposed functions

initialize :: Connection -> IO ()
initialize conn = do
  -- run all the headers in db to initialize the sheet namespaces
  sids <- map sheetId <$> DB.getAllSheets conn
  headers <- mapM (\sid -> DE.getEvalHeader conn sid Python) sids
  mapM_ (\(sid, code) -> runEitherT $ evaluateHeader sid code) $ zip sids headers

evaluate = evaluateWithScope Cell
evaluateHeader = evaluateWithScope Header
clear = sendMessage_ . ClearRequest

-- SQL has not been updated to use the new kernel yet, because it doesn't seem super urgent rn.
-- #anand 1/1/16
evaluateSql :: String -> String -> EitherTExec CompositeValue
evaluateSql _ "" = return $ CellValue NoValue
evaluateSql header str = do
    validCode <- formatCode header SQL str
    if isDebug
        then lift $ writeExecFile SQL validCode
        else return ()
    execWrappedCode validCode

testCell :: ASSheetId -> EvalCode -> IO ()
testCell sid code = printObj "Test evaluate python cell: " =<< (runEitherT $ evaluate sid code)

testHeader :: ASSheetId -> EvalCode -> IO ()
testHeader sid code = printObj "Test evaluate python header: " =<< (runEitherT $ evaluateHeader sid code)

---------------------------------------------------------------------------------
-- Helpers

data EvalScope = Header | Cell deriving (Generic)
data KernelMessage = 
    EvaluateRequest { scope :: EvalScope, envSheetId :: ASSheetId, code :: String } 
  | GetStatusRequest ASSheetId
  | AutocompleteRequest { envSheetId' :: ASSheetId, completeString :: String }
  | ClearRequest ASSheetId
  deriving (Generic)

data KernelResponse = 
    EvaluateReply { value :: Maybe String, eval_error :: Maybe String, display :: Maybe String } 
  | GetStatusReply -- TODO
  | AutocompleteReply -- TODO
  | ClearReply Bool
  deriving (Generic)

instance ToJSON EvalScope

instance ToJSON KernelMessage where
  toJSON msg = case msg of 
    EvaluateRequest scope sid code -> object  [ "type" .= ("evaluate" :: String)
                                              , "scope" .= scope
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
      "get_status" -> return GetStatusReply -- TODO
      "autocomplete" -> return AutocompleteReply -- TODO
      "clear" -> ClearReply <$> v .: "success"

evaluateWithScope :: EvalScope -> ASSheetId -> String -> EitherTExec CompositeValue
evaluateWithScope _ _ "" = return $ CellValue NoValue
evaluateWithScope scope sid code = do
  (EvaluateReply v err disp) <- sendMessage $ EvaluateRequest scope sid code
  case v of 
    Nothing -> case err of 
      Just e -> return . CellValue $ ValueError e ""
      Nothing -> printWithTimeT "ERROR: received neither value nor error from kernel." >> left KernelError
    Just v -> hoistEither $ R.parseValue Python v

sendMessage :: KernelMessage -> EitherTExec KernelResponse
sendMessage msg = do
  resp <- liftIO $ runZMQ $ do
    reqSocket <- connectToKernel
    send' reqSocket [] $ encode msg
    eitherDecodeStrict <$> receive reqSocket
  case resp of 
    Left e -> left $ EvaluationError e
    Right r -> return r

sendMessage_ :: KernelMessage -> IO ()
sendMessage_ msg = runZMQ $ do
  reqSocket <- connectToKernel
  send' reqSocket [] $ encode msg
  return ()

connectToKernel = do
  reqSocket <- socket Req 
  connect reqSocket pykernelHost
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