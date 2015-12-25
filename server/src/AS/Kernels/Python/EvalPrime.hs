{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module AS.Kernels.Python.EvalPrime where

import GHC.Generics

import AS.Kernels.Internal
import AS.Kernels.LanguageUtils
import AS.Kernels.Python.Pyfi

import qualified AS.Parsing.Read as R

import AS.Types.Cell
import AS.Types.Eval
import AS.Types.Errors
import AS.Types.Sheets
import AS.Config.Settings

import Data.Maybe (fromJust)
import Data.Aeson 
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BC

import Control.Exception (catch, SomeException)
import System.ZMQ4.Monadic

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

---------------------------------------------------------------------------------
-- this portion is a drop-in replacement for Eval

data EvalScope = Local | Global deriving (Generic)
data KernelMessage = 
    EvaluateRequest { scope :: EvalScope, envSheetId :: ASSheetId, code :: String } 
  | GetStatusRequest ASSheetId
  | AutocompleteRequest { envSheetId' :: ASSheetId, completeString :: String }
  deriving (Generic)

data KernelResponse = 
    EvaluateReply { value :: Maybe String, stdOutput :: Maybe String, stdErr :: Maybe String } 
  | GetStatusReply -- TODO
  | AutocompleteReply -- TODO
  deriving (Generic)

instance ToJSON EvalScope

instance ToJSON KernelMessage where
  toJSON msg = case msg of 
    EvaluateRequest scope sid code -> object  [ "type" .= ("evaluate" :: String)
                                              , "scope" .= scope
                                              , "sheetId" .= sid
                                              , "code" .= code]
    GetStatusRequest sid -> object  [ "type" .= ("get_status" :: String)
                                    , "sheetId" .= sid]
    AutocompleteRequest sid str -> object [ "type" .= ("autocomplete" :: String)
                                          , "sheetId" .= sid
                                          , "complete_str" .= str]

instance FromJSON KernelResponse where
  parseJSON (Object v) = do
    val <- v .: "type" :: (Parser String)
    case val of 
      "evaluate" -> EvaluateReply <$> v .:? "value" <*> v .:? "stdOput" <*> v .:? "stdErr"
      "get_status" -> return GetStatusReply -- TODO
      "autocomplete" -> return AutocompleteReply -- TODO

evaluate = evaluateWithScope Local
evaluateHeader = evaluateWithScope Global

evaluateWithScope :: EvalScope -> ASSheetId -> String -> EitherTExec CompositeValue
evaluateWithScope scope sid code = do
  (EvaluateReply v out err) <- sendMessage $ EvaluateRequest scope sid code
  case v of 
    Nothing -> error "didn't get a value" -- TODO
    Just v -> hoistEither $ R.parseValue Python v

sendMessage :: KernelMessage -> EitherTExec KernelResponse
sendMessage msg = do
  resp <- liftIO $ runZMQ $ do
    reqSocket <- socket Req
    connect reqSocket "tcp://localhost:20000"
    send' reqSocket [] $ encode msg
    eitherDecodeStrict <$> receive reqSocket
  case resp of 
    Left e -> left $ EvaluationError e
    Right r -> return r