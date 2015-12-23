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

data KernelAction = EvalLocal | EvalGlobal deriving (Generic)
data KernelMessage = KernelMessage { action :: KernelAction, code :: String } deriving (Generic)
data KernelResponse = KernelResponse { value :: Maybe String, stdOutput :: String, stdErr :: String } deriving (Generic)

instance ToJSON KernelAction

instance ToJSON KernelMessage where
  toJSON (KernelMessage action code) = object ["action" .= action, "code" .= code]

instance FromJSON KernelResponse where
  parseJSON (Object v) = do
    val <- (v .:? "value") 
    KernelResponse val <$> v .: "stdOutput" <*> v .: "stdErr"

evaluate = evaluateWithAction EvalLocal
evaluateHeader = evaluateWithAction EvalGlobal

evaluateWithAction :: KernelAction -> String -> EitherTExec CompositeValue
evaluateWithAction action code = do
  resp <- sendMessage $ KernelMessage action code
  case (value resp) of 
    Nothing -> error "didn't get a value"
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


---------------------------------------------------------------------------------
-- this portion is for when we refactor ValueError and the types we return to frontend
