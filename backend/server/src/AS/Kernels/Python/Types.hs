{-# LANGUAGE DataKinds, ExistentialQuantification, RankNTypes #-}

module AS.Kernels.Python.Types where

import Data.Aeson 
import Data.Aeson.Types (Parser)
import Data.SafeCopy
import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Control.Monad.IO.Class (MonadIO)
import System.ZMQ4.Monadic


import AS.Prelude
import AS.Logging (getTime)
import AS.Types.Eval (EvalResult)
import AS.Types.Cell hiding (Cell)
import AS.Types.Messages (MessageId)

--------------------------------------------------------------------------------
-- Types

data EvalScope = Header | Cell deriving (Generic)
data KernelRequest = 
    EvaluateRequest { 
      scope :: EvalScope, 
      evalMessageId :: MessageId, 
      evalWorkbookId :: WorkbookID, 
      code :: String } 
  | EvaluateFormatRequest { formatorkbookId :: WorkbookID, code :: String }
  | ClearRequest WorkbookID
  | HaltMessageRequest MessageId
  | GetStatusRequest MessageId
  deriving (Generic)

data KernelReply = 
    EvaluateReply { 
      value :: Maybe String, 
      evalError :: Maybe String, 
      display :: Maybe String } 
  | EvaluateFormatReply { 
      formatValue :: Maybe String, 
      formatError :: Maybe String }
  | GenericSuccessReply
  | GenericErrorReply String
  | StillProcessingReply
  deriving (Generic, Eq)

--------------------------------------------------------------------------------
-- Standalone instances

instance ToJSON EvalScope

instance ToJSON KernelRequest where
  toJSON msg = case msg of 
    EvaluateRequest scope mid wid code -> object  
      [ "type" .= ("evaluate" :: String)
      , "scope" .= scope
      , "message_id" .= mid
      , "workbook_id" .= wid
      , "code" .= code
      ]
    EvaluateFormatRequest wid code -> object  
      [ "type" .= ("evaluate_format" :: String)
      , "workbook_id" .= wid
      , "code" .= code
      ]
    ClearRequest wid -> object 
      [ "type" .= ("clear" :: String)
      , "workbook_id" .= wid
      ]
    HaltMessageRequest mid -> object 
      [ "type" .= ("halt_message" :: String)
      , "message_id" .= mid
      ]
    GetStatusRequest mid -> object 
      [ "type" .= ("get_status" :: String)
      , "message_id" .= mid
      ]

instance FromJSON KernelReply where
  parseJSON (Object v) = do
    val <- v .: "type" :: (Parser String)
    case val of 
      "evaluate" -> 
        EvaluateReply <$> v .:? "value" <*> v .:? "error" <*> v .:? "display"
      "evaluate_format" -> 
        EvaluateFormatReply <$> v .:? "value" <*> v .:? "error"
      "still_processing" -> return StillProcessingReply
      "generic_success" -> return GenericSuccessReply
      "generic_error" -> GenericErrorReply <$> v .: "error"

--------------------------------------------------------------------------------