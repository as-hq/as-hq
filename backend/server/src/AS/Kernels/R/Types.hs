{-# LANGUAGE DataKinds, ExistentialQuantification, RankNTypes #-}
module AS.Kernels.R.Types where

import GHC.Generics

import AS.Prelude
import Data.SafeCopy
import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Concurrent.Async
import Control.Lens
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)

import Foreign.R.Type
import Foreign.R
import Control.Memory.Region

import AS.Types.Eval (EvalResult)
import AS.Types.Cell hiding (Cell)
import AS.Types.Messages (MessageId)

-----------------------------------------------------------------------------------------------------------------------------
-- Client types

data EvalScope = Header | Cell deriving (Show, Generic)
data KernelRequest = 
    EvaluateRequest { scope :: EvalScope, evalMessageId :: MessageId, envSheetId :: ASSheetId, code :: String } 
  | ClearRequest ASSheetId
  | HaltMessageRequest MessageId
  deriving (Show, Generic)

data KernelReply = 
    EvaluateReply EvalResult
  | GenericSuccessReply
  | GenericErrorReply String
  deriving (Show, Generic)

deriveSafeCopy 1 'base ''EvalScope
deriveSafeCopy 1 'base ''KernelRequest
deriveSafeCopy 1 'base ''KernelReply

-----------------------------------------------------------------------------------------------------------------------------
-- Server types 

type Addr = String
type WorkerId = B.ByteString
type NetworkId = B.ByteString

data Worker = Worker {_workerId :: WorkerId, _networkId :: Maybe NetworkId, _thread :: Async (), _status :: WorkerStatus}
data WorkerStatus = 
    Unregistered
  | Idle
  | Busy (Maybe MessageId)
  deriving (Ord, Eq)

data State = State {_workers :: M.Map WorkerId Worker, _numWorkers :: Int, _shell :: Shell}

-- A map of pointers to R environments. There is one environment per sheet.
data Shell = Shell {_environments :: M.Map ASSheetId SEXP0}

initialState :: Shell -> State
initialState = State M.empty 0

makeLenses ''Worker
makeLenses ''State
makeLenses ''Shell

instance NFData Shell where rnf x = seq x ()
