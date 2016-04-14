{-# LANGUAGE DataKinds, ExistentialQuantification, RankNTypes #-}

module AS.Kernels.R.Types where

import AS.Prelude
import AS.Logging (getTime)
import Data.SafeCopy
import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Control.Monad.IO.Class (MonadIO)

import Foreign.R.Type
import Foreign.R
import Control.Memory.Region

import AS.Types.Eval (EvalResult)
import AS.Types.Cell hiding (Cell)
import AS.Types.Messages (MessageId)

--------------------------------------------------------------------------------
-- Settings

logging_on         = False

--------------------------------------------------------------------------------
-- Client types

data EvalScope = Header | Cell deriving (Show, Generic, Data)
data KernelRequest = 
    EvaluateRequest { scope :: EvalScope, 
                      evalMessageId :: MessageId, 
                      envSheetId :: ASSheetId, 
                      code :: String } 
  | ClearRequest ASSheetId
  | HaltMessageRequest MessageId
  | GetStatusRequest MessageId
  deriving (Show, Generic, Data)

data KernelReply = 
    EvaluateReply EvalResult
  | GenericSuccessReply
  | GenericErrorReply String
  | StillProcessingReply
  deriving (Show, Generic, Eq)

deriveSafeCopy 1 'base ''EvalScope
deriveSafeCopy 1 'base ''KernelRequest
deriveSafeCopy 1 'base ''KernelReply

--------------------------------------------------------------------------------
-- Server types 

type Addr = String
type WorkerId = B.ByteString
type NetworkId = B.ByteString

data Worker = Worker {_workerId :: WorkerId, 
                      _networkId :: Maybe NetworkId, 
                      _thread :: Async (), 
                      _status :: WorkerStatus }
data WorkerStatus = 
    Unregistered
  | Idle
  | Busy (Maybe MessageId)
  deriving (Ord, Eq)

data State = State 
  { _workers :: M.Map WorkerId Worker
  , _numWorkers :: Int
  , _shell :: Shell
  , _log :: Maybe Handle}

emptyState :: Shell -> Maybe Handle -> State
emptyState s l = State M.empty 0 s l 

-- A map of pointers to R environments. There is one environment per sheet.
data Shell = Shell {_environments :: M.Map ASSheetId SEXP0}

makeLenses ''Worker
makeLenses ''State
makeLenses ''Shell

instance NFData Shell where rnf x = seq x ()

--------------------------------------------------------------------------------
-- Helper functions

closeLog :: (MonadIO m) => MVar State -> m ()
closeLog st = liftIO $ readMVar st >>= \st_ -> hClose ($fromJust $ st_^.log)

flushLog :: (MonadIO m) => MVar State -> m ()
flushLog st = liftIO $ readMVar st >>= \st_ -> hFlush ($fromJust $ st_^.log)

puts :: (MonadIO m) => MVar State -> String -> m ()
puts st x = liftIO $ do
  putStrLn x 
  when logging_on $ do
    _st <- readMVar st
    hPutStrLn ($fromJust $ _st^.log) x

putsTimed :: (MonadIO m) => MVar State -> String -> m ()
putsTimed st x = liftIO $ do
  t <- getTime
  let x' = x ++ " [" ++ t ++ "]"
  puts st x'

--------------------------------------------------------------------------------
