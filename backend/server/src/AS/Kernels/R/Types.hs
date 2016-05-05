{-# LANGUAGE DataKinds, ExistentialQuantification, RankNTypes #-}

module AS.Kernels.R.Types where

import AS.Prelude
import AS.Config.Settings
import AS.Logging (getTime)
import Data.SafeCopy
import qualified Data.Map as Map
import Data.Map         (Map)
import Data.ByteString  (ByteString)
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

import System.Posix.Types

--------------------------------------------------------------------------------
-- Client types

data EvalScope = Header | Cell deriving (Show, Generic, Data)
data KernelRequest = 
  -- An evaluation request for user-supplied code
    EvaluateRequest 
      { scope :: EvalScope          -- the scope in which to execute code
      , evalMessageId :: MessageId  -- the message ID associated with an eval request
      , envSheetId :: ASSheetId     -- the sheet in which the execution is requested
      , code :: String              -- user-supplied code
      } 
  -- Clears the namespace of global (header) variables in a sheet
  | ClearRequest ASSheetId
  -- Halts execution of a previous message.
  | HaltMessageRequest MessageId
  -- Queries the status of a previous message.
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
type NetworkId = ByteString
type Message = [ByteString]

data SheetWorker = SheetWorker 
  { _workerSheetId :: ASSheetId
  , _networkId :: Maybe NetworkId
  , _process :: ProcessID 
  , _isRegistered :: Bool
  }

data CellWorker = CellWorker
  { _cellProcess :: ProcessID
  , _cellMessage :: MessageId
  }

data SheetState = SheetState
  { _sheetEnvironment :: SEXP0                -- a pointer to an environment on the R heap
  , _cellWorkers :: Map ProcessID CellWorker
  , _sheetLog :: Maybe Handle
  }

data KernelState = KernelState 
  { _sheetWorkers :: Map ASSheetId SheetWorker
  , _kernelLog :: Maybe Handle
  , _messageQueue :: Map ASSheetId [Message]
  }

newKernelState :: Maybe Handle -> KernelState
newKernelState l = KernelState 
  { _sheetWorkers = Map.empty
  , _kernelLog = l 
  , _messageQueue = Map.empty
  }

newSheetState :: SEXP0 -> Maybe Handle -> SheetState
newSheetState e h = SheetState 
  { _sheetEnvironment = e
  , _cellWorkers = Map.empty
  , _sheetLog = h
  }

makeLenses ''SheetWorker
makeLenses ''CellWorker
makeLenses ''KernelState
makeLenses ''SheetState

--------------------------------------------------------------------------------
-- Helper functions

class LoggableState a where
  getLog :: a -> IO (Maybe Handle)

instance LoggableState (MVar KernelState) where
  getLog st = view kernelLog <$> readMVar st

instance LoggableState (MVar SheetState) where
  getLog st = view sheetLog <$> readMVar st 

closeLog :: (MonadIO m, LoggableState s) => s -> m ()
closeLog st = liftIO $ getLog st >>= \log -> 
  maybe (return ()) hClose log

flushLog :: (MonadIO m, LoggableState s) => s -> m ()
flushLog st = liftIO $ getLog st >>= \log -> 
  maybe (return ()) hFlush log

puts :: (MonadIO m, LoggableState s) => s -> String -> m ()
puts st x = liftIO $ getLog st >>= \log -> do
  putStrLn x 
  maybe (return ()) (\l -> hPutStrLn l x) log

putsTimed :: (MonadIO m, LoggableState s) => s -> String -> m ()
putsTimed st x = liftIO $ do
  t <- getTime
  let x' = x ++ " [" ++ t ++ "]"
  puts st x'

--------------------------------------------------------------------------------
