module AS.Types.Logging where

import AS.Prelude

import AS.Types.Network
import AS.Types.Commits

import qualified Data.Map as M
import Control.Concurrent.Chan 
import Control.Lens

-----------------------------------------------------------------------------------------------------------------------------
-- Logging

data LogSource = 
    MainLog
  | SheetLog ASSheetId
  | ErrorLog CommitSource
  | BugLog CommitSource
  | ConsoleLog
  deriving (Eq, Ord)

data LogMessage = 
    Log LogSource String
  | CloseLog

type Logger = Chan LogMessage

data LoggerState = LoggerState
  { _handles :: M.Map LogSource Handle
  }

makeLenses ''LoggerState