{-# LANGUAGE TemplateHaskell #-}

module AS.Types.Logging where

import Control.Concurrent.Chan 
import qualified Data.Map as M

import AS.Prelude
import AS.Types.Network
import AS.Types.Commits
import AS.Types.Sheets


-----------------------------------------------------------------------------------------------------------------------------
-- Logging

data LogSource = 
    MainLog
  | SheetLog SheetID
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