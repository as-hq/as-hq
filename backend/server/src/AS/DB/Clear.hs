
module AS.DB.Clear where

import Database.Redis
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Lens hiding (set)
import Control.Monad.Trans

import AS.Config.Settings
import AS.DB.Graph as G
import AS.Prelude
import AS.Types.Network 
import AS.Kernels.Python as KP
import AS.Types.DB
import AS.Types.Sheets

clear :: Connection -> IO ()
clear conn = runRedis conn $ flushall >> return ()

clearSheet :: Connection -> ASSheetId -> IO ()
clearSheet conn sid = do
  delInSheet SheetBarsKey conn sid
  delInSheet SheetCFRulesKey conn sid 
  delInSheet SheetLocsKey conn sid
  delInSheet SheetLastMessagesKey conn sid
  delInSheet SheetTempCommitsKey conn sid
  delInSheet SheetRangesKey conn sid
  G.recomputeSheetDAG conn sid

  -- clear python kernel for sheet
  liftIO $ KP.clear sid 
