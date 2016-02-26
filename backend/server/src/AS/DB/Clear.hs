
module AS.DB.Clear where

import Database.Redis
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Lens hiding (set)
import Control.Monad.Trans

import Prelude()
import AS.Prelude
import AS.Types.Network 
import AS.Kernels.Python as KP
import AS.Types.DB
import AS.Types.Sheets

clear :: Connection -> IO ()
clear conn = runRedis conn $ flushall >> return ()

clearSheet :: AppSettings -> Connection -> ASSheetId -> IO ()
clearSheet settings conn sid = do
  delInSheet SheetBarsKey conn sid
  delInSheet SheetCFRulesKey conn sid 
  delInSheet SheetLocsKey conn sid
  delInSheet SheetLastMessagesKey conn sid
  delInSheet SheetTempCommitsKey conn sid
  delInSheet SheetRangesKey conn sid
  -- clear python kernel for sheet
  liftIO $ KP.clear (settings^.pyKernelAddress) sid 