module AS.Kernels.API where

import AS.Prelude
import AS.Config.Settings

import AS.Types.Cell
import AS.Types.Sheets
import AS.Types.EvalHeader

import qualified AS.Kernels.Python.Client as Python
import qualified AS.Kernels.R.Client      as R

import qualified AS.DB.API as DB

import Database.Redis (Connection)

--------------------------------------------------------------------
-- This module provides a common interface to the kernels.

openWorkbook :: Connection -> WorkbookID -> IO ()
openWorkbook conn wid = do
  forM_ headerLangs $ \lang -> do
   header <- DB.getEvalHeader conn wid lang
   case lang of  
    Python -> Python.openWorkbook wid $ header^.evalHeaderExpr
    R      -> R.openWorkbook wid $ header^.evalHeaderExpr
