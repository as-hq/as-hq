module AS.DB.Export where

import Control.Lens hiding ((.=))
import Database.Redis

import AS.Config.Settings
import AS.Prelude
import AS.Types.Cell
import AS.Types.DB
import AS.Types.Network
import AS.Config.Settings
import AS.Types.Graph 

import AS.DB.API as DB
import AS.DB.Clear as DC
import AS.DB.Transaction as DT

-------------------------------------------------------------------------------------------------------------------------
-- This module is for database functions associated with exporting/importing data.

exportSheetData :: Connection -> ASSheetId -> IO ExportData
exportSheetData conn sid = do
  cells <- DB.getCellsInSheet conn sid
  bars <- DB.getBarsInSheet conn sid
  descs <- DB.getRangeDescriptorsInSheet conn sid
  condFormatRules <- DB.getCondFormattingRulesInSheet conn sid
  headers <- mapM (DB.getEvalHeader conn sid) headerLangs
  return $ ExportData cells bars descs condFormatRules headers

importSheetData :: Connection -> ExportData -> IO ()
importSheetData conn (ExportData cells bars descs condFormatRules headers) = do
  let sid = view (cellLocation.locSheetId) . $head $ cells
  DC.clearSheet conn sid 
  DT.setCellsPropagated conn cells
  mapM_ (DB.setBar conn) bars
  mapM_ (DB.setDescriptor conn) descs
  DB.setCondFormattingRules conn sid condFormatRules
  mapM_ (DB.setEvalHeader conn) headers