module AS.DB.Export where

import Prelude()
import AS.Prelude

import AS.Types.Cell
import AS.Types.DB
import AS.Types.Network
import AS.Config.Settings

import AS.DB.API as DB
import AS.DB.Clear as DC
import AS.DB.Expanding as DE
import AS.DB.Graph as G (recompute)

import Control.Lens hiding ((.=))

import Database.Redis

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

importSheetData :: AppSettings -> Connection -> ExportData -> IO ()
importSheetData settings conn (ExportData cells bars descs condFormatRules headers) = do
  let sid = view (cellLocation.locSheetId) . $head $ cells
  DC.clearSheet settings conn sid 
  DB.setCells conn cells
  G.recompute (settings^.graphDbAddress) conn
  mapM_ (DB.setBar conn) bars
  mapM_ (DE.setDescriptor conn) descs
  DB.setCondFormattingRules conn sid condFormatRules
  mapM_ (DB.setEvalHeader conn) headers