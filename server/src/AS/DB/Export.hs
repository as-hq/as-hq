module AS.DB.Export where

import Prelude

import AS.Types.Cell
import AS.Types.DB
import AS.DB.API as DB
import AS.DB.Expanding as DE
import AS.DB.Graph as G (recompute)

import Database.Redis

-------------------------------------------------------------------------------------------------------------------------
-- This module is for database functions associated with exporting/importing data.

exportData :: Connection -> ASSheetId -> IO ExportData
exportData conn sid = do
  cells <- DB.getCellsInSheet conn sid
  descs <- DB.getRangeDescriptorsInSheet conn sid
  return $ ExportData cells descs

importData :: Connection -> ExportData -> IO ()
importData conn (ExportData cs descriptors) = do
  DB.clearSheet conn $ locSheetId . cellLocation . head $ cs -- assumes all cells are in the same sheet.
  DB.setCells cs
  G.recompute conn
  mapM_ (DE.couple conn) descriptors
