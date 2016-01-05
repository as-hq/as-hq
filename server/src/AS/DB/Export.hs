module AS.DB.Export where

import Prelude

import AS.Types.Cell
import AS.Types.DB
import AS.Types.Network

import AS.DB.API as DB
import AS.DB.Clear as DC
import AS.DB.Expanding as DE
import AS.DB.Graph as G (recompute)

import Control.Lens hiding ((.=))

import Database.Redis

-------------------------------------------------------------------------------------------------------------------------
-- This module is for database functions associated with exporting/importing data.

exportData :: Connection -> ASSheetId -> IO ExportData
exportData conn sid = do
  cells <- DB.getCellsInSheet conn sid
  descs <- DB.getRangeDescriptorsInSheet conn sid
  return $ ExportData cells descs

importData :: AppSettings -> Connection -> ExportData -> IO ()
importData settings conn (ExportData cs descriptors) = do
  DC.clearSheet settings conn $ locSheetId . view cellLocation . head $ cs -- assumes all cells are in the same sheet.
  DB.setCells conn cs
  G.recompute (settings^.graphDbAddress) conn
  mapM_ (DE.setDescriptor conn) descriptors
