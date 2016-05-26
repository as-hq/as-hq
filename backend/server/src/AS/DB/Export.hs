module AS.DB.Export where

import Database.Redis

import AS.Config.Settings
import AS.Prelude
import AS.Types.Cell
import AS.Types.DB
import AS.Types.Network
import AS.Config.Settings
import AS.Types.Graph 
import AS.Types.User

import AS.DB.API          as DB
import AS.DB.Clear        as DB
import AS.DB.Transaction  as DB

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T

-------------------------------------------------------------------------------------------------------------------------
-- This module is for database functions associated with exporting/importing data.

-- does not consider headers, which are workbook-wide.
exportSheetData :: Connection -> SheetID -> IO SheetExportData
exportSheetData conn sid = do
  sname <- view sheetName . fromJust <$> DB.getSheet conn sid
  cells <- DB.getCellsInSheet conn sid
  bars <- DB.getBarsInSheet conn sid
  descs <- DB.getRangeDescriptorsInSheet conn sid
  condFormatRules <- DB.getCondFormattingRulesInSheet conn sid
  return $ SheetExportData
    { _exportSheetId         = sid
    , _exportSheetName       = sname
    , _exportCells           = cells
    , _exportBars            = bars
    , _exportDescriptors     = descs
    , _exportCondFormatRules = condFormatRules
    }

-- does not consider headers, which are workbook-wide.
importSheetData :: Connection -> UserID -> SheetExportData -> IO SheetID
importSheetData conn uid ex = do
  sheet <- DB.createSheet conn uid $ ex^.exportSheetName
  let sid = sheet^.sheetId
  let ex' = cloneSheetData sid ex
  DB.setCellsPropagated conn uid $ ex'^.exportCells
  DB.setBars conn $ ex'^.exportBars
  DB.setDescriptors conn $ ex'^.exportDescriptors
  DB.setCondFormattingRules conn sid $ ex'^.exportCondFormatRules
  return sid

-- | Make a carbon-copy of a sheet. will not copy headers since they 
-- are a workbook-level construct.
cloneSheet :: Connection -> UserID -> SheetID -> IO SheetID
cloneSheet conn uid sid = 
  exportSheetData conn sid >>= importSheetData conn uid

exportWorkbookData :: Connection -> WorkbookID -> IO WorkbookExportData
exportWorkbookData conn wid = do
  wb <- fromJust <$> DB.getWorkbook conn wid
  headers <- mapM (DB.getEvalHeader conn wid) headerLangs
  sheets <- mapM (exportSheetData conn) . Set.toList $ wb^.workbookSheetIds
  return $ WorkbookExportData 
    { _exportWorkbookId     = wb^.workbookId 
    , _exportWorkbookName   = wb^.workbookName
    , _exportLastOpenSheet  = wb^.lastOpenSheet
    , _exportWorkbookSheets = sheets
    , _exportHeaders        = headers
    }

importWorkbookData :: Connection -> UserID -> WorkbookExportData -> IO WorkbookID
importWorkbookData conn uid ex = do
  cloneSids <- mapM (importSheetData conn uid) $ ex^.exportWorkbookSheets
  let sids = map (view exportSheetId) $ ex^.exportWorkbookSheets
  let cloneLastOpen = (Map.fromList $ zip sids cloneSids) Map.! (ex^.exportLastOpenSheet)
  wid <- T.pack <$> getUUID
  DB.setWorkbook conn $
    Workbook wid (ex^.exportWorkbookName) (Set.fromList cloneSids) uid cloneLastOpen
  let ex' = cloneWorkbookData wid ex
  mapM_ (DB.setEvalHeader conn) $ ex'^.exportHeaders
  return wid

-- | Make a carbon-copy of a workbook.
cloneWorkbook :: Connection -> UserID -> WorkbookID -> IO WorkbookID
cloneWorkbook conn uid wid = 
  exportWorkbookData conn wid >>= importWorkbookData conn uid
