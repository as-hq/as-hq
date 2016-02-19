module AS.DB.Sheets where

import AS.Prelude
import Prelude()

import AS.Types.DB
import AS.Types.Sheets

import AS.Util (getUniqueId)
import AS.Serialize (encode, maybeDecode) 

import AS.DB.API

import qualified Data.Text as T
import Database.Redis

----------------------------------------------------------------------------------------------------------------------
-- Sheets

--getSheet :: Connection -> ASSheetId -> IO (Maybe ASSheet)
--getSheet conn sid = runRedis conn $ do
--  Right sheet <- get . toRedisFormat $ SheetKey sid
--  return $ DI.bStrToSheet sheet

getAllSheets :: Connection -> IO [ASSheet]
getAllSheets conn = runRedis conn $ do
  Right sheetKeys <- smembers (toRedisFormat AllSheetsKey)
  sheets <- mget' sheetKeys
  return $ map ($fromJust . (maybeDecode =<<)) sheets

-- creates a new sheet, given a name
createSheet :: Connection -> String -> IO ASSheet
createSheet conn name = do
  sid <- T.pack <$> getUniqueId
  let sheet = Sheet sid name
  setSheet conn sheet
  return sheet

--getUniqueSheetName :: Connection -> IO String
--getUniqueSheetName conn = do
--  ss <- getAllSheets conn
--  return $ DI.getUniquePrefixedName "Sheet" $ map sheetName ss

setSheet :: Connection -> ASSheet -> IO ()
setSheet conn sheet = runRedis conn $ do
  let sheetKey = toRedisFormat . SheetKey . sheetId $ sheet
  TxSuccess _ <- multiExec $ do
      set sheetKey (encode sheet) 
      sadd (toRedisFormat AllSheetsKey) [sheetKey]  -- add the sheet key to the set of all sheets
  return ()


----------------------------------------------------------------------------------------------------------------------
-- Raw workbooks

--createWorkbook :: Connection -> [ASSheetId] -> IO ASWorkbook
--createWorkbook conn sheetids = do
--  wbName <- getUniqueWbName conn
--  let wb = Workbook wbName sheetids
--  setWorkbook conn wb
--  return wb

--getUniqueWbName :: Connection -> IO WorkbookName
--getUniqueWbName conn = do
--  wbs <- getAllWorkbooks conn
--  return $ DI.getUniquePrefixedName "Workbook" $ map workbookName wbs

--getWorkbook :: Connection -> WorkbookName -> IO (Maybe ASWorkbook)
--getWorkbook conn name = do
--    runRedis conn $ do
--        mwb <- get . toRedisFormat $ WorkbookKey name
--        case mwb of
--            Right wb -> return $ DI.bStrToWorkbook wb
--            Left _   -> return Nothing

--getAllWorkbooks :: Connection -> IO [ASWorkbook]
--getAllWorkbooks conn = runRedis conn $ do
--  Right wbKeys <- smembers . toRedisFormat $ AllWorkbooksKey
--  wbs <- mapM get wbKeys
--  return $ map (\(Right (Just w)) -> $read (BC.unpack w) :: ASWorkbook) wbs

--setWorkbook :: Connection -> ASWorkbook -> IO ()
--setWorkbook conn wb = runRedis conn $ do
--  let workbookKey = toRedisFormat . WorkbookKey . workbookName $ wb
--  TxSuccess _ <- multiExec $ do
--      set workbookKey (BC.pack . show $ wb)  -- set the workbook as key-value
--      sadd (toRedisFormat AllWorkbooksKey) [workbookKey]  -- add the workbook key to the set of all sheets
--  return ()

--workbookExists :: Connection -> WorkbookName -> IO Bool
--workbookExists conn wName = runRedis conn $ do
--  Right result <- exists . toRedisFormat $ WorkbookKey wName
--  return result

---- only removes the workbook, not contained sheets
--deleteWorkbook :: Connection -> WorkbookName -> IO ()
--deleteWorkbook conn name = runRedis conn $ do
--  let workbookKey = toRedisFormat $ WorkbookKey name
--  multiExec $ do
--    del [workbookKey]
--    srem (toRedisFormat AllWorkbooksKey) [workbookKey]
--  return ()

----------------------------------------------------------------------------------------------------------------------
-- WorkbookSheets (for frontend API)

--matchSheets :: [ASWorkbook] -> [ASSheet] -> [WorkbookSheet]
--matchSheets ws ss = [WorkbookSheet (workbookName w) (catMaybes $ lookUpSheets w ss) | w <- ws]
--  where
--    findSheet sid = find (\sh -> sid == sheetId sh) ss
--    lookUpSheets workbook sheets = map findSheet (workbookSheets workbook)

--getAllWorkbookSheets :: Connection -> IO [WorkbookSheet]
--getAllWorkbookSheets conn = do
--  ws <- getAllWorkbooks conn
--  ss <- getAllSheets conn
--  return $ matchSheets ws ss

--createWorkbookSheet :: Connection -> WorkbookSheet -> IO WorkbookSheet
--createWorkbookSheet conn wbs = do
--  let newSheets = wsSheets wbs
--  newSheets' <- mapM (createSheet conn) newSheets
--  let newSheetIds = map sheetId newSheets'
--  wbResult <- getWorkbook conn $ wsName wbs
--  case wbResult of
--    Just wb -> do
--      modifyWorkbookSheets conn (\ss -> nub $ newSheetIds ++ ss) (workbookName wb)
--      return wbs
--    Nothing -> do
--      wb <- createWorkbook conn newSheetIds
--      return $ WorkbookSheet (workbookName wb) newSheets'

--modifyWorkbookSheets :: Connection -> ([ASSheetId] -> [ASSheetId]) -> String -> IO ()
--modifyWorkbookSheets conn f wName = do
--  (Just (Workbook wsName sheetIds)) <- getWorkbook conn wName
--  let wbNew = Workbook wsName $ f sheetIds
--  setWorkbook conn wbNew
