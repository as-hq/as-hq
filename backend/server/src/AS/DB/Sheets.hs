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

getAllSheets :: Connection -> IO [ASSheet]
getAllSheets conn = runRedis conn $ do
  Right sheetKeys <- smembers (toRedisFormat AllSheetsKey)
  sheets <- mget' sheetKeys
  return $ map ($fromJust . (maybeDecode =<<)) sheets

-- create a new sheet by name
createSheet :: Connection -> String -> IO ASSheet
createSheet conn name = do
  sid <- T.pack <$> getUniqueId
  let sheet = Sheet sid name
  setSheet conn sheet
  return sheet

setSheet :: Connection -> ASSheet -> IO ()
setSheet conn sheet = runRedis conn $ do
  let sheetKey = toRedisFormat . SheetKey . sheetId $ sheet
  TxSuccess _ <- multiExec $ do
      set sheetKey (encode sheet) 
      sadd (toRedisFormat AllSheetsKey) [sheetKey]  -- add the sheet key to the set of all sheets
  return ()
