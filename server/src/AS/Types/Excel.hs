module AS.Types.Excel where

import AS.Types.Core
import Database.Redis (Connection)
import Data.List 
----------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel

-- TODO fix recursion
data ExLoc = ExSheet {name :: String, sheetLoc :: ExLoc} |
             ExRange {first :: ExLoc, second :: ExLoc}     |
             ExIndex {d1 :: String, col :: String, d2 :: String, row :: String} 
             deriving (Show,Read,Eq,Ord)

data ExcelAction = 
  Lookup ExLoc |
  CheckIndirectRef ExLoc |
  LookupSheets () |
  LookupWorkbooks () |
  CurrentLocation () 
  deriving (Show,Read)

data ExcelResult = 
  ERN () |
  ERE ExLoc |
  ERB Bool |
  ERV ASValue |
  ERS String

type ExcelContext = [(ExcelAction, ExcelResult)]

type BaseContext = (Connection, ASExpression, ASReference)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | String conversions

instance Show ExcelResult where 
  show (ERN _) = "Nothing"
  show (ERE l) = showExcelLoc l
  show (ERB b) = show b
  show (ERV v) = showExcelValue v
  show (ERS s) = s

instance Show ExcelContext where
  show ctx = "{" ++ jsonBody ++ "}"
    where 
      jsons = getJsonPairs ctx
      jsonBody = intercalate "," jsons

getJsonPairs :: ExcelContext -> [String]
getJsonPairs ctx = jsons
  where
    buckets = getBuckets ctx
    jsons = map bucketToJson buckets

getBuckets  :: ExcelContext -> [[(ExcelAction, ExcelResult)]]
getBuckets ctx = [lookups, indirects, sheets, workbooks, currentlocation]
  where
    lookups = filter (\b -> case b of 
      ((Lookup _ ),_) -> True
      _ -> False) ctx
    indirects = filter (\b -> case b of 
      ((CheckIndirectRef _ ),_) -> True
      _ -> False) ctx
    sheets = filter (\b -> case b of 
      ((LookupSheets _ ),_) -> True
      _ -> False) ctx
    workbooks = filter (\b -> case b of 
      ((LookupWorkbooks _ ),_) -> True
      _ -> False) ctx
    currentlocation = filter (\b -> case b of 
      ((CurrentLocation _ ),_) -> True
      _ -> False) ctx

bucketToJson :: [(ExcelAction, ExcelResult)] -> String
bucketToJson b@((Lookup _,_):_) = intercalate "," jsons
  where 
    lookupToJson ((Lookup l), r) = "'" ++ (showExcelLoc l) ++ "':" ++ (show r)
    jsons = map lookupToJson b
bucketToJson b@((CheckIndirectRef _,_):_) = "IndirectRefs:{" ++ jsonBody ++ "}"
  where 
    jsons = map checkToJson b
    checkToJson ((CheckIndirectRef l),r) = "'" ++ (showExcelLoc l) ++ "':" ++ (show r)
    jsonBody = intercalate "," jsons
bucketToJson ((LookupSheets _,r):[]) = "Sheets:" ++ (show r)
bucketToJson ((LookupWorkbooks _,r):[]) = "Workbooks:" ++ (show r) 
bucketToJson ((CurrentLocation _,r):[]) = "CurrentLocation:" ++ (show r)

showExcelLoc :: ExLoc -> String
showExcelLoc exLoc = case exLoc of
  ExSheet sheet rest -> sheet ++ "!" ++ (showExcelLoc rest)
  ExRange first second -> (showExcelLoc first) ++ ":" ++ (showExcelLoc second)
  ExIndex dol1 c dol2 r -> dol1 ++ c ++ dol2 ++ r

showExcelValue :: ASValue -> String
showExcelValue val = case val of
  ValueNaN ()   -> "Undefined"
  ValueS s      -> show s
  ValueI i      -> show i
  ValueD d      -> show d
  ValueB b      -> show b 
  ValueL l      -> toExcelList $ fmap showExcelValue l

toExcelList :: [String] -> String
toExcelList lst  = "[" ++ (intercalate "," lst) ++ "]"