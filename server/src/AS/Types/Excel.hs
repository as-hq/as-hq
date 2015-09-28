module AS.Types.Excel where

import AS.Types.Core
import Database.Redis (Connection)
import Data.List 
----------------------------------------------------------------------------------------------------------------------------------------------
-- | Excel

-- d1 = $ or nothing; $ means absolute column, nothing means relative. ditto for d2 but for rows
data ExLoc   = ExIndex {d1 :: String, col :: String, d2 :: String, row :: String} deriving (Show, Read, Eq, Ord)
data ExRange = ExRange {first :: ExLoc, second :: ExLoc} deriving (Show, Read, Eq, Ord)
data ExLocOrRange = ExLoc1 ExLoc | ExRange1 ExRange deriving (Show, Read, Eq, Ord)
data ExRef = ExLocOrRangeRef ExLocOrRange | ExSheetLocOrRangeRef { sheetName :: String, locOrRange :: ExLocOrRange } deriving (Show, Read, Eq, Ord)

data ExcelAction = 
  Lookup ExRef |
  CheckIndirectRef ExRef |
  LookupSheets () |
  LookupWorkbooks () |
  CurrentLocation () 
  deriving (Show,Read)

data ExcelResult = 
  ERN () |
  ERE ExRef |
  ERB Bool |
  ERV ASValue |
  ERS String

type ExcelContext = [(ExcelAction, ExcelResult)]

type BaseContext = (Connection, ASExpression, ASReference)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | String conversions

instance Show ExcelResult where 
  show (ERN _) = "Nothing"
  show (ERE l) = showExcelRef l
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
    lookupToJson ((Lookup l), r) = "'" ++ (showExcelRef l) ++ "':" ++ (show r)
    jsons = map lookupToJson b
bucketToJson b@((CheckIndirectRef _,_):_) = "IndirectRefs:{" ++ jsonBody ++ "}"
  where 
    jsons = map checkToJson b
    checkToJson ((CheckIndirectRef l),r) = "'" ++ (showExcelRef l) ++ "':" ++ (show r)
    jsonBody = intercalate "," jsons
bucketToJson ((LookupSheets _,r):[]) = "Sheets:" ++ (show r)
bucketToJson ((LookupWorkbooks _,r):[]) = "Workbooks:" ++ (show r) 
bucketToJson ((CurrentLocation _,r):[]) = "CurrentLocation:" ++ (show r)

showExcelRef :: ExRef -> String
showExcelRef exRef = case exRef of
  ExSheetLocOrRangeRef sheet rest -> sheet ++ "!" ++ (showExcelRef (ExLocOrRangeRef rest))
  ExLocOrRangeRef (ExRange1 (ExRange first second)) -> (showExcelRef $ ExLocOrRangeRef $ ExLoc1 $ first) ++ ":" ++ (showExcelRef $ ExLocOrRangeRef $ ExLoc1 second)
  ExLocOrRangeRef (ExLoc1 (ExIndex dol1 c dol2 r)) -> dol1 ++ c ++ dol2 ++ r

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