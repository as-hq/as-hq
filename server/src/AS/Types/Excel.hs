module AS.Types.Excel where

import AS.Types
import Database.Redis (Connection)

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

type BaseContext = (Connection, ASExpression, ASLocation)

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

bucketToJson :: [(ExcelAction, ExcelResult)] -> String
bucketToJson b@((Lookup _,_):_) = map lookupToJson b
  where lookupToJson ((Lookup l), r) = "'" ++ (showExcelLoc l) ++ "':" ++ (show r)
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
showExcelValue val = case v of
  ValueNaN ()   -> "Undefined"
  ValueS s      -> show s
  ValueI i      -> show i
  ValueD d      -> show d
  ValueB b      -> show b 
  ValueL l      -> toExcelList $ fmap (showValue lang) l

toExcelList :: [String] -> String
toExcelList lst  = "[" ++ (L.intercalate delim lst) ++ start