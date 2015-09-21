module AS.Eval.Excel where

import Prelude
import Database.Redis(Connection)

import AS.Types 
import AS.Parsing.In (parseValue)
import AS.Eval.Core (pyfiString)

-----------------------------------------------------------------------------------------------------------------------
-- Exposed functions

--evalExcel :: Connection -> ASCell -> IO ASValue
--evalExcel conn cell = do


-------------------------------------------------------------------------------------------------------------------------
---- Excel helpers

--transformExpression :: ASExpression -> IO (Either ASValue (ASExpression, [ExcelAction, ExcelPayload]))
--transformExpression xp = do
--    let newXp = "evalExcel('"++(expression xp)++"')"
--    resultInit <- pyfiString interpolated
--    let result = parseValue Excel resultInit
--    return $ case result of 
--        (ValueL a) -> case parseLookupMap a of 
--            e@(ValueError _ _ _ _) -> Left e
--            lookupMap -> Right lookupMap
--        e@(ValueError _ _ _ _) -> Left e
--        _ -> Left $ ValueError "Could not transform Excel expression" "ExcelParse" "" -1


--parseLookupMap :: [a] -> Either ASValue [ExcelAction, ExcelPayload]
--parseLookupMap lst = 
--    

--performExcelAction :: ExcelAction -> ExcelPayload -> ExcelResult
--performExcelAction Lookup (PayloadE a) = 