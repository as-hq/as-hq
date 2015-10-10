module AS.Eval.Excel where

import Prelude
import Database.Redis(Connection)

import AS.Types.Core
import AS.Types.Excel
import AS.Parsing.In (parseValue)
import AS.Eval.Core (pyfiString)
import AS.DB.API as DB
import AS.Parsing.Out as O

-----------------------------------------------------------------------------------------------------------------------
-- Exposed functions

evalExcel :: Connection -> ASCell -> IO ASValue
evalExcel conn c@(Cell l xp v ts) = do
    initContext <- getInitialContext c
    transResult <- transformExpression (conn,xp,l) initContext
    case transResult of 
        (Left e) -> return e
        (Right xp') -> do
            let xp = "evalExcel('" ++ (expression xp') ++ "')"
            result <- pyfiString interpolated
            return $ parseValue Excel result

-------------------------------------------------------------------------------------------------------------------------
---- Excel helpers

getInitialContext :: ASCell -> IO [(ExRef, ASValue)]
getInitialContext cell = do
    ancs <- DB.getCells (concat deps)
    vals = map cellValue ancs
    return $ zip exRefs vals
    where
        sheetid = locSheetId l
        (inter, exRefs) = getMatchesWithContext str excelMatch
        deps = getDependenciesFromExRefs sheetid exRefs (getOffsets l)

transformExpression :: BaseContext -> ExcelContext -> IO (Either ASValue ASExpression) 
transformExpression bc@(conn,xp,loc) ctx = do
    let newXp = "transformExcel('" ++ (expression xp) ++ "', " ++ (show ctx) ++ ")"
    resultInit <- pyfiString interpolated
    let result = parseValue Excel resultInit
    case result of 
        e@(ValueError _ _ _ _) -> return $ Left e
        _ -> return . Left $ ValueError "Could not transform Excel expression" "ExcelParse" "" -1


parseLookupMap :: [a] -> Either ASValue [ExcelAction]
parseLookupMap lst = Right [] -- TODO

-- args: base context, old context, new actions
getNewContext :: BaseContext -> ExcelContext -> [ExcelAction] -> IO ExcelContext
getNewContext bc ctx acts = return $ zip acts =<< mapM (performExcelAction bc) acts

-- args: (original xp, original location) -> desired action -> result
performExcelAction :: BaseContext -> ExcelAction -> IO ExcelResult
performExcelAction _ (Lookup loc) = DB.getCell loc' >>= \c -> 
    | (Just cell) = return . ERV $ cellValue cell
    | Nothing  = return $ ERN ()
    where loc' = O.exRefToASRef loc 
performExcelAction (_,_,loc) (CheckIndirectRef l) = do
    let sheetid = locSheetId loc
    c <- DB.getCell $ exRefToASRef sheetid l
    case c of 
        Nothing -> return $ ERB False
        (Just cell) -> case (parseExcelLoc . cellValue $ c) of
            (Left e) -> return $ ERB False
            (Right l) -> do
                c' <- DB.getCell $ exRefToASRef sheetid l'
                case c' of 
                    Nothing -> return $ ERB False
                    (Just cell') -> return . ERV . cellValue $ cell'
performExcelAction (conn,_,_) (LookupSheets _) = do
    sheets <- DB.getAllSheets conn
    let str = intercalate "," $ map sheetName sheets
    return $ ERS str
performExcelAction (conn,_,_) (LookupWorkbooks _) = do
    workbooks <- DB.getAllWorkbooks conn
    let str = intercalate "," $ map workbookName workbooks
    return $ ERS str
performExcelAction (_,_,loc) (CurrentLocation _) = return $ ERE (O.asRefToExRef loc) 
