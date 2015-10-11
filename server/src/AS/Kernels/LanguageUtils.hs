module AS.Kernels.LanguageUtils where

import AS.Types.Core


import Prelude
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T
import qualified Data.List as L

import AS.Config.Paths
import AS.Parsing.Out
import AS.Parsing.Common

import Control.Applicative hiding ((<|>))
import System.IO.Strict as S   
import System.IO

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Maybe as MB
import Data.List.Split as SP
import qualified Data.Map as M
import qualified Prelude as P


import AS.Parsing.Common as C

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-----------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

introspectCode :: ASLanguage -> String -> EitherTExec String
introspectCode lang str = do 
    let trimmed = trimWhitespace lang str
    let onSuccess = (wrapCode lang False) . recombineLines 
    let onFailure _ = return ExpressionNotEvaluable
    case lang of 
        SQL         -> lift $ wrapCode SQL False trimmed
        otherwise   -> case (tryPrintingLast lang trimmed) of
            (Left _)        -> left ExpressionNotEvaluable
            (Right result)  -> lift $ wrapCode lang False $ recombineLines result

-- returns (repl record code, repl eval code)
introspectCodeRepl :: ASLanguage -> String -> IO (String, String)
introspectCodeRepl lang str = do
    let trimmed = trimWhitespace lang str
    case (tryPrintingLast lang trimmed) of
        (Left _) -> return $ (trimmed, emptyExpression) -- nothing to print, so nothing to evaluate
        (Right (recordXp, printedLine)) -> do
            evalXp <- wrapCode lang True $ recombineLines (recordXp, printedLine)
            return (recordXp, evalXp)

-----------------------------------------------------------------------------------------------------------------------
-- | Helpers

tryPrintingLast :: ASLanguage -> String -> Either () (String, String)
tryPrintingLast lang str = 
    let (startLines, endLine) = splitLastLine lang str
    in if (isPrintable lang endLine)
        then (Left ())
        else Right (startLines, printCmd lang endLine)

isPrintable :: ASLanguage -> String -> Bool
isPrintable lang = containsAny [assignOp lang, returnOp lang, importOp lang]

printCmd :: ASLanguage -> String -> String
printCmd lang str = case (tryParse (replacePrintStmt lang) str) of 
    (Left _) -> addPrintCmd lang str
    (Right printed) -> printed 

replacePrintStmt :: ASLanguage -> Parser String -- TODO generalize
replacePrintStmt lang = case lang of 
    Python -> do
        spaces >> string "print" >> spaces
        opener <- option ' ' $ try (char '(')
        let closer = if (opener == '(')
            then char ')' >> spaces
            else spaces
        arg <- manyTill anyChar (try (closer >> eof))
        return $ "result = " ++ arg

splitLastLine :: ASLanguage -> String -> (String, String)
splitLastLine lang str = case (tryParse (parseLastline lang) (reverse str)) of 
    (Right result)  -> result
    (Left _)        -> (emptyExpression, str) -- only one line, which becomes the 'last' line

parseLastline :: ASLanguage -> Parser (String, String)
parseLastline lang = do
    endLine <- manyTill anyChar $ string "\n" <|> string (lineDelim lang)
    startLines <- manyTill anyChar (try eof)
    return (reverse startLines, reverse endLine)

lastLine :: ASLanguage ->  Parser String
lastLine lang = do
    string "\n" <|> string (lineDelim lang)
    manyTill anyChar eof

addPrintCmd :: ASLanguage -> String -> String
addPrintCmd lang str = case lang of 
    R       -> str
    Python  -> "result = " ++ str 
    OCaml   -> "print_string(Std.dump(" ++ str ++ "))"
    SQL     -> "result = pprintSql(db(\'" ++ str ++ "\'))" -- hardcoded db() function usage for demos

recombineLines :: (String, String) -> String
recombineLines ("", endLine) = endLine
recombineLines (startLines, "") = startLines
recombineLines (startLines, endLine) = startLines ++ "\n" ++ endLine

trimWhitespace :: ASLanguage -> String -> String 
trimWhitespace lang = L.dropWhileEnd isWhitespace . L.dropWhile isWhitespace
    where isWhitespace c = (c == ' ') || (c == '\n') || (c == '\t') || (c == ';')

-- TODO fix
--trim :: ASLanguage -> Parser String
--trim lang = do
--    try $ skipMany1 (whitespace lang)
--    manyTill anyChar (try eof) 

--whitespace :: ASLanguage -> Parser ()
--whitespace SQL = 
--        (spaces >> return ())
--    <|> (char '\n' >> return ())
--    <|> (char '\t' >> return ())
--whitespace lang = 
--        (spaces >> return ())
--    <|> (char '\n' >> return ())
--    <|> (char '\t' >> return ())
--    <|> (string (lineDelim lang) >> return ())

wrapCode :: ASLanguage -> Bool -> String -> IO String
wrapCode lang isRepl str =  
    let 
        insertedCode = case lang of 
            Python -> replaceSubstrings str [("\n", "\n\t")]
            SQL -> replaceSubstrings str [("\n", "\n\t")]
            otherwise -> str
    in do
        template <- if isRepl
            then getTemplateRepl lang
            else getTemplate lang
        return $ replaceSubstrings template [("#CODE#", insertedCode)]

-----------------------------------------------------------------------------------------------------------------------
-- | Language-specific string modifiers

lineDelim :: ASLanguage -> String
lineDelim lang = case lang of 
    Python  -> ";"
    R       -> ";"
    OCaml   -> ";;"

assignOp :: ASLanguage -> String
assignOp lang = case lang of 
    R           -> "<-"
    otherwise   -> "="

returnOp :: ASLanguage -> String
returnOp lang = case lang of 
    otherwise -> "return"

importOp :: ASLanguage -> String
importOp lang = case lang of 
    Python  -> "import"
    R       -> "library"

getRunnerCmd :: ASLanguage -> String
getRunnerCmd lang = case lang of 
    R       -> "Rscript "
    OCaml   -> "ocamlfind ocamlc -linkpkg -thread -package extlib -package core "

getRunnerArgs :: ASLanguage -> IO [String]
getRunnerArgs lang = do
    evalPath <- getEvalPath
    return $ case lang of 
        OCaml -> ["-o " ++ path ++ "test"]
            where 
                path = evalPath ++ "ocaml/"
        CPP -> ["-o " ++ path ++ "testCPP && " ++ path ++ "testCPP"]
            where
                path = evalPath ++ "cpp/"
        otherwise -> [] -- in case we ever use more args


formatRunArgs :: ASLanguage -> String -> String -> [String] -> String
formatRunArgs lang cmd filename args = case lang of 
    otherwise -> cmd ++ filename ++ " " ++ (L.intercalate " " args)

addCompileCmd :: ASLanguage -> String -> IO String
addCompileCmd OCaml cmd = do
    evalPath <- getEvalPath
    let path = evalPath ++ "ocaml/"
    return $ cmd ++ "; " ++ path ++ "test"

-- | Helper function for interpolate. Takes in a RefValMap and a reference and returns
-- the value as a string. 
lookupString :: ASLanguage -> RefValMap -> ASReference -> String
lookupString lang mp loc = case loc of
    IndexRef (Index sh (a,b)) -> (showValue lang) (mp M.! loc)
    RangeRef (Range sh ((a,b),(c,d))) -> 
        if (c==a)
            then modifiedLists lang (toListStr lang [ ((showValue lang) (mp M.! (IndexRef $ Index sh (a,row)))) | row<-[b..d]])
            else modifiedLists lang (toListStr lang [modifiedLists lang (toListStr lang ([(showValue lang) (mp M.! (IndexRef $ Index sh (col,row)))| col <-[a..c]]))| row<-[b..d]])


-- | Replaces all the Excel references in an expression with the values corresponding to them. 
-- TODO clean up SQL mess
insertValues :: ASSheetId -> RefValMap -> ASExpression -> String
insertValues sheetid values (Expression origString SQL) = contextStmt ++ evalStmt
    where
        exRefs = getMatchesWithContext origString excelMatch
        matchLocs = map (exRefToASRef sheetid) (snd exRefs)
        context = map (lookupString SQL values) matchLocs
        st = ["dataset"++(show i) | i<-[0..((L.length matchLocs)-1)]]
        newExp = replaceMatches exRefs (\el -> (L.!!) st (MB.fromJust (L.findIndex (el==) (snd exRefs)))) origString
        contextStmt = "setGlobals("++(show context) ++")\n"
        evalStmt = "result = pprintSql(db(\'" ++ newExp ++ "\'))"

insertValues sheetid values (Expression origString lang) = evalString
    where
        exRefToStringEval = (lookupString lang values) . (exRefToASRef sheetid) -- ExRef -> String. (Takes in ExRef, returns the ASValue corresponding to it, as a string.)
        evalString = replaceMatches (getMatchesWithContext origString excelMatch) exRefToStringEval origString

-----------------------------------------------------------------------------------------------------------------------
-- | File management

writeExecFile :: ASLanguage -> String -> IO ()
writeExecFile lang contents = getRunFile lang >>= \f -> writeFile (f :: System.IO.FilePath) contents

writeReplFile :: ASLanguage -> String -> IO ()
writeReplFile lang contents = getRunReplFile lang >>= \f -> writeFile (f :: System.IO.FilePath) contents

writeReplRecord :: ASLanguage -> String -> IO ()
writeReplRecord lang contents = getReplRecordFile lang >>= \f -> writeFile (f :: System.IO.FilePath) contents

clearReplRecord :: ASLanguage -> IO ()
clearReplRecord lang = getReplRecordFile lang >>= \f -> writeFile (f :: System.IO.FilePath)  ""

getTemplate :: ASLanguage -> IO String
getTemplate lang = do
    path <- getEvalPath
    P.readFile $ path ++ file
    where
        file = case lang of 
            R       -> "r/template.r"
            Python  -> "py/template.py"
            OCaml   -> "ocaml/template.ml"
            SQL     -> "sql/template.py"
            CPP     -> "cpp/template.cpp"
            Java    -> "java/Template.java"
            Excel   -> "excel/template.py"

getTemplateRepl :: ASLanguage -> IO String
getTemplateRepl lang = do
    path <- getEvalPath
    P.readFile $ path ++ file
    where
        file = case lang of 
            R       -> "r/template_repl.r"
            Python  -> "py/template_repl.py"
            OCaml   -> "ocaml/template_repl.ml"
            SQL     -> "sql/template_repl.py"
            CPP     -> "cpp/template_repl.cpp"
            Java    -> "java/Template_repl.java"

getRunFile :: ASLanguage -> IO String
getRunFile lang = do
    path <- getEvalPath 
    return $ path ++ case lang of 
        R       -> "r/run.r"
        Python  -> "py/run.py"
        OCaml   -> "ocaml/run.ml"
        SQL     -> "sql/run.py"

getRunReplFile :: ASLanguage -> IO String
getRunReplFile lang = do
    path <- getEvalPath 
    return $ path ++ case lang of 
        R       -> "r/run_repl.r"
        Python  -> "py/run_repl.py"
        OCaml   -> "ocaml/run_repl.ml"
        SQL     -> "sql/run_repl.py"

getReplRecord :: ASLanguage -> IO String
getReplRecord lang = do
    path <- getEvalPath
    S.readFile $ path ++ file
    where
        file = case lang of 
            Python  -> "py/repl_record.py"

getReplRecordFile :: ASLanguage -> IO String
getReplRecordFile lang = do
    path <- getEvalPath 
    return $ path ++ file
    where
        file = case lang of 
            Python  -> "py/repl_record.py"