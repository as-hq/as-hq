module AS.Kernels.LanguageUtils where

import AS.Types.Core

import Prelude
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T
import qualified Data.List as L

import AS.Config.Paths
import AS.Parsing.Out
import AS.Parsing.Substitutions
import AS.Parsing.Common

import Control.Exception (SomeException, catch)
import Control.Applicative hiding ((<|>))
import System.IO.Strict as S
import System.IO

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Maybe as MB
import Data.List.Split as SP
import qualified Data.Map as M
import qualified Prelude as P

import AS.Util

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
        (startLines, endLine) = splitLastLine lang str
        foo = tryPrintingLastRepl lang trimmed
    case (tryPrintingLastRepl lang trimmed) of
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
        then Right (startLines, printCmd lang endLine)
        else (Left ())

tryPrintingLastRepl :: ASLanguage -> String -> Either () (String, String)
tryPrintingLastRepl lang str =
    let (startLines, endLine) = splitLastLine lang str
    in if (isPrintable lang endLine)
        then case startLines of 
                    "" -> Right (str, printCmd lang endLine)
                    otherwise -> Right (startLines, printCmd lang endLine)
        else (Left ())

-- | WRONG! Not sure what this is supposed to mean... at any rate I'm convinced the logic here
-- is extremely wrong and incomplete right now. 10/29. 
isPrintable :: ASLanguage -> String -> Bool
isPrintable lang s = not (containsAny ["\t"] s) -- assignOp lang, returnOp lang, importOp lang]

printCmd :: ASLanguage -> String -> String
printCmd lang str = case (tryParse (replacePrintStmt lang) str) of
    (Left _) -> addPrintCmd lang str
    (Right printed) -> printed

replacePrintStmt :: ASLanguage -> Parser String
replacePrintStmt lang = case lang of
    Python -> do
        spaces >> string "print" >> spaces
        opener <- option ' ' $ try (char '(')
        let closer = if (opener == '(')
            then char ')' >> spaces
            else spaces
        arg <- manyTill anyChar (try (closer >> eof))
        return $ "result = " ++ arg
    R -> manyTill anyChar (try eof) -- TODO


-- | Takes in a string and returns (remaining lines, last line). Works by reversing the string, 
-- getting its first line, and reversing the results. 
splitLastLine :: ASLanguage -> String -> (String, String)
splitLastLine lang str = case (tryParse (firstLineAndRest lang) (reverse str)) of
    (Right (reversedLastLine, reversedRest))  -> (reverse reversedRest, reverse reversedLastLine)
    (Left _)        -> (emptyExpression, str) -- only one line, which becomes the 'last' line

-- | Parser that splits multiline string into (firstLine, remaining lines)
firstLineAndRest :: ASLanguage -> Parser (String, String)
firstLineAndRest lang = do
    firstLine <- manyTill anyChar $ string "\n" <|> string (lineDelim lang)
    remainingLines <- manyTill anyChar (try eof)
    return (firstLine, remainingLines)

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

trimWhitespace :: ASLanguage -> String -> String  -- TODO use the language to get block delimiters
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


-- | Helper function for interpolate. Takes in a IndValMap and a reference and returns
-- what that'd map to, as a string. as a string.
lookUpRef :: ASLanguage -> IndValMap -> ASReference -> String
lookUpRef lang valuesMap ref = case ref of
  IndexRef ind -> showValue lang $ valuesMap M.! ind
  RangeRef (Range sh ((a,b),(c,d))) -> if (c == a)
    then modifiedLists lang (toListStr lang [ ((showValue lang) (valuesMap M.! (Index sh (a,row)))) | row<-[b..d]])
    else modifiedLists lang (toListStr lang [modifiedLists lang (toListStr lang ([(showValue lang) (valuesMap M.! (Index sh (col,row)))| col <-[a..c]]))| row<-[b..d]])

-- | Replaces all the Excel references in an expression with the valuesMap corresponding to them.
-- TODO clean up SQL mess

insertValues :: ASSheetId -> IndValMap -> ASExpression -> String
insertValues sheetid valuesMap xp@(Expression origString lang) = case lang of
  SQL -> contextStmt ++ evalStmt
    where
        exRefs = getUnquotedMatchesWithContext xp refMatch
        matchRefs = map (exRefToASRef sheetid) (snd exRefs)
        context = map (lookUpRef SQL valuesMap) matchRefs
        st = ["dataset"++(show i) | i<-[0..((L.length matchRefs)-1)]]
        newExp = replaceMatches exRefs (\el -> (L.!!) st (MB.fromJust (L.findIndex (el==) (snd exRefs)))) origString
        contextStmt = "setGlobals("++(show context) ++")\n"
        evalStmt = "result = pprintSql(db(\'" ++ newExp ++ "\'))"
  otherLang -> evalString
    where
        exRefs = getUnquotedMatchesWithContext xp refMatch
        exRefToStringEval = (lookUpRef lang valuesMap) . (exRefToASRef sheetid) -- ExRef -> String. (Takes in ExRef, returns the ASValue corresponding to it, as a string.)
        evalString = replaceMatches exRefs exRefToStringEval origString

-----------------------------------------------------------------------------------------------------------------------
-- | File management

writeExecFile :: ASLanguage -> String -> IO ()
writeExecFile lang contents = catch (getRunFile lang >>= \f -> writeFile (f :: System.IO.FilePath) contents)
                                    (\e -> putStrLn ("Error opening exec file: " ++ (show (e :: SomeException))))

-- no catch statements here because we're basically giving up on the Repl for now (11/6 Alex)
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
