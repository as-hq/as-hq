module AS.Kernels.LanguageUtils where


import Prelude()
import AS.Prelude

import AS.Config.Settings as S

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T
import qualified Data.List as L

import AS.Types.Cell
import AS.Types.Excel
import AS.Types.Eval
import AS.Types.Errors

import AS.Logging

import AS.Parsing.Show
import AS.Parsing.Substitutions
import AS.Parsing.Common
import AS.Parsing.Excel
import qualified AS.LanguageDefs as LD

import Control.Exception (SomeException, catch)
import Control.Applicative hiding ((<|>))
import Control.Lens
import System.IO.Strict as S
import System.IO

import qualified Data.Text as T
import qualified Data.List as L
import Data.List.Split as SP
import qualified Data.Map as M
import qualified Prelude as P

import AS.Util

import AS.Parsing.Common as C

import qualified AS.DB.Eval as DV
import Database.Redis (Connection)


-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-----------------------------------------------------------------------------------------------------------------------
-- | Exposed functions

formatSqlCode :: EvalCode -> IO EvalCode
formatSqlCode code = do
  template <- getTemplate SQL
  return $ replaceSubstrings template [("#CODE#", code)]

-- DEPRECATED (anand 1/19/16)
--formatCode :: String -> ASLanguage -> String -> EitherTExec String
--formatCode header lang str = do
--  let trimmed = trimWhitespace lang str
--      onSuccess = (wrapCode header lang False) . recombineLines
--      onFailure _ = return ExpressionNotEvaluable
--  case lang of
--    SQL     -> lift $ wrapCode header SQL False trimmed
--    otherwise   -> case (tryPrintingLast lang trimmed) of
--      (Left _)    -> left ExpressionNotEvaluable
--      (Right result)  -> lift $ wrapCode header lang False $ recombineLines result

-- NOT MAINTAINED since we're not using REPL (anand 1/19/16)
-- returns (repl record code, repl eval code)
--formatCodeRepl :: String -> ASLanguage -> String -> IO (String, String)
--formatCodeRepl header lang str = do
--  let trimmed = trimWhitespace lang str
--      (startLines, endLine) = splitLastLine lang str
--  case (tryPrintingLastRepl lang trimmed) of
--    (Left _) -> return $ (trimmed, "") -- nothing to print, so nothing to evaluate
--    (Right (recordXp, printedLine)) -> do
--      evalXp <- wrapCode header lang True $ recombineLines (recordXp, printedLine)
--      return (recordXp, evalXp)

-----------------------------------------------------------------------------------------------------------------------
-- | Helpers

--tryPrintingLast :: ASLanguage -> String -> Either () (String, String)
--tryPrintingLast lang str =
--  let (startLines, endLine) = splitLastLine lang str
--  in if (isPrintable lang endLine)
--    then Right (startLines, printCmd lang endLine)
--    else (Left ())

--tryPrintingLastRepl :: ASLanguage -> String -> Either () (String, String)
--tryPrintingLastRepl lang str =
--  let (startLines, endLine) = splitLastLine lang str
--  in if (isPrintable lang endLine)
--    then case startLines of 
--          "" -> Right (str, printCmd lang endLine)
--          otherwise -> Right (startLines, printCmd lang endLine)
--    else (Left ())

---- | WRONG! Not sure what this is supposed to mean... at any rate I'm convinced the logic here
---- is extremely wrong and incomplete right now. (Alex 10/29) 
--isPrintable :: ASLanguage -> String -> Bool
--isPrintable lang s = not (containsAny ["\t", "import"] s) -- assignOp lang, returnOp lang, importOp lang]

--printCmd :: ASLanguage -> String -> String
--printCmd lang str = case (tryParse (replacePrintStmt lang) str) of
--  (Left _) -> addPrintCmd lang str
--  (Right printed) -> printed

--replacePrintStmt :: ASLanguage -> Parser String
--replacePrintStmt lang = case lang of
--  Python -> do
--    spaces >> string "print" >> spaces
--    opener <- option ' ' $ try (char '(')
--    let closer = if (opener == '(')
--          then char ')' >> spaces
--          else spaces
--    arg <- manyTill anyChar (try (closer >> eof))
--    return $ "result = " ++ arg
--  R -> manyTill anyChar (try eof) -- TODO


---- | Takes in a string and returns (remaining lines, last line). Works by reversing the string, 
---- getting its first line, and reversing the results. 
--splitLastLine :: ASLanguage -> String -> (String, String)
--splitLastLine lang str = case (tryParse (firstLineAndRest lang) (reverse str)) of
--  (Right (reversedLastLine, reversedRest))  -> (reverse reversedRest, reverse reversedLastLine)
--  (Left _)    -> ("", str) -- only one line, which becomes the 'last' line

---- | Parser that splits multiline string into (firstLine, remaining lines)
--firstLineAndRest :: ASLanguage -> Parser (String, String)
--firstLineAndRest lang = do
--    firstLine <- manyTill anyChar $ string "\n" <|> string (LD.inlineDelimiter lang)
--    remainingLines <- manyTill anyChar (try eof)
--    return (firstLine, remainingLines)

--lastLine :: ASLanguage ->  Parser String
--lastLine lang = do
--    string "\n" <|> string (LD.inlineDelimiter lang)
--    manyTill anyChar eof

--addPrintCmd :: ASLanguage -> String -> String
--addPrintCmd lang str = case lang of
--  R     -> str
--  Python  -> "result = " ++ str
--  OCaml   -> "print_string(Std.dump(" ++ str ++ "))"
--  SQL   -> "result = serialize(db(\'" ++ str ++ "\'))" -- hardcoded db() function usage for demos

--recombineLines :: (String, String) -> String
--recombineLines ("", endLine) = endLine
--recombineLines (startLines, "") = startLines
--recombineLines (startLines, endLine) = startLines ++ "\n" ++ endLine

trimWhitespace :: ASLanguage -> String -> String  -- TODO use the language to get block delimiters
trimWhitespace lang = L.dropWhileEnd isWhitespace . L.dropWhile isWhitespace
  where isWhitespace c = (c == ' ') || (c == '\n') || (c == '\t') || (c == ';')

-- TODO fix
--trim :: ASLanguage -> Parser String
--trim lang = do
--  try $ skipMany1 (whitespace lang)
--  manyTill anyChar (try eof)

--whitespace :: ASLanguage -> Parser ()
--whitespace SQL =
--    (spaces >> return ())
--  <|> (char '\n' >> return ())
--  <|> (char '\t' >> return ())
--whitespace lang =
--    (spaces >> return ())
--  <|> (char '\n' >> return ())
--  <|> (char '\t' >> return ())
--  <|> (string (lineDelim lang) >> return ())

--formatCodeForInsert :: ASLanguage -> String -> String
--formatCodeForInsert lang str = case lang of
--  Python -> replaceSubstrings str [("\n", "\n\t")]
--  SQL -> replaceSubstrings str [("\n", "\n\t")]
--  otherwise -> str

--wrapCode :: String -> ASLanguage -> Bool -> String -> IO String
--wrapCode header lang isRepl str = do 
--  let insertedCode = formatCodeForInsert lang str
--      insertedHeader = formatCodeForInsert lang header
--  template <- if isRepl
--    then getTemplateRepl lang
--    else getTemplate lang
--  return $ replaceSubstrings template [("#CODE#", insertedCode), ("#HEADER#", insertedHeader)]

-----------------------------------------------------------------------------------------------------------------------
-- | Language-specific string modifiers

-- NOT MAINTAINED, due to OCaml not currently being supported (anand 1/19/16)

--getRunnerCmd :: ASLanguage -> String
--getRunnerCmd lang = case lang of
--  R     -> "Rscript "
--  OCaml   -> "ocamlfind ocamlc -linkpkg -thread -package extlib -package core "

--getRunnerArgs :: ASLanguage -> IO [String]
--getRunnerArgs lang = do
--  evalPath <- getEvalPath
--  return $ case lang of
--    OCaml -> ["-o " ++ path ++ "test"]
--      where
--        path = evalPath ++ "ocaml/"
--    CPP -> ["-o " ++ path ++ "testCPP && " ++ path ++ "testCPP"]
--      where
--        path = evalPath ++ "cpp/"
--    otherwise -> [] -- in case we ever use more args


--formatRunArgs :: ASLanguage -> String -> String -> [String] -> String
--formatRunArgs lang cmd filename args = case lang of
--  otherwise -> cmd ++ filename ++ " " ++ (L.intercalate " " args)

--addCompileCmd :: ASLanguage -> String -> IO String
--addCompileCmd OCaml cmd = do
--  evalPath <- getEvalPath
--  let path = evalPath ++ "ocaml/"
--  return $ cmd ++ "; " ++ path ++ "test"

-----------------------------------------------------------------------------------------------------------------------
-- | Value interpolation


lookUpRef :: Connection -> ASLanguage -> EvalContext -> ASReference -> IO String
lookUpRef conn lang context ref = showValue lang <$> DV.referenceToCompositeValue conn context ref

-- | Replaces all the Excel references in an expression with the valuesMap corresponding to them.
-- TODO clean up SQL mess
-- #mustrefactor IO String should be EitherTExec string
insertValues :: Connection -> ASSheetId -> EvalContext -> ASExpression -> IO EvalCode
insertValues conn sheetid ctx xp = do 
  let exRefToStringEval = lookUpRef conn (xp^.language) ctx . exRefToASRef sheetid -- ExRef -> String. (Takes in ExRef, returns the ASValue corresponding to it, as a string.)
  view expression <$> replaceRefsIO exRefToStringEval xp

-- | We evaluate SQL expressions by converting them to Python code, and substituting it into a template
-- file that imports pysql and defines helper functions for SQL.  
-- 
-- Note: this can probably be significantly optimized. 
sqlToPythonCode :: Connection -> ASSheetId -> EvalContext -> ASExpression -> IO EvalCode
sqlToPythonCode conn sheetid ctx xp = do 
  let exRefs = getExcelReferences xp
      matchedRefs = map (exRefToASRef sheetid) exRefs -- ASReferences found inside xp
  context <- mapM (lookUpRef conn SQL ctx) matchedRefs
  let st = ["dataset"++(show i) | i<-[0..((L.length matchedRefs)-1)]]
      newExp = view expression $ replaceRefs (\el -> (L.!!) st ($fromJust (L.findIndex (el==) exRefs))) xp
      contextStmt = "setGlobals(" ++ show context ++ ")\n"
      evalStmt = "db(\'" ++ newExp ++ "\')"
  return $ contextStmt ++ evalStmt

-----------------------------------------------------------------------------------------------------------------------
-- | File management

-- DEPRECATED, we are no longer using file-based evaluation (anand 1/19/16)

--writeExecFile :: ASLanguage -> String -> IO ()
--writeExecFile lang contents = catch (getRunFile lang >>= \f -> writeFile (f :: System.IO.FilePath) contents)
--                  (\e -> putStrLn ("Error opening exec file: " ++ (show (e :: SomeException))))

---- no catch statements here because we're basically giving up on the Repl for now (11/6 Alex)
--writeReplFile :: ASLanguage -> String -> IO ()
--writeReplFile lang contents = getRunReplFile lang >>= \f -> writeFile (f :: System.IO.FilePath) contents

--writeReplRecord :: ASLanguage -> String -> IO ()
--writeReplRecord lang contents = getReplRecordFile lang >>= \f -> writeFile (f :: System.IO.FilePath) contents

--writeHeaderRecord :: ASLanguage -> String -> IO ()
--writeHeaderRecord lang contents = getHeaderRecordFile lang >>= \f -> writeFile (f :: System.IO.FilePath) contents

--clearReplRecord :: ASLanguage -> IO ()
--clearReplRecord lang = getReplRecordFile lang >>= \f -> writeFile (f :: System.IO.FilePath)  ""

getTemplate :: ASLanguage -> IO String
getTemplate lang = do
  P.readFile $ S.eval_dir ++ file
  where
    file = case lang of
      R     -> "r/template.r"
      Python  -> "py/template.py"
      OCaml   -> "ocaml/template.ml"
      SQL   -> "sql/template.py"
      CPP   -> "cpp/template.cpp"
      Java  -> "java/Template.java"
      Excel   -> "excel/template.py"

-- DEPRECATED, we are not currently supporting the REPL (anand 1/19/16)
--getTemplateRepl :: ASLanguage -> IO String
--getTemplateRepl lang = do
--  path <- getEvalPath
--  P.readFile $ path ++ file
--  where
--    file = case lang of
--      R     -> "r/template_repl.r"
--      Python  -> "py/template_repl.py"
--      OCaml   -> "ocaml/template_repl.ml"
--      SQL   -> "sql/template_repl.py"
--      CPP   -> "cpp/template_repl.cpp"
--      Java  -> "java/Template_repl.java"

-- DEPRECATED, we are not currently supporting OCaml (anand 1/19/16)
--getRunFile :: ASLanguage -> IO String
--getRunFile lang = do
--  path <- getEvalPath
--  return $ path ++ case lang of
--    R     -> "r/run.r"
--    Python  -> "py/run.py"
--    OCaml   -> "ocaml/run.ml"
--    SQL   -> "sql/run.py"

-- DEPRECATED, we are not currently supporting the REPL (anand 1/19/16)
--getRunReplFile :: ASLanguage -> IO String
--getRunReplFile lang = do
--  path <- getEvalPath
--  return $ path ++ case lang of
--    R     -> "r/run_repl.r"
--    Python  -> "py/run_repl.py"
--    OCaml   -> "ocaml/run_repl.ml"
--    SQL   -> "sql/run_repl.py"

-- DEPRECATED, we are not currently supporting the REPL (anand 1/19/16)
--getReplRecord :: ASLanguage -> IO String
--getReplRecord lang = do
--  path <- getEvalPath
--  S.readFile $ path ++ file
--  where
--    file = case lang of
--      Python  -> "py/repl_record.py"

-- DEPRECATED, we are not currently supporting the REPL (anand 1/19/16)
--getReplRecordFile :: ASLanguage -> IO String
--getReplRecordFile lang = do
--  path <- getEvalPath
--  return $ path ++ file
--  where
--    file = case lang of
--      Python  -> "py/repl_record.py"

-- DEPRECATED, we are no longer using file-based evaluation (anand 1/19/16)
--getHeaderRecordFile :: ASLanguage -> IO String
--getHeaderRecordFile lang = do
--  path <- getEvalPath
--  return $ path ++ file
--  where
--    file = case lang of
--      Python  -> "py/header_record.py"
