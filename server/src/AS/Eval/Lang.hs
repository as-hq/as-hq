module AS.Eval.Lang where

import AS.Types.Core
import AS.Config.Paths
import AS.Parsing.Out
import AS.Parsing.Common

import Prelude
import Control.Applicative     
import Control.Monad.IO.Class (liftIO)                              
import System.Directory(getCurrentDirectory)
import System.IO.Strict as S   

import qualified Data.Text as T
import qualified Data.List as L
import Data.List.Split as SP
import qualified Data.Map as M
import qualified Prelude as P

pid :: String
pid = "1730"

importFile :: ASLanguage -> (String, String, String) -> String
importFile lang (name, cmd, loc) = 
	let
		dlm = getBlockDelim lang
	in case lang of 
		R -> case cmd of 
			"" 			-> "library(" ++ name ++ ", lib.loc=\"" ++ loc ++ "\")" ++ dlm ++ "\n"
			otherwise 	-> cmd
		Python -> case cmd of 
			"execfile" 	-> "execfile(\"" ++ loc ++ name ++ ".py" ++ "\")" ++ dlm ++ "\n" 
			otherwise 	-> cmd ++ "\n"
		OCaml -> "load " ++ name ++ dlm ++ "\n" ++ "open " ++ name ++ dlm ++ "\n"
		SQL -> case cmd of 
			"execfile" 	-> "execfile(\"" ++ loc ++ name ++ ".py" ++ "\")" ++ dlm ++ "\n" 
			otherwise 	-> cmd ++ "\n"
			

getTemplate :: ASLanguage -> IO String
getTemplate lang = do
	path <- getEvalPath
	P.readFile $ path ++ file
	where
		file = case lang of 
			R 		-> "r/template.r"
			Python 	-> "py/template.py"
			OCaml 	-> "ocaml/template.ml"
			SQL		-> "sql/template.py"
			CPP 	-> "cpp/template.cpp"
			Java	-> "java/Template.java"
			Excel 	-> "excel/template.py"

getReplTemplate :: ASLanguage -> IO String
getReplTemplate lang = do
	path <- getEvalPath
	P.readFile $ path ++ file
	where
		file = case lang of 
			R 		-> "r/template_repl.r"
			Python 	-> "py/template_repl.py"
			OCaml 	-> "ocaml/template_repl.ml"
			SQL		-> "sql/template_repl.py"
			CPP 	-> "cpp/template_repl.cpp"
			Java	-> "java/Template_repl.java"
			Excel 	-> "excel/template_repl.py"

getRunFile :: ASLanguage -> IO String
getRunFile lang = do
	path <- getEvalPath 
	return $ path ++ case lang of 
		R 		-> "r/run.r"
		Python 	-> "py/run.py"
		OCaml 	-> "ocaml/run.ml"
		SQL 	-> "sql/run.py"
		CPP 	-> "cpp/run.cpp"
		Java 	-> "java/Run.java"
		Excel 	-> "excel/run.py"

getRunReplFile :: ASLanguage -> IO String
getRunReplFile lang = do
	path <- getEvalPath 
	return $ path ++ case lang of 
		R 		-> "r/temp_repl.r"
		Python 	-> "py/temp_repl.py"
		OCaml 	-> "ocaml/temp_repl.ml"
		SQL 	-> "sql/temp_repl.py"

getReplRecord :: ASLanguage -> IO String
getReplRecord lang = do
	path <- getEvalPath
	S.readFile $ path ++ file
	where
		file = case lang of 
			Python 	-> "py/repl_record.py"

getReplRecordFile :: ASLanguage -> IO String
getReplRecordFile lang = do
	path <- getEvalPath 
	return $ path ++ file
	where
		file = case lang of 
			Python 	-> "py/repl_record.py"

getRunnerCmd :: ASLanguage -> String
getRunnerCmd lang = case lang of 
	R 		-> "Rscript "
	Python 	-> "python -u " -- "pyrasite --output 'localterm' " ++ pid ++ " "
	OCaml 	-> "ocamlfind ocamlc -linkpkg -thread -package extlib -package core "
	SQL  	-> "python "
	CPP 	-> "g++ -std=c++11 "
	Java 	-> "javac "
	Excel 	-> "python "

getRunnerCmdRepl :: ASLanguage -> String
getRunnerCmdRepl lang = case lang of 
	Python 	-> "python "

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

layoutCodeFile :: ASLanguage -> (String, String, String) -> String
layoutCodeFile lang (imports, template, cmd) = case lang of 
	Python 	-> replaceSubstrings importedTemplate [("#CMD#", tabbedCmd)]
		where
			importedTemplate = L.intercalate "\n" [imports, template]
			tabbedCmd = replaceSubstrings cmd [("\n", "\n\t")]
	R 		-> replaceSubstrings importedTemplate [("#CMD#", cmd)]
		where
			importedTemplate = L.intercalate "\n" [imports, template]
	Excel 	-> replaceSubstrings importedTemplate [("#CMD#", tabbedCmd)]
		where
			importedTemplate = L.intercalate "\n" [imports, template]
			tabbedCmd = replaceSubstrings cmd [("\n", "\n\t")]
	SQL 	-> replaceSubstrings importedTemplate [("#CMD#", tabbedCmd)]
		where
			importedTemplate = L.intercalate "\n" [imports, template]
			tabbedCmd = replaceSubstrings cmd [("\n", "\n\t")]
	otherwise -> L.intercalate "\n" [imports, template, cmd]



formatRunArgs :: ASLanguage -> String -> String -> [String] -> String
formatRunArgs lang cmd filename args = case lang of 
	otherwise -> cmd ++ filename ++ " " ++ (L.intercalate " " args)

addCompileCmd :: ASLanguage -> String -> IO String
addCompileCmd lang cmd = do
	evalPath <- getEvalPath
	return $ case lang of 
		OCaml -> cmd ++ "; " ++ path ++ "test"
			where
				path = evalPath ++ "ocaml/"
		Java -> "cd "++ path ++ " && "++cmd ++ " && java Temp"
			where
				path = evalPath ++ "java/"
		otherwise -> cmd

--addCompileCmdRepl :: ASLanguage -> String -> String
--addCompileCmdRepl lang cmd = case lang of 
--	Python -> cmd ++ "; pyrasite --output 'localterm' "++pid++ " " ++(getRunReplFile lang)

--interpolateFile :: ASLanguage -> String -> IO String
--interpolateFile lang execCmd = do
--	functions <- getFuncs lang
--	let (cleanCmd, imports) = replaceAliases execCmd functions
--	$(logInfo) $ "EVAL REPLACED XP: " ++ (fromString . show $ cleanCmd)
--	let editedCmd = insertPrintCmd lang $ splitLastCmd lang cleanCmd
--	let importCmds = unlines . map (importFile lang) $ imports
--	template <- getTemplate lang

--	-- $(logInfo) $ "EVAL EDITED XP: " ++ (fromString $ show editedCmd)
--	-- $(logInfo) $ "EVAL USING TEMPLATE: " ++ (fromString $ show template)
--	return $ layoutCodeFile lang (importCmds, template, editedCmd)

interpolateFile :: ASLanguage -> String -> IO String
interpolateFile lang execCmd = do
	let editedCmd = insertPrintCmd lang $ splitLastCmd lang execCmd
	template <- getTemplate lang
	return $ layoutCodeFile lang ("", template, editedCmd)

interpolateFileRepl :: ASLanguage -> String -> IO String
interpolateFileRepl lang execCmd = do
	template <- getReplTemplate lang
	return $ layoutCodeFile lang ("", template, execCmd)

-- Helper function for interpolate
-- ^^ THEN WHY THE FUCK ARE YOU CALLING IT FROM CORE.HS 
lookupString :: ASLanguage -> M.Map ASLocation ASValue -> ASLocation -> String
lookupString lang mp loc = case loc of
	IndexLoc (Index sh (a,b)) -> (showFilteredValue lang) (mp M.! loc)
	RangeLoc (Range sh ((a,b),(c,d))) -> 
		if (c==a)
			then
				modifiedLists lang (toListStr lang [ ((showFilteredValue lang) (mp M.! (IndexLoc $ Index sh (a,row)))) | row<-[b..d]])
			else 
				modifiedLists lang (toListStr lang [modifiedLists lang (toListStr lang ([(showFilteredValue lang) (mp M.! (IndexLoc $ Index sh (col,row)))| col <-[a..c]]))| row<-[b..d]])

interpolate :: ASSheetId -> M.Map ASLocation ASValue -> ASExpression -> String
interpolate sheetid values xp = evalString
	where
		origString = expression xp
		lang = language xp 
		exLocToStringEval = (lookupString lang values) . (exLocToASLocation sheetid) -- ExLoc -> String
		evalString = replaceMatches (getMatchesWithContext origString excelMatch) exLocToStringEval origString


insertPrintCmd :: ASLanguage -> (String, String) -> String
insertPrintCmd lang (s, lst) = s ++ (printCmd lang lst) 

printCmd :: ASLanguage -> String -> String
printCmd lang str = case lang of 
	R 		-> str
	Python 	-> "result = " ++ str 
	OCaml 	-> "print_string(Std.dump(" ++ str ++ "))"
	SQL 	-> "result = pprintSql(db(\'" ++ str ++ "\'))" -- hardcoded db() function usage for demos
	CPP 	-> "int main() { std::cout << (" ++ str ++ "); }" 
	Java 	-> "public static void main(String[] args) throws Exception{Object x = " ++ str ++ "; System.out.println(pprint(x));}}"
	Excel 	-> "result = " ++ str

splitLastCmd :: ASLanguage -> String -> (String, String)
splitLastCmd lang cmd = 
	let 
		lines 			= T.splitOn (T.pack "\n") (T.pack cmd)
		lastStmts 		= T.splitOn (T.pack $ getInlineDelim lang) (P.last lines)
		initLines 		= L.intercalate "\n" $ map T.unpack (P.init lines)
		initLastLines 	= L.intercalate "\n" $ map T.unpack (P.init lastStmts)
		initStmts 		= initLines ++ "\n" ++ initLastLines ++ (getBlockDelim lang) ++ "\n"
		lastStmt 		= T.unpack $ P.last lastStmts
	in (initStmts, lastStmt)