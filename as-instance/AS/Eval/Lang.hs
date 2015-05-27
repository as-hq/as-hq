module AS.Eval.Lang where

import AS.Types
import AS.Config.Paths
import AS.TypesHelper
import AS.Parsing.Out
import AS.Parsing.Common
import AS.DB

import Import
import Control.Applicative                                   
import System.Directory(getCurrentDirectory)

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Prelude as P
import Prelude ((!!), read)

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
			

getTemplate :: ASLanguage -> Handler String
getTemplate lang = Import.readFile $ getEvalPath ++ file
	where
		file = case lang of 
			R 		-> "r/template.r"
			Python 	-> "py/template.py"
			OCaml 	-> "ocaml/template.ml"

getRunFile :: ASLanguage -> String
getRunFile lang = getEvalPath ++ case lang of 
	R 		-> "r/temp.r"
	Python 	-> "py/temp.py"
	OCaml 	-> "ocaml/temp.ml"


getRunnerCmd :: ASLanguage -> String
getRunnerCmd lang = case lang of 
	R 		-> "Rscript "
	Python 	-> "python "
	OCaml 	-> "ocamlfind ocamlc -linkpkg -package extlib "

getRunnerArgs :: ASLanguage -> [String]
getRunnerArgs lang = case lang of 
	OCaml -> ["-o " ++ path ++ "test"]
		where 
			path = getEvalPath ++ "ocaml/"
	otherwise -> [] -- in case we ever use more args

layoutCodeFile :: ASLanguage -> (String, String, String) -> String
layoutCodeFile lang (imports, template, cmd) = case lang of 
	Python 	-> replaceSubstrings importedTemplate [("#CMD#", tabbedCmd)]
		where
			importedTemplate = intercalate "\n" [imports, template]
			tabbedCmd = replaceSubstrings cmd [("\n", "\n\t")]
	R 		-> replaceSubstrings importedTemplate [("#CMD#", cmd)]
		where
			importedTemplate = intercalate "\n" [imports, template]
	otherwise -> intercalate "\n" [imports, template, cmd]

formatRunArgs :: ASLanguage -> String -> String -> [String] -> String
formatRunArgs lang cmd filename args = case lang of 
	otherwise -> cmd ++ filename ++ " " ++ (intercalate " " args)

addCompileCmd :: ASLanguage -> String -> String
addCompileCmd lang cmd = case lang of 
	OCaml -> cmd ++ "; " ++ path ++ "test"
		where
			path = getEvalPath ++ "ocaml/"
	otherwise -> cmd


interpolateFile :: ASLanguage -> String -> Handler String
interpolateFile lang execCmd = do
	functions <- getFuncs lang
	let (cleanCmd, imports) = replaceAliases execCmd functions
	$(logInfo) $ "EVAL REPLACED XP: " ++ (fromString . show $ cleanCmd)
	let editedCmd = insertPrintCmd lang $ splitLastCmd lang cleanCmd
	let importCmds = unlines . map (importFile lang) $ imports
	template <- getTemplate lang
	$(logInfo) $ "EVAL EDITED XP: " ++ (fromString $ show editedCmd)
	$(logInfo) $ "EVAL USING TEMPLATE: " ++ (fromString $ show template)
	return $ layoutCodeFile lang (importCmds, template, editedCmd)

interpolate :: Map ASLocation ASValue -> ASExpression -> String
interpolate values xp = execCmd
	where
		execCmd			= replaceSubstrings expandedLists matches
		expandedLists 	= excelRangesToLists lang $ expression xp
		matches 		= map (\(a, b) -> (toExcel a, showFilteredValue lang a b)) (M.toList values)
		lang 			= language xp

insertPrintCmd :: ASLanguage -> (String, String) -> String
insertPrintCmd lang (s, lst) = s ++ process lst 
	where
		process l 	= case lang of 
			R 		-> l
			Python 	-> "print(repr(" ++ l ++ "))"
			OCaml 	-> "print_string(Std.dump(" ++ l ++ "))"

splitLastCmd :: ASLanguage -> String -> (String, String)
splitLastCmd lang cmd = 
	let 
		lines 			= T.splitOn (pack "\n") (pack cmd)
		lastStmts 		= T.splitOn (pack $ getInlineDelim lang) (P.last lines)
		initLines 		= intercalate "\n" $ map unpack (P.init lines)
		initLastLines 	= intercalate "\n" $ map unpack (P.init lastStmts)
		initStmts 		= initLines ++ "\n" ++ initLastLines ++ (getBlockDelim lang) ++ "\n"
		lastStmt 		= unpack $ P.last lastStmts
	in (initStmts, lastStmt)