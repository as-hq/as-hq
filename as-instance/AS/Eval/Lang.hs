module AS.Eval.Lang where

import AS.Types
import AS.Config.Paths
import AS.TypesHelper
import Import
import Control.Applicative                                   
import System.Directory(getCurrentDirectory)

importFile :: ASLanguage -> (String, String, String) -> String
importFile lang (name, cmd, loc) = 
	let
		dlm = getLineDelim lang
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

formatRunArgs :: ASLanguage -> String -> String -> [String] -> String
formatRunArgs lang cmd filename args = case lang of 
	otherwise -> cmd ++ filename ++ " " ++ (intercalate " " args)

addCompileCmd :: ASLanguage -> String -> String
addCompileCmd lang cmd = case lang of 
	OCaml -> cmd ++ "; " ++ path ++ "test"
		where
			path = getEvalPath ++ "ocaml/"
	otherwise -> cmd

toListStr :: ASLanguage -> [String] -> String
toListStr lang lst  = end ++ (intercalate delim lst) ++ start
  where
    (end, delim, start) = case lang of 
      R     -> ("c(", ",", ")")
      Python-> ("[", ",", "]")
      OCaml -> ("[", ";", "]")

getLineDelim :: ASLanguage -> String
getLineDelim lang = case lang of 
  R     -> ""
  Python-> ""
  OCaml -> ";;"

getInlineDelim :: ASLanguage -> String
getInlineDelim lang = case lang of 
  R     -> ";"
  Python-> ";"
  OCaml -> ";;"

jsonDeserialize :: ASLanguage -> String -> String -> String
jsonDeserialize lang objType jsonRep = 
	let 
		dlm = getLineDelim lang
	in case lang of 
	  R       -> objType ++ "$(" ++ jsonRep ++ ")" ++ dlm
	  Python  -> objType ++ ".deserialize(" ++ jsonRep ++ ")" ++ dlm
	  OCaml   -> "Serialization# " ++ objType ++ " " ++ jsonRep ++ dlm-- TODO ocaml serialization class

showValue :: ASLanguage -> ASValue -> String
showValue lang v = case v of
  ValueImage path 	-> "PLOT"--ADDED, open file here?
  ValueNaN () 		-> "Undefined"
  ValueS s 			-> s
  ValueD d 			-> show d
  ValueL l 			-> toListStr lang $ fmap (showValue lang) l
  StyledValue s v 	-> showValue lang v
  DisplayValue d v 	-> showValue lang v
  ObjectValue o js 	-> jsonDeserialize lang o js