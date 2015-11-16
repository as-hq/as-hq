module AS.LanguageDefs where

import Prelude

import qualified Data.List as L

import AS.Types.Core

-----------------------------------------------------------------------------------------------------------------------
-- functions

readBool :: ASLanguage -> String -> Bool
readBool lang str = case (head str) of
  't' -> True
  'T' -> True
  'f' -> False
  'F' -> False

-----------------------------------------------------------------------------------------------------------------------
-- definitions

listStops :: ASLanguage -> (String, String)
listStops lang = case lang of 
  R -> ("c(", ")")
  _ -> ("[", "]")

listDelimiter :: ASLanguage -> Char
listDelimiter lang = case lang of 
  OCaml -> ';'
  _ -> ','

null :: ASLanguage -> String
null lang = case lang of 
  Python -> "None"
  R -> "NULL"
  SQL -> "None"
  _ -> error $ "null value not found for " ++ (show lang)

bool :: ASLanguage -> Bool -> String
bool lang val = case val of 
  True -> case lang of 
    R     -> "true"
    Python-> "True"
    SQL   -> "True"
    Excel -> "True"
  False -> case lang of
    R     -> "false"
    Python-> "False"
    SQL   -> "False"
    Excel -> "False"

blockDelimiter :: ASLanguage -> String
blockDelimiter lang = case lang of
  OCaml -> ";;"
  _ -> ""

inlineDelimiter :: ASLanguage -> String
inlineDelimiter lang = case lang of
  R     -> ";"
  Python-> ";"
  OCaml -> ";;"
  SQL   -> ";"
  Excel -> ";"

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