module AS.LanguageDefs where

import Prelude()
import AS.Prelude
import AS.Types.Cell

import qualified Data.List as L

-----------------------------------------------------------------------------------------------------------------------
-- Definitions

listStops :: ASLanguage -> (String, String)
listStops lang = case lang of 
  R -> ("c(", ")")
  _ -> ("[", "]")

listDelimiter :: ASLanguage -> Char
listDelimiter lang = case lang of 
  OCaml -> ';'
  _ -> ','

outNull :: ASLanguage -> String
outNull lang = case lang of 
  Python -> "None"
  R -> "NULL"
  SQL -> "None"
  _ -> $error $ "null value not found for " ++ (show lang)

inNull :: ASLanguage -> String
inNull lang = case lang of 
  Python -> "null"
  _ -> outNull lang

inNan :: ASLanguage -> String
inNan lang = case lang of 
  Python -> "NaN"
  _ -> $error $ "nan value not found for " ++ (show lang)

outNan :: ASLanguage -> String
outNan lang = case lang of 
  Python -> "np.nan"
  R -> "NaN"
  _ -> $error $ "nan value not found for " ++ (show lang)

inInf :: ASLanguage -> String
inInf lang = case lang of 
  Python -> "Infinity"
  _ -> $error $ "nan value not found for " ++ (show lang)

outInf :: ASLanguage -> String
outInf lang = case lang of 
  Python -> "np.inf"
  R -> "Inf"
  _ -> $error $ "inf value not found for " ++ (show lang)

outBool :: ASLanguage -> Bool -> String
outBool lang val = case val of 
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