module AS.Parsing.Read where

import Prelude()
import AS.Prelude

import Data.List (elemIndex)
import Data.Maybe
import Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy (replace)
import qualified Data.List as L
import qualified Data.Map as M
import Text.Read (readMaybe)


import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.Parsec.Token as O
import qualified Text.Parsec.Language as Lang (haskellDef)
import Control.Applicative hiding ((<|>), many)

import AS.Types.Cell
import AS.Types.Eval

import AS.Parsing.Common as C
import qualified AS.LanguageDefs as LD

-----------------------------------------------------------------------------------------------------------------------
-- top-level parsers

parseValue :: ASLanguage -> String -> Either ASExecError CompositeValue
parseValue lang = readOutput . (parse (value lang) "")
  where
    readOutput (Right v)  = Right v
    readOutput (Left e)   = Left ParseError

-- TODO alex fill this in
parseFormatValue = undefined

value :: ASLanguage -> Parser CompositeValue
value lang = 
      CellValue <$> try (asValue lang)
  <|> try (parseComposite lang)

asValue :: ASLanguage -> Parser ASValue
asValue lang =
      try (ValueD <$> float)
  <|> try (ValueI <$> integer)
  <|> try (ValueB <$> C.bool)
  <|> try (ValueS <$> quotedString)
  <|> try (nullValue lang)
  <|> try (nanValue lang)
  <|> try (infValue lang)
  <|> try (cellJsonValue lang)

-----------------------------------------------------------------------------------------------------------------------
-- primitive parsers

integer :: Parser Integer
integer = P.integer lexer

float :: Parser Double
float = float'

nullValue :: ASLanguage -> Parser ASValue
nullValue lang = case lang of 
  Python -> string (LD.inNull Python) >> return NoValue
  _      -> fail $ "No nullValue in " ++ (show lang)

nanValue :: ASLanguage -> Parser ASValue
nanValue lang = case lang of 
  Python -> string (LD.inNan Python) >> return ValueNaN
  _      -> fail $ "No NaN value in " ++ (show lang)

infValue :: ASLanguage -> Parser ASValue
infValue lang = case lang of 
  Python -> string (LD.inInf Python) >> return ValueInf
  _      -> fail $ "No Inf value in " ++ (show lang)

lexer = P.makeTokenParser Lang.haskellDef

cellJsonValue :: ASLanguage -> Parser ASValue
cellJsonValue lang = f =<< (try $ json lang)
  where 
    f js = maybe complain return $ extractCellValue js

-----------------------------------------------------------------------------------------------------------------------
-- composite parsers

-- for looking up arbitrary fields
(.>) :: JSON -> JSONKey -> Maybe JSONField
(.>) = flip M.lookup

-- for looking up strings
(.$>) :: JSON -> JSONKey -> Maybe String
(.$>) js key = (\(JSONLeaf (SimpleValue (ValueS s))) -> s) <$> M.lookup key js

complain = fail "could not parse complex/object/json value"

parseComposite :: ASLanguage -> Parser CompositeValue
parseComposite lang = 
      f =<< try (json lang) 
  <|> complain
  where f js = case (js .$> "tag") of 
            Just tag -> maybe complain return $ extractCompositeValue tag js
            Nothing -> fail "expecting field \"tag\" in complex value"

extractCompositeValue :: JSONKey -> JSON -> Maybe CompositeValue
extractCompositeValue tag js = case tag of 
  "CellValue" -> CellValue <$> extractCellValue js
  "Expanding" -> Expanding <$> extractExpanding js

extractCellValue :: JSON -> Maybe ASValue
extractCellValue js = case (js .$> "cellValueType") of 
  Just "Image"      -> extractImage js 
  Just "Error"      -> extractError js
  Just "Serialized" -> extractSerialized js
  _ -> Nothing

extractExpanding :: JSON -> Maybe ExpandingValue
extractExpanding js = 
  let readEType e = $read e :: ExpandingType
  in case (readEType <$> js .$> "expandingType") of 
    Just List -> VList <$> extractCollection js "listVals"
    Just NPArray -> VNPArray <$> extractCollection js "arrayVals"
    Just NPMatrix -> (\(M mat) -> VNPMatrix mat) <$> extractCollection js "matrixVals"
    Just PDataFrame -> Just $ VPDataFrame labels indices vals
      where
        (Just (A labels)) = extractCollection js "dfLabels"
        (Just (A indices)) = extractCollection js "dfIndices"
        (Just (M vals)) = extractCollection js "dfData"
    Just PSeries -> Just $ VPSeries indices vals
      where 
        (Just (A indices)) = extractCollection js "seriesIndices"
        (Just (A vals)) = extractCollection js "seriesData"
    _ -> Nothing

extractSerialized :: JSON -> Maybe ASValue
extractSerialized js = ValueSerialized <$> (js .$> "serializedValue") <*> (js .$> "displayName")

extractImage :: JSON -> Maybe ASValue
extractImage js = ValueImage <$> (js .$> "imagePath")  

extractError :: JSON -> Maybe ASValue
extractError js = ValueError <$> (js .$> "errorMsg") <*> (js .$> "errorType") 

extractCollection :: JSON -> JSONKey -> Maybe Collection
extractCollection js key = case (js .> key) of 
  Just (JSONLeaf (ListValue collection)) -> Just collection
  _ -> Nothing 

-----------------------------------------------------------------------------------------------------------------------
-- low-level parsers

json :: ASLanguage -> Parser JSON
json lang = extractMap
  where
    braces      = between (char '{') (char '}')
    leaf        = JSONLeaf <$> jsonValue lang
    tree        = JSONTree <$> json lang
    delimiter   = spaces >> (char ',') >> spaces
    pair        = do
      spaces
      key  <- quotedString
      spaces >> char ':' >> spaces
      field <- try tree <|> try leaf
      spaces
      return (key, field)
    extractMap  = M.fromList <$> (braces $ sepBy pair delimiter)

jsonValue :: ASLanguage -> Parser JSONValue
jsonValue lang = 
      ListValue <$> list lang
  <|> SimpleValue <$> asValue lang

-- this parser will only allow 1 and 2D lists
list :: ASLanguage -> Parser Collection
list lang = 
      A <$> try (array $ asValue lang)
  <|> M <$> try (array $ array $ asValue lang)
  where
    (start, end) = LD.listStops lang
    brackets     = between (string start) (string end)
    delimiter    = spaces >> (char $ LD.listDelimiter lang) >> spaces
    array p      = brackets $ sepBy p delimiter

 --DEPRECATED
 -- #needsrefactor should create general error parser later, which parses ocamlError as a special case. (Alex 10/10)
--ocamlError :: Parser ASValue
--ocamlError = do
--  string "File "
--  file  <- manyTill anyChar (try (string ", line "))
--  pos   <- manyTill anyChar (try (string ", characters"))
--  manyTill anyChar (try (string "Error: "))
--  err   <- manyTill anyChar (try eof)
--  return $ ValueError err "StdErr" file ((read pos :: Int) - 4)