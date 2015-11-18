module AS.Parsing.Read where

import Prelude
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

import AS.Types.Core
import AS.Parsing.Common
import qualified AS.LanguageDefs as LD

-----------------------------------------------------------------------------------------------------------------------
-- top-level parsers

parseValue :: ASLanguage -> String -> Either ASExecError CompositeValue
parseValue lang = readOutput . (parse (value lang) "")
  where
    readOutput (Right v)  = Right v
    readOutput (Left e)   = Left ParseError

value :: ASLanguage -> Parser CompositeValue
value lang = 
      CellValue <$> try (asValue lang)
  <|> (try $ extractComplex lang)

asValue :: ASLanguage -> Parser ASValue
asValue lang =
      try (ValueD <$> float)
  <|> try (ValueI <$> integer)
  <|> try (ValueB <$> bool lang)
  <|> try (ValueS <$> quotedString)
  <|> try (nullValue lang)

-----------------------------------------------------------------------------------------------------------------------
-- primitive parsers

integer :: Parser Int
integer = fromInteger <$> P.integer lexer

float :: Parser Double
float = float' lexer

bool :: ASLanguage -> Parser Bool
bool lang = true <|> false
  where
    true = string (LD.bool lang True) >> return True
    false = string (LD.bool lang False) >> return False

nullValue :: ASLanguage -> Parser ASValue
nullValue lang = case lang of 
  Python -> string (LD.null Python) >> return NoValue
  R      -> string (LD.null R) >> return NoValue
  _      -> fail $ "No nullValue in " ++ (show lang)

lexer = P.makeTokenParser Lang.haskellDef

-----------------------------------------------------------------------------------------------------------------------
-- complex parsers

(.>) :: JSON -> JSONKey -> Maybe JSONField
(.>) = flip M.lookup

complain = fail "could not parse complex/object/json value"

extractComplex :: ASLanguage -> Parser CompositeValue
extractComplex lang = 
      f =<< (try $ json lang) 
  <|> complain
  where f js = case (js .> "tag") of 
            Just (JSONLeaf (SimpleValue (ValueS tag))) -> case (extractComplexValue tag js) of 
              Just val -> return val
              Nothing -> complain
            Nothing -> fail "expecting field \"tag\" in complex value"

extractComplexValue :: JSONKey -> JSON -> Maybe CompositeValue
extractComplexValue tag js = case (readMaybe tag :: Maybe ComplexType) of 
  Nothing -> Nothing
  Just tag' -> case tag' of 
    List -> Expanding . VList <$> extractCollection js "listVals"
    Object -> Expanding <$> extractObject js
    Image -> CellValue <$> extractImage js
    Error -> CellValue <$> extractError js

extractCollection :: JSON -> JSONKey -> Maybe Collection
extractCollection js key = case (js .> key) of 
  Just (JSONLeaf (ListValue collection)) -> Just collection
  _ -> Nothing 

extractImage :: JSON -> Maybe ASValue
extractImage js = case (js .> "imagePath") of 
  Just (JSONLeaf (SimpleValue (ValueS path))) -> Just $ ValueImage path
  _ -> Nothing

extractObject :: JSON -> Maybe ExpandingValue
extractObject js = case (js .> "objectType") of 
  Just (JSONLeaf (SimpleValue (ValueS o))) -> case (readMaybe o :: Maybe ObjectType) of 
    Just otype -> case otype of 
      NPArray -> VNPArray <$> extractCollection js "arrayVals"
      NPMatrix -> (\(M mat) -> VNPMatrix mat) <$> extractCollection js "matrixVals"
      PDataFrame -> Just $ VPDataFrame labels indices vals
        where
          (Just (A labels)) = extractCollection js "dfLabels"
          (Just (A indices)) = extractCollection js "dfIndices"
          (Just (M vals)) = extractCollection js "dfData"
        -- ^^ if these don't exist, something fucked up.
      PSeries -> Just $ VPSeries indices vals
        where 
          (Just (A indices)) = extractCollection js "seriesIndices"
          (Just (A vals)) = extractCollection js "seriesData"
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

extractError :: JSON -> Maybe ASValue
extractError js = case (js .> "errorMsg") of 
  Just (JSONLeaf (SimpleValue (ValueS emsg))) -> case (js .> "errorType") of
    Just (JSONLeaf (SimpleValue (ValueS etype))) -> Just $ ValueError emsg etype
    _ -> Nothing
  _ -> Nothing

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
    extractMap      = M.fromList <$> (braces $ sepBy pair delimiter)

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