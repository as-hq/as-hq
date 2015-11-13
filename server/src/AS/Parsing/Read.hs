module AS.Parsing.Read where

import Prelude
import Data.List (elemIndex)
import Data.Maybe
import Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy (replace)
import qualified Data.List as L
import qualified Data.Map as M

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.Parsec.Token as O
import qualified Text.Parsec.Language as Lang (haskellDef)
import Control.Applicative hiding ((<|>), many)

import AS.Types.Core
import AS.Parsing.Common
import AS.LanguageDefs as LD

-----------------------------------------------------------------------------------------------------------------------
-- top-level parsers

parseValue :: ASLanguage -> String -> Either ASExecError CompositeValue
parseValue lang = readOutput . (parse (value lang) "")
  where
    readOutput (Right v)  = Right v
    readOutput (Left e)   = Left ParseError

value :: ASLanguage -> Parser CompositeValue
value lang = 
      unpackExpanding <$> (try $ possiblyExpanding lang)
  <|> CellValue <$> (try $ asValue lang)
  where 
    unpackExpanding (Right val) = CellValue val
    unpackExpanding (Left val) = Expanding val

possiblyExpanding :: ASLanguage -> Parser Either ExpandingValue ASValue
possiblyExpanding = extractComplex

asValue :: ASLanguage -> Parser ASValue
asValue lang =
      try (ValueD <$> float)
  <|> try (ValueI <$> integer)
  <|> try (ValueB <$> bool lang)
  <|> try (ValueS <$> quotedString)
  <|> try (nullValue lang)
  <|> return NoValue

-----------------------------------------------------------------------------------------------------------------------
-- primitive parsers

integer :: Parser Int
integer = fromInteger <$> P.integer lexer

float :: Parser Double
float = float' lexer

-- | Because Haskell's float lexer doesn't parse negative floats out of the box. <__<
float' :: P.TokenParser () -> Parser Double
float' lexer = do 
  maybeMinus <- option ' ' $ try (char '-') 
  f <- P.float lexer
  if (maybeMinus == ' ') then (return f) else (return (-f))

bool :: ASLanguage -> Parser Bool
bool lang = LD.readBool <$> (true <|> false)
  where
    true = string (LD.bool lang True)
    false = string (LD.bool lang False) 

-- | Matches an escaped string and returns the unescaped version. E.g. 
-- "\"hello" -> "hello
quotedString :: Parser String
quotedString = (quoteString <|> apostropheString)
  where
    quoteString      = quotes $ many $ escaped <|> noneOf ['"']
    apostropheString = apostrophes $ many $ escaped <|> noneOf ['\'']
    quotes           = between quote quote
    quote            = char '"' --
    apostrophes      = between apostrophe apostrophe
    apostrophe       = char '\'' -- TODO apostrophes also
    escaped          = char '\\' >> choice (zipWith escapedChar codes replacements)
    escapedChar code replacement = char code >> return replacement
    codes            = ['b',  'n',  'f',  'r',  't',  '\\', '\'', '\"', '/']
    replacements     = ['\b', '\n', '\f', '\r', '\t', '\\', '\'', '\"', '/']

nullValue :: ASLanguage -> Parser ASValue
nullValue lang = case lang of 
  Python -> string (LD.null Python) >> return NoValue
  R      -> string (LD.null R) >> return NoValue
  _      -> fail $ "No nullValue in " (show lang)

lexer = P.makeTokenParser Lang.haskellDef

-----------------------------------------------------------------------------------------------------------------------
-- complex parsers

complain = fail "could not parse complex/object/json value"

extractComplex :: ASLanguage -> Parser CompositeValue
extractComplex lang = 
      f =<< (try $ json lang) 
  <|> complain
  where f js = case (M.lookup js "tag") of 
    Just tag -> case (extractComplexValue tag js) of 
      Just val -> return val
      Nothing -> complain
    Nothing -> complainField "tag"

extractComplexValue :: JSONKey -> JSON -> Maybe CompositeValue
extractComplexValue tag js = case (read tag :: Maybe ComplexType) of 
  Nothing -> Nothing
  Just tag' -> case tag' of 
    List -> Expanding . VList <$> extractCollection "listVals" js
    Object -> Expanding <$> extractObject js
    Image -> CellValue <$> extractImage js
    Error -> CellValue <$> extractError js

extractCollection :: JSON -> JSONKey -> Maybe Collection
extractCollection js key = case (M.lookup js key) of 
  Just (JSONLeaf (ListValue collection)) -> collection
  _ -> Nothing 

extractImage :: JSON -> Maybe ASValue
extractImage js = case (M.lookup js "imagePath") of 
  Just (JSONLeaf (PrimitiveValue (ValueS path))) -> Just $ ValueImage path
  _ -> Nothing

extractObject :: JSON -> Maybe ExpandingValue
extractObject js = case (M.lookup js "objectType") of 
  Just (JSONLeaf (PrimitiveValue (ValueS o))) -> case (read o :: Maybe ObjectType) of 
    (Just otype) -> case otype of 
      NPArray -> VNPArray <$> extractCollection "arrayVals" js 
      NPMatrix -> VNPMatrix <$> extractCollection "matrixVals" js
      PDataFrame -> VPDataFrame <$> dflabels <*> dfdata
        where
          dflabels = rdLabels <$> extractCollection "dfLabels" js
          dfdata = extractCollection "dfData" js
          rdLabels coll = case coll of 
            (A labels) -> map (\(ValueS s) -> s) labels
            _ -> error "expected dataframe labels to be list of strings"
      PSeries -> VPSeries . rdSeries <$> extractCollection "seriesVals"
        where 
          rdSeries coll = case coll of 
            (A series) -> series
            _ -> error "expected pandas series to be one-dimensional" 
  _ -> Nothing

extractError :: JSON -> Maybe ASValue
extractError js = case (M.lookup js "errorMsg") of 
  Nothing -> complainField "errorMsg"
  Just (JSONValue (ValueS emsg)) -> case (M.lookup js "errorType") of
    Nothing -> complainField "errorType"
    Just (JSONValue (ValueS etype)) -> ValueError emsg etype

json :: ASLanguage -> Parser JSON
json lang = JSON <$> extractMap
  where
    braces      = between (char '{') (char '}')
    leaf        = JSONLeaf <$> (try jsonValue lang)
    tree        = JSONTree <$> (try json lang)
    delimiter   = spaces >> comma >> spaces
    pair        = do
      key  <- quotedString
      spaces >> char ':' >> spaces
      field <- tree <|> leaf
      return (key, field)
    extractMap      = M.fromList <$> (braces $ sepBy pair delimiter)

jsonValue :: Parser JSONValue
jsonValue lang = 
      ListValue <$> list lang
  <|> PrimitiveValue <$> asValue lang

-- this parser will only allow 1 and 2D lists
list :: ASLanguage -> Parser Collection
list lang = 
      A <$> array (asValue lang)
  <|> M <$> array (array $ asValue lang)
  where
    (start, end) = LD.listStops lang
    brackets     = between (string start) (string end)
    delimiter    = spaces >> (LD.listDelimiter lang) >> spaces
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