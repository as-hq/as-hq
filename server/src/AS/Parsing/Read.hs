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
      CellValue <$> (try $ asValue lang)
  <|> unpackExpanding <$> (try $ possiblyExpanding lang)
  where 
    unpackExpanding (Right val) = CellValue val
    unpackExpanding (Left val) = Expanding val

possiblyExpanding :: ASLanguage -> Parser Either ExpandingValue ASValue
possiblyExpanding lang = try (extractComplex lang)

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

list :: ASLanguage -> Parser ASValue
list lang = sanitizeList . ValueL <$> (brackets $ sepBy (asValue lang) (delim >> spaces))
  where
    brackets     = between (string start) (string end)
    (start, end) = LD.listStops lang
    delim        = LD.listDelimiter lang

nullValue :: ASLanguage -> Parser ASValue
nullValue lang = case lang of 
  Python -> string (LD.null Python) >> return NoValue
  R      -> string (LD.null R) >> return NoValue
  _      -> fail $ "No nullValue in " (show lang)

lexer = P.makeTokenParser Lang.haskellDef

-----------------------------------------------------------------------------------------------------------------------
-- complex parsers

complain = fail "could not parse complex/object/json value"

extractComplex :: ASLanguage -> Parser ASValue
extractComplex lang = 
      f =<< (try $ jsonValue lang) 
  <|> complain
  where f js = case (M.lookup js "tag") of 
    Nothing -> complainField "tag"
    Just tag -> case (extractValue tag js) of 
      Nothing -> complain
      Just val -> return val

extractValue :: String -> JSON -> Maybe ASValue
extractValue tag js = case (read tag :: Maybe ComplexType) of 
  Nothing -> Nothing
  Just tag' -> case tag' of 
    List -> extractList js
    Object -> extractObject js
    Image -> extractImage js
    Error -> extractError js

extractList :: JSON -> Maybe ASValue

extractImage :: JSON -> ASValue
extractImage js = case (M.lookup js "imagePath") of 
  Nothing -> Nothing
  Just path -> Just $ ValueImage path

extractObject :: JSON -> ASValue
extractObject js = case (M.lookup js "objectType") of 
  Nothing -> Nothing
  Just (JSONValue (ValueS otype)) -> case (M.lookup js "listRep") of
    Nothing -> complainField "listRep"
    Just (JSONValue (ValueL olist)) -> case (M.lookup js "attrs") of
    Nothing -> complainField "attrs"
    Just (JSONField oattrs) -> ValueObject otype olist oattrs

extractError :: JSON -> ASValue
extractError js = case (M.lookup js "errMsg") of 
  Nothing -> complainField "errMsg"
  Just (JSONValue (ValueS emsg)) -> case (M.lookup js "errType") of
    Nothing -> complainField "errType"
    Just (JSONValue (ValueS etype)) -> ValueError emsg etype

jsonValue :: ASLanguage -> Parser JSON
jsonValue lang = JSON <$> extractMap
  where
    braces      = between (char '{') (char '}')
    primitive'  = JSONValue <$> (try primitive lang)
    json'       = JSONField <$> (try jsonValue lang)
    delimiter   = spaces >> comma >> spaces
    pair        = do
      key  <- quotedString
      spaces >> char ':' >> spaces
      field <- primitive' <|> json'
      return (key, field)
    extractMap      = M.fromList <$> (braces $ sepBy pair delimiter)

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