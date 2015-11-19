module AS.Parsing.In where

import Prelude
import Text.Regex.Posix
import qualified Data.Functor
import Data.List (elemIndex)
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import qualified Data.List as L
-- import Text.Parsec.String
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.Parsec.Token as O
import qualified Text.Parsec.Language as Lang (haskellDef)
import Control.Applicative hiding ((<|>), many)
import qualified Data.Map as M
import qualified Data.Text.Lazy (replace)

import AS.Types.Core
import AS.Parsing.Common
import qualified AS.Util as U

-- removes the first and last brackets from expression, if they exist
removeBrackets :: String -> String
removeBrackets str
  | length(str)<2 = str --deal with nonsense empty string
  | (L.head str == '{') && (L.last str =='}') = L.init $ L.tail str
  | otherwise = str

--Parsing values ---

readBool :: String -> Bool
readBool str = case (head str) of
  't' -> True
  'T' -> True
  'f' -> False
  'F' -> False

bool :: ASLanguage -> Parser Bool
bool lang = fmap readBool $ true <|> false
  where
    true = case lang of
      R     -> string "true"
      Python-> string "True"
      OCaml -> string "true"
      SQL   -> string "True"
      Excel -> string "True"
    false = case lang of
      R     -> string "false"
      Python-> string "False"
      OCaml -> string "false"
      SQL   -> string "False"
      Excel -> string "False"

valueS :: Parser ASValue
valueS = ValueS <$> U.quotedString

-- valueS :: Parser ASValue
-- valueS = ValueS <$> (O.stringLiteral $ O.makeTokenParser Lang.haskellDef)

-- valueSFailsafe :: Parser ASValue
-- valueSFailsafe = ValueS <$> (many $ noneOf "'\"")


valueL :: ASLanguage -> Parser ASValue
valueL lang = U.sanitizeList . ValueL <$> (brackets $ sepBy (asValue lang) (delim >> spaces))
  where
    brackets  = between (string start) (string end)
      where
        (start, end) = case lang of
          R     -> ("[", "]")   -- TODO R array parsing
          Python-> ("[", "]")
          OCaml -> ("[" , "]")
          SQL   -> ("[", "]")
          CPP   -> ("[", "]")
          Java  -> ("[", "]")
          Excel -> ("[", "]")

    delim     = case lang of
      R     -> char ','
      Python-> char ','
      OCaml -> char ';'
      SQL   -> char ','
      CPP   -> char ','
      Java  -> char ','
      Excel -> char ','

extractValue :: M.Map String ASValue -> ASValue
extractValue m
  | M.member "imagePath" m    = extractImageValue m
  | M.member "objectType" m   = extractObjectValue m
  | M.member "error" m        = extractError m
  | otherwise                 = extractObjectValue m
  where
    extractImageValue mm = ValueImage p
      where
        ValueS p    = m M.! "imagePath"
    extractObjectValue mm = ValueObject disp typ rep
      where
        ValueS disp = m M.! "displayValue"
        ValueS typ  = m M.! "objectType"
        ValueS rep  = m M.! "jsonRepresentation"
    extractError mm = ValueError err typ file pos
      where
        ValueS err         = m M.! "error"
        ValueS typ         = m M.! "errType"
        ValueS file        = m M.! "file"
        ValueI pos         = m M.! "position"

-- TODO this is how Python errors are caught, which is absurd. Need to build special
-- parser for Python errors, and need to build a parser for errors generally. #needsrefactor
jsonValue :: ASLanguage -> Parser ASValue
jsonValue lang = extractValue <$> extractMap
  where
    braces          = between (char '{') (char '}')
    comma           = char ','
    colon           = char ':'
    dictEntry       = do
      ValueS str  <- valueS <* (colon >> spaces) --valueS Python good enough for json
      dictValue   <- asValue lang
      return (str, dictValue)
    extractMap      = M.fromList <$> (braces $ sepBy dictEntry (comma >> spaces))

-- | 
nullValue :: ASLanguage -> Parser ASValue
nullValue lang = case lang of 
  Python -> string "None" >> return NoValue
  R      -> string "NULL" >> return NoValue
  _      -> fail "No nullValue in this language"

-- #needsrefactor should create general error parser later, which parses ocamlError as a special case. (Alex 10/10)
ocamlError :: Parser ASValue
ocamlError = do
  string "File "
  file  <- manyTill anyChar (try (string ", line "))
  pos   <- manyTill anyChar (try (string ", characters"))
  manyTill anyChar (try (string "Error: "))
  err   <- manyTill anyChar (try eof)
  return $ ValueError err "StdErr" file ((read pos :: Int) - 4)

asValue :: ASLanguage -> Parser ASValue
asValue lang =
      try (ValueD <$> float)
  <|> try (ValueI <$> integer)
  <|> try (ValueB <$> bool lang)
  <|> try valueS
  <|> try (valueL lang)
  <|> try (jsonValue lang)
  <|> try ocamlError
  <|> try (nullValue lang)
  <|> return NoValue

parseValue :: ASLanguage -> String -> Either ASExecError ASValue --needs to change to reflect ValueImage
parseValue lang = readOutput . (parse (asValue lang) "")
  where
    readOutput (Right v)  = Right v
    readOutput (Left e)   = Left ParseError

lexer :: P.TokenParser ()
lexer = P.makeTokenParser Lang.haskellDef

integer = fromInteger <$> P.integer lexer
float = U.float'