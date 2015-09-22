module AS.Parsing.In where

import Prelude
import Text.Regex.Posix
import qualified Data.Functor
import Data.List (elemIndex)
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import qualified Data.List as L
import Text.Parsec
-- import Text.Parsec.String
import Text.Parsec.Text
import qualified Text.Parsec.Token as O
import qualified Text.Parsec.Language as Lang (haskellDef)
import Control.Applicative hiding ((<|>), many)
import qualified Data.Map as M
import qualified Data.Text.Lazy (replace)

import AS.Types.Core
import AS.Parsing.Common
import AS.Util

-- removes the first and last brackets from expression, if they exist
removeBrackets :: String -> String
removeBrackets str 
  | length(str)<2 = str --deal with nonsense empty string
  | (L.head str == '{') && (L.last str =='}') = L.init $ L.tail str
  | otherwise = str

--Parsing values ---

double :: Parser Double
double = fmap rd $ int <++> dec
  where
    rd      = read :: String -> Double
    dNumber = int <++> dec
    number  = many1 digit
    plus    = char '+' *> number
    minus   = char '-' <:> number
    int     = plus <|> minus <|> number
    dec     = option "" $ (Text.Parsec.try $ period <:> number) <|> (skip period)
    period  = char '.'

int :: Parser Int
int = fmap rd $ int
  where
    rd      = read :: String -> Int
    number  = manyTill digit (try eof)
    plus    = char '+' *> number
    minus   = char '-' <:> number
    int     = plus <|> minus <|> number

valueD :: Parser ASValue
valueD = ValueD <$> double 

valueI :: Parser ASValue
valueI = ValueI <$> int

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
      CPP   -> string "true"
      Java  -> string "true"
      Excel -> string "True"
    false = case lang of 
      R     -> string "false"
      Python-> string "False"
      OCaml -> string "false" 
      SQL   -> string "False"
      CPP   -> string "false"
      Java  -> string "false"
      Excel -> string "False"

valueB :: ASLanguage -> Parser ASValue
valueB lang = ValueB <$> (bool lang)

valueS :: Parser ASValue
valueS = ValueS <$> (quoteString <|> apostropheString)
  where
  	quoteString 		= quotes $ many $ escaped <|> noneOf ['"']
  	apostropheString 	= apostrophes $ many $ escaped <|> noneOf ['\'']
  	quotes = between quote quote
  	quote = char '"' -- 
  	apostrophes = between apostrophe apostrophe
  	apostrophe = char '\'' -- TODO apostrophes also
  	escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
  	escapedChar code replacement = char code >> return replacement
  	codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
  	replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

-- valueS :: Parser ASValue
-- valueS = ValueS <$> (O.stringLiteral $ O.makeTokenParser Lang.haskellDef)

-- valueSFailsafe :: Parser ASValue
-- valueSFailsafe = ValueS <$> (many $ noneOf "'\"")

valueL :: ASLanguage -> Parser ASValue
valueL lang = ValueL <$> (brackets $ sepBy (asValue lang) (delim >> spaces))
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
  | M.member "error" m        = extractError m
  | M.member "style" m        = extractStyledValue m
  | M.member "displayValue" m = extractDisplayValue m
  | M.member "imagePath" m    = extractImageValue m
  | M.member "stockPrices" m  = extractStockChart m
  | M.member "excelLocs"   m  = extractExcel m 
  | M.member "rickshawData" m = extractRick m 
  | M.member "objectType" m   = extractObjectValue m
  | otherwise = extractObjectValue m
  where
    extractExcel mm       = ExcelSheet l e v
      where
        l           = m M.! "excelLocs"
        e           = m M.! "excelExprs"
        v           = m M.! "excelVals"
    extractStyledValue mm = StyledValue s v
      where
        ValueS s    = m M.! "style"
        v           = m M.! "value"
    extractDisplayValue mm = DisplayValue s v
      where
        ValueS s    = m M.! "displayValue"
        v           = m M.! "actualValue"
    -- { 'imagePath': 'whatever the path is' }
    -- print({ 'imagePath': 'whatever the path is' })
    extractImageValue mm = ValueImage p
      where
        ValueS p    = m M.! "imagePath"
    extractObjectValue mm = ObjectValue typ rep
      where
        ValueS typ  = m M.! "objectType"
        ValueS rep  = m M.! "jsonRepresentation"
    extractStockChart mm = StockChart prices name
      where
        prices      = m M.! "stockPrices"
        ValueS name = m M.! "stockName"
    extractRick mm = Rickshaw d
      where
        d           = m M.! "rickshawData"
    extractError mm      = ValueError err typ file (floor pos)
      where
        ValueS err         = m M.! "error"
        ValueS typ         = m M.! "err_type"
        ValueS file        = m M.! "file"
        ValueD pos         = m M.! "position"

--extractValue :: M.Map String ASValue -> ASValue
--extractValue m
--  | M.member "error" m        = extractError m
--  | M.member "style" m        = extractStyledValue m
--  | M.member "displayValue" m = extractDisplayValue m
--  | M.member "imagePath" m    = extractImageValue m
--  | M.member "stockPrices" m  = extractStockChart m
--  | M.member "excelLocs"   m  = extractExcel m 
--  | M.member "rickshawData" m = extractRick m 
--  | M.member "objectValue" m  = extractObjectValue m
--  | otherwise                 = parseMap m  
--  where
--    extractExcel mm       = ExcelSheet l e v
--      where
--        l           = m M.! "excelLocs"
--        e           = m M.! "excelExprs"
--        v           = m M.! "excelVals"
--    extractStyledValue mm = StyledValue s v
--      where
--        ValueS s    = m M.! "style"
--        v           = m M.! "value"
--    extractDisplayValue mm = DisplayValue s v
--      where
--        ValueS s    = m M.! "displayValue"
--        v           = m M.! "actualValue"
--    -- { 'imagePath': 'whatever the path is' }
--    -- print({ 'imagePath': 'whatever the path is' })
--    extractImageValue mm = ValueImage p
--      where
--        ValueS p    = m M.! "imagePath"
--    extractObjectValue mm = ObjectValue typ rep
--      where
--        ValueS typ  = m M.! "objectType"
--        ValueS rep  = m M.! "jsonRepresentation"
--    extractStockChart mm = StockChart prices name
--      where
--        prices      = m M.! "stockPrices"
--        ValueS name = m M.! "stockName"
--    extractRick mm = Rickshaw d
--      where
--        d           = m M.! "rickshawData"
--    extractError mm      = ValueError err typ file (floor pos)
--      where
--        ValueS err         = m M.! "error"
--        ValueS typ         = m M.! "err_type"
--        ValueS file        = m M.! "file"
--        ValueD pos         = m M.! "position"
--    --parseMap mm = ValueL listRep -- minimal amount of parsing to get maps to work, doesn't work for different sized lists
--    --  where
--    --    f (s, ValueL l) = (ValueS s):l
--    --    f (s, o) = (ValueS s):[o]
--    --    listRep = map ValueL $ L.transpose $ map f (M.toList mm) 


complexValue :: Parser ASValue
complexValue = extractValue <$> extractMap
  where
    braces          = between (char '{') (char '}')
    comma           = char ','
    colon           = char ':'
    dictEntry       = do
      ValueS str <- valueS <* (colon >> spaces) --valueS Python good enough for json
      dictValue <- asValue Python
      return (str, dictValue)
    extractMap      = M.fromList <$> (braces $ sepBy dictEntry (comma >> spaces))

ocamlError :: Parser ASValue
ocamlError = do
  string "File "
  file <- manyTill anyChar (try (string ", line "))
  pos <- manyTill anyChar (try (string ", characters"))
  manyTill anyChar (try (string "Error: "))
  err <- manyTill anyChar (try eof)
  return $ ValueError err "StdErr" file ((read pos :: Int) - 4)

asValue :: ASLanguage -> Parser ASValue 
asValue lang = choice [valueD, valueS, (valueB lang), (valueL lang), complexValue, ocamlError, return $ ValueNaN ()]

parseValue :: ASLanguage -> String -> ASValue --needs to change to reflect ValueImage
parseValue lang = readOutput . (parse (asValue lang) "") . T.pack
  where
    readOutput (Right v) = v
    readOutput (Left e) = ValueError (show e) "Parsing error" "AlphaSheets evaluation engine" 0 -- minimal amount of parsing to get maps to work, doesn't work for different sized lists
    --  where
    --    f (s, ValueL l) = (ValueS s):l
    --    f (s, o) = (ValueS s):[o]
    --    listRep = map ValueL $ L.transpose $ map f (M.toList mm) 