module AS.Parsing.In where

import Import hiding ((<|>))
import qualified Prelude as P
import Prelude ((!!), read)
import AS.Types as Ty
import AS.Parsing.Common
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

valueD :: Parser ASValue
valueD = ValueD <$> double 

bool :: ASLanguage -> Parser Bool
bool lang = fmap rd $ true <|> false
  where
    rd = read :: String -> Bool
    true = case lang of 
      R     -> string "true"
      Python-> string "True"
      OCaml -> string "true"
      SQL   -> string "True"
      CPP   -> string "true"
      Java  -> string "true"
    false = case lang of 
      R     -> string "false"
      Python-> string "False"
      OCaml -> string "false" 
      SQL   -> string "False"
      CPP   -> string "false"
      Java  -> string "false"

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
          Java   -> ("[", "]")

    delim     = case lang of 
      R     -> char ','
      Python-> char ','
      OCaml -> char ';'
      SQL   -> char ','
      CPP   -> char ','
      Java  -> char ','

extractValue :: M.Map String ASValue -> ASValue
extractValue m
  | M.member "error" m        = extractError m
  | M.member "style" m        = extractStyledValue m
  | M.member "displayValue" m = extractDisplayValue m
  | M.member "imagePath" m    = extractImageValue m
  | M.member "stockPrices" m  = extractStockChart m
  | M.member "excelLocs"   m  = extractExcel m 
  | M.member "rickshawData" m = extractRick m 
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

asValue :: ASLanguage -> Parser ASValue 
asValue lang = choice [valueD, valueS, (valueL lang), complexValue, return $ ValueNaN ()]

parseValue :: ASLanguage -> String -> ASValue --needs to change to reflect ValueImage
parseValue lang = fromRight . (parse (asValue lang) "") . T.pack
  where
    fromRight (Right v) = v