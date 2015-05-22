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
    false = case lang of 
      R     -> string "false"
      Python-> string "False"
      OCaml -> string "false" 

valueB :: ASLanguage -> Parser ASValue
valueB lang = ValueB <$> (bool lang)

-- valueS :: ASLanguage -> Parser ASValue
-- valueS lang = ValueS <$> ((quotes $ many $ noneOf ['"']) <|> (apostrophes $ many $ noneOf ['\'']))
--   where
--     quotes = between quote quote
--     quote = case lang of 
--       otherwise -> char '"' -- TODO quotes for langs
--     apostrophes = between apostrophe apostrophe
--     apostrophe = case lang of 
--       otherwise -> char '\'' -- TODO apostrophes also

valueS :: Parser ASValue
valueS = ValueS <$> (O.stringLiteral $ O.makeTokenParser Lang.haskellDef)

-- valueSFailsafe :: Parser ASValue
-- valueSFailsafe = ValueS <$> (many $ noneOf "'\"")

valueL :: ASLanguage -> Parser ASValue
valueL lang = ValueL <$> (brackets $ sepBy (asValue lang) (delim >> spaces))
  where
    brackets  = between (string start) (string end)
      where
        (start, end) = case lang of 
          R -> ("[", "]")   -- TODO R array parsing
          Python-> ("[", "]")
          OCaml -> ("[" , "]")
    delim     = case lang of 
      R     -> char ','
      Python-> char ','
      OCaml -> char ';'

extractValue :: M.Map String ASValue -> ASValue
extractValue m
  | M.member "style" m        = extractStyledValue m
  | M.member "displayValue" m = extractDisplayValue m
  | M.member "imagePath" m    = extractImageValue m
  | M.member "stockPrices" m  = extractStockChart m
  | M.member "error" m        = extractError m
  | otherwise = extractObjectValue m
  where
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
    extractError mm      = ValueError err typ file (read pos :: Int)
      where
        ValueS err         = m M.! "error"
        ValueS typ         = m M.! "err_type"
        ValueS file        = m M.! "file"
        ValueS pos         = m M.! "position"

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