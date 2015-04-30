module AS.Parsing where

import Import hiding ((<|>))
import qualified Prelude as P
import Prelude ((!!), read)
import AS.Types
import Text.Regex.Posix
import Data.List (elemIndex)
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Control.Applicative hiding ((<|>), many)
import qualified Data.Map as M
import qualified Data.Text.Lazy (replace)

deleteEmpty = filter ((/=) "")

normalizeRanges :: [ASLocation] -> [ASLocation]
normalizeRanges locs = do
  loc <- locs
  case loc of
    Range (p1, p2) -> decomposeLocs loc
    Index i        -> return loc

parseDependencies :: ASExpression -> [ASLocation]
parseDependencies expr =
  case expr of
    Expression e -> (map fromExcel rangeMatches) ++ (map fromExcel cellMatches)
      where
        rangeMatches = deleteEmpty $ regexList e "([A-Z][0-9]+:[A-Z][0-9]+)"
        cellMatches = deleteEmpty $ regexList noRangeExpr ("[A-Z][0-9]+")
          where
            noRangeExpr = replaceSubstrings e (zip rangeMatches (repeat ""))
    Reference r _ -> [r]

toExcel :: ASLocation -> String
toExcel loc = case loc of 
	(Index a) -> indexToExcel a
	(Range a) -> (indexToExcel (fst a)) ++ ":" ++ (indexToExcel (snd a))

fromExcel :: String -> ASLocation
fromExcel str
	| elem ':' str = Range (excelToIndex $ P.head spt, excelToIndex $ P.last spt)
	| otherwise    = Index . excelToIndex $ str
    where
      spt = map unpack $ T.splitOn (pack ":") (pack str) 

indexToExcel :: (Int, Int) -> String
indexToExcel idx = (['A'..'Z'] !! ((fst idx) - 1)):(show (snd idx))

excelToIndex :: String -> (Int, Int)
excelToIndex str = (toDigit (P.head str), P.read (P.tail str) :: Int)

toDigit :: Char -> Int
toDigit x = fromJust (elemIndex x ['A'..'Z']) + 1

-- takes text, pattern, returns list of occurences
regexList :: String -> String -> [String]
regexList _ "" = []
regexList "" _ = []
regexList text pattern = match:(regexList rest pattern)
  where
    matchTuple = text =~ pattern :: (String, String, String)
    match = (\(_,b,_)->b) matchTuple
    rest = (\(_,_,c)->c) matchTuple

replaceSubstrings :: String -> [(String, String)] -> String
replaceSubstrings "" _ = ""
replaceSubstrings m [] = m
replaceSubstrings m (x:xs) = replaceSubstrings (unpack scrubbed) xs
	where 
		scrubbed = Data.Text.Lazy.replace (pack (fst x)) (pack (snd x)) (pack m) 


lastN :: Int -> [a] -> [a]
lastN n xs = let m = length xs in drop (m-n) xs

decomposeLocs :: ASLocation -> [ASLocation]
decomposeLocs loc = case loc of 
  (Index a) -> [loc]
  (Range (ul, lr)) -> [Index (x,y) | x <- [(fst ul)..(fst lr)], y <- [(snd ul)..(snd lr)] ]

--Parsing values

(<++>) a b = (++) <$> a <*> b
(<:>) a b  = (:) <$> a <*> b

double :: Parser Double
double = fmap rd $ int <++> dec
  where
    rd      = read :: String -> Double
    number  = many1 digit
    plus    = char '+' *> number
    minus   = char '-' <:> number
    int     = plus <|> minus <|> number
    dec     = option "" $ char '.' <:> number

valueD :: Parser ASValue
valueD = ValueD <$> double

valueS :: Parser ASValue
valueS = ValueS <$> ((quotes $ many $ noneOf ['"']) <|> (apostrophes $ many $ noneOf ['\'']))
  where
    quotes = between quote quote
    quote = char '"'
    apostrophes = between apostrophe apostrophe
    apostrophe = char '\''

valueSFailsafe :: Parser ASValue
valueSFailsafe = ValueS <$> (many $ noneOf "'\"")

valueL :: Parser ASValue
valueL = ValueL <$> (brackets $ sepBy asValue (comma >> spaces))
  where
    brackets  = between (char '[') (char ']')
    comma     = char ','

extractValue :: M.Map String ASValue -> ASValue
extractValue m
  | M.member "style" m = extractStyledValue m
  | M.member "displayValue" m = extractDisplayValue m
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
    extractObjectValue mm = ObjectValue typ rep
      where
        ValueS typ  = m M.! "objectType"
        ValueS rep  = m M.! "jsonRepresentation"

complexValue :: Parser ASValue
complexValue = extractValue <$> extractMap
  where
    braces          = between (char '{') (char '}')
    comma           = char ','
    colon           = char ':'
    dictEntry       = do
      ValueS str <- valueS <* (colon >> spaces)
      dictValue <- asValue
      return (str, dictValue)
    extractMap      = M.fromList <$> (braces $ sepBy dictEntry (comma >> spaces))

asValue :: Parser ASValue
asValue = choice [valueD, valueS, valueL, complexValue, return $ ValueNaN ()]

showFilteredValue :: ASLocation -> ASValue -> String
showFilteredValue (Index i) (ValueL l) = showFilteredValue (Index i) (headOrNull l)
  where
    headOrNull [] = ValueNaN ()
    headOrNull (x:xs) = x
showFilteredValue _ a = showValue a

showValue :: ASValue -> String
showValue v = case v of
  ValueNaN () -> "undefined"
  ValueS s -> s
  ValueD d -> show d
  ValueL l -> toListStr $ fmap showValue l
  StyledValue s v -> showValue v
  DisplayValue d v -> showValue v
  ObjectValue o js -> o ++ ".deserialize(" ++ js ++ ")"

parseValue :: String -> ASValue
parseValue = fromRight . (parse asValue "") . T.pack
  where
    fromRight (Right v) = v

--parsing ranges

excelRngToIdxs :: String -> String
excelRngToIdxs rng
  | x1 /= x2 = toListStr $ map toListStr' [[x:(show y) ++ ',':[] | x<-[x1..x2]] | y<-[y1..y2]] 
  | otherwise = toListStr' [x:(show y) ++ ',':[] | x<-[x1..x2], y<-[y1..y2]]
    where
      spt = map unpack $ T.splitOn (pack ":") (pack rng) 
      x1 = (P.head (spt !! 0))
      x2 = (P.head (spt !! 1))
      y1 = (read (P.tail (spt !! 0))::Int)
      y2 = (read (P.tail (spt !! 1))::Int)
      toListStr' lst = "["++(P.init $ concat lst)++"]"

toListStr :: [String] -> String
toListStr lst  = "[" ++ (intercalate "," lst) ++ "]"

excelRangesToLists :: String -> String
excelRangesToLists str = replaceSubstrings str (zip toReplace replaceWith)
  where
    toReplace = deleteEmpty $ regexList str "([A-Z][0-9]+:[A-Z][0-9]+)"
    replaceWith = map excelRngToIdxs toReplace

rangeDiff :: ((Int, Int), (Int, Int)) -> (Int, Int)
rangeDiff (a,b) = (fst b - fst a + 1, snd b - snd a + 1)

reshapeColArr :: [a] -> (Int, Int) -> [[a]]
reshapeColArr lst@(x:xs) (m,n) = 
  if (length lst) /= (m*n-m)
    then (every m lst):(reshapeColArr xs (m,n))
    else []

-- always includes first element
every :: Int -> [a] -> [a]
every n = map P.head . takeWhile (not . null) . P.iterate (drop n)