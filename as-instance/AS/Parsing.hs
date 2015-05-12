module AS.Parsing where

import Import hiding ((<|>))
import qualified Prelude as P
import Prelude ((!!), read)
import AS.Types as Ty
import Text.Regex.Posix
import Data.List (elemIndex)
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import qualified Data.List as L
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

-- takes in an ASExpression Range, row/col offset, parses expression, and gives list of parseDependencies, as well as new asexpression
parseDependenciesRelative:: ASExpression -> Int -> Int -> ([ASLocation],ASExpression)
parseDependenciesRelative xp rowOff colOff = (concat $ map (\m -> fromExcelRelativeLoc m rowOff colOff) matches, newExp)
  where 
    rangeMatches = deleteEmpty $ regexList (expression xp) "(\\${0,1}+[A-Z]+\\${0,1}+[0-9]+:+\\${0,1}+[A-Z]+\\${0,1}+[0-9]+)"
    cellMatches = deleteEmpty $ regexList noRangeExpr ("\\${0,1}+[A-Z]+\\${0,1}++[0-9]+")
      where
        noRangeExpr = replaceSubstrings (expression xp) (zip rangeMatches (repeat ""))  --get rid of range matches, then look for index matches
    matches = rangeMatches ++ cellMatches
    newExp = Expression $ replaceSubstrings (expression xp) (zip matches (map (\m -> fromExcelRelativeString m rowOff colOff) matches ))

-- replaces any occurrences of A2:B4 with A2, keeps dollar signs
topLeft :: String-> String
topLeft str = tlString
  where 
    rangeMatches = deleteEmpty $ regexList str "(\\${0,1}+[A-Z]+\\${0,1}+[0-9]+:+\\${0,1}+[A-Z]+\\${0,1}+[0-9]+)"
    tlString = replaceSubstrings str (zip rangeMatches (map tlFunc rangeMatches))
      where
        tlFunc m 
          |elem ':' m = first --this is the only case that will be used
          |otherwise = m
            where 
              spt = map unpack $ T.splitOn (pack ":") (pack m) 
              first = P.head spt

-- removes the first and last brackets from expression, if they exist
removeBrackets :: String -> String
removeBrackets str 
  | length(str)<2 = str --deal with nonsense empty string
  | (L.head str == '{') && (L.last str =='}') = L.init $ L.tail str
  | otherwise = str

fromExcelRelativeString :: String -> Int -> Int -> String
fromExcelRelativeString str row col 
  | elem ':' str =  (fromExcelRelativeString first row col) ++ ":" ++ (fromExcelRelativeString second row col)
  | otherwise = func str row col
    where  
      func ('$':letter:'$':num) row col = str --letter:num 
      func ('$':letter:num) row col = '$':(letter:(show ((P.read num::Int)+col))) --(letter:(show ((P.read num::Int)+col)))
      func (letter:'$':num:"") row col = (['A'..'Z']!!((toDigit letter)+row-1)):("$"++(num:""))  --(['A'..'Z']!!((toDigit letter)+row-1)):num
      func (letter:num) row col = (['A'..'Z']!!((toDigit letter)+row-1)):(show ((P.read num::Int)+col))
      spt = map unpack $ T.splitOn (pack ":") (pack str) 
      first = P.head spt
      second = P.last spt

-- takes in string ("$A$1", "A2:A5"), offset row, offset col, and gives a list of the correct ASLocations that they correspond to (relative references)
fromExcelRelativeLoc :: String -> Int -> Int -> [ASLocation]
fromExcelRelativeLoc str row col 
  | elem ':' str = decomposeLocs $ Range ( Ty.index ((fromExcelRelativeLoc first row col)!!0) , Ty.index ((fromExcelRelativeLoc second row col)!!0)) --deal with A1:A9 
  | otherwise = [func str row col]
    where 
      func ('$':letter:'$':num) row col = Index ((toDigit letter),(P.read num::Int))
      func ('$':letter:num) row col = Index ((toDigit letter),(P.read num::Int)+col)
      func (letter:'$':num) row col = Index ((toDigit letter)+row,(P.read num::Int))
      func (letter:num) row col = Index ((toDigit letter)+row,(P.read num::Int)+col)
      spt = map unpack $ T.splitOn (pack ":") (pack str) 
      first = P.head spt
      second = P.last spt

-- deletes all occurrences of $ from text, used for evalPy on relative references
deleteDollars :: String -> String
deleteDollars str = T.unpack $ T.concat $ T.splitOn (T.pack "$") (T.pack str)


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

-- THIS NEEDS TO EVENTUALLY CHANGE TO SUPPORT AA,AB etc, also true in other places
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

-- replaceSubstrings needs to correctly sort its input
replaceSubstrings :: String -> [(String, String)] -> String
replaceSubstrings "" _ = ""
replaceSubstrings m [] = m
replaceSubstrings m (x:xs) = replaceSubstrings (unpack scrubbed) ys
  where 
    (y:ys)=L.sortBy sortStrList (x:xs) 
    scrubbed = Data.Text.Lazy.replace (pack (fst y)) (pack (snd y)) (pack m) 
    --replace all occurrences of fst x with snd x in m
    --keep doing this for all (str,str) in list
    --pack: string to text

sortStrList :: (String,String) -> (String,String) -> Ordering
sortStrList (a1, b1) (a2, b2)
  | (show $ dropWhile isUpper a1) < (show $ dropWhile isUpper a2) = GT
  | (show $ dropWhile isUpper a1) > (show $ dropWhile isUpper a2) = LT
  | otherwise = EQ

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

bool :: Parser Bool 
bool = fmap rd $ true <|> false 
  where
    rd = read :: String -> Bool
    true = string "True"
    false = string "False"

valueB :: Parser ASValue 
valueB = ValueB <$> bool

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
  | M.member "imagePath" m = extractImageValue m
  | M.member "stockPrices" m = extractStockChart m
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
asValue = choice [valueD, valueB, valueS, valueL, complexValue, return $ ValueNaN ()]

showFilteredValue :: ASLocation -> ASValue -> String
showFilteredValue (Index i) (ValueL l) = showFilteredValue (Index i) (headOrNull l)
  where
    headOrNull [] = ValueNaN ()
    headOrNull (x:xs) = x
showFilteredValue _ a = showValue a

showValue :: ASValue -> String
showValue v = case v of
  ValueImage path -> "PLOT"--ADDED, open file here?
  ValueB b -> show b
  ValueNaN () -> "UNDefined"
  ValueS s -> s
  ValueD d -> show d
  ValueL l -> toListStr $ fmap showValue l
  StyledValue s v -> showValue v
  DisplayValue d v -> showValue v
  ObjectValue o js -> o ++ ".deserialize(" ++ js ++ ")"

parseValue :: String -> ASValue --needs to change to reflect ValueImage
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

excelRangesToIterables :: String -> String
excelRangesToIterables str = replaceSubstrings str (zip toReplace replaceWith)
  where
    toReplace = deleteEmpty $ regexList str "([A-Z][0-9]+:[A-Z][0-9]+)"
    replaceWith = map ((\x->"arr("++x++")") . excelRngToIdxs) toReplace

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