module AS.Parsing where

import Import hiding (index)
import qualified Prelude 
import Prelude ((!!))
import AS.Types
import Text.Regex.Posix
import Data.List (elemIndex)
import Data.Maybe
import Data.Char
import qualified Data.Text.Lazy (replace)

deleteEmpty = filter ((/=) "")

normalizeRanges :: [ASLocation] -> [ASLocation]
normalizeRanges locs = do
  loc <- locs
  case loc of
    Range (p1, p2) -> map Index $ decomposeLocs loc
    Index i        -> return loc

parseDependencies :: ASExpression -> [ASLocation]
parseDependencies expr = (map fromExcel rangeMatches) ++ (map fromExcel cellMatches)
  where
    rangeMatches = deleteEmpty $ regexList (expression expr) "([A-Z][0-9]:[A-Z][0-9])"
    cellMatches = deleteEmpty $ regexList noRangeExpr ("[A-Z][0-9]")
    	where
    		noRangeExpr = replaceSubstrings (expression expr) (zip rangeMatches (repeat ""))

toExcel :: ASLocation -> String
toExcel loc = case loc of 
	(Index a) -> indexToExcel a
	(Range a) -> (indexToExcel (fst a)) ++ ":" ++ (indexToExcel (snd a))

fromExcel :: String -> ASLocation
fromExcel str
	| elem ':' str = Range (excelToIndex (take 2 str), excelToIndex (lastN 2 str))
	| otherwise    = Index . excelToIndex $ str

indexToExcel :: (Int, Int) -> String
indexToExcel idx = (['A'..'Z'] !! ((fst idx) - 1)):(show (snd idx))

excelToIndex :: String -> (Int, Int)
excelToIndex str = (toDigit (Prelude.head str), Prelude.read (Prelude.tail str) :: Int)

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

decomposeLocs :: ASLocation -> [(Int, Int)]
decomposeLocs loc = case loc of 
  (Index a) -> [a]
  (Range (ul, lr)) -> [(x,y) | x <- [(fst ul)..(fst lr)], y <- [(snd ul)..(snd lr)] ]

rangeDiff :: ASLocation -> ASLocation -> (Int, Int)
rangeDiff (Index a) (Index b) = (fst b - fst a, snd b - snd a)

maxRangeDiff :: [ASLocation] -> (Int,Int)
maxRangeDiff locs = (diff $ map fst myTuples, diff $ map snd myTuples)
  where
    myTuples = concat $ map decomposeLocs locs
    diff = (-) <$> Prelude.maximum <*> Prelude.minimum

showValue :: ASValue -> String
showValue (ValueS str) = str
showValue (ValueD d) = show d
showValue (ValueLD ld) = show ld
showValue (ValueLS ls) = show ls

parseValue :: String -> ASValue
parseValue str 
  | str == "" = ValueS ""
  | isDouble str = ValueD (Prelude.read str :: Double)
  | isDoubleList str = ValueLD (Prelude.read str :: [Double])
  | isStringList str = ValueLS (Prelude.read str :: [String])
  | otherwise = ValueS str

isDouble :: String -> Bool
isDouble str = and . map (\c -> (isDigit c) || (c == '.')) $ str

isDoubleList :: String -> Bool
isDoubleList str = (and . map (\c -> (isDigit c) || (isSpace c) || (c == ',') || (c == '.') || (c == '[') || (c == ']')) $ str) && (elem '[' str)

isStringList :: String -> Bool
isStringList str = (and . map (\c -> (isAlpha c) || (isSpace c) || (c == ',') || (c == '[') || (c == ']')) $ str) && (elem '[' str)

excelRngToIdxs :: String -> String
excelRngToIdxs rng = "["++(Prelude.init $ concat myList)++"]" 
  where 
    myList = [x:y:',':[] | x<-[(rng !! 0)..(rng !! 3)], y<-[(rng !! 1)..(rng !! 4)]]

excelRangesToLists :: String -> String
excelRangesToLists str = replaceSubstrings str (zip toReplace replaceWith)
  where
    toReplace = deleteEmpty $ regexList str "([A-Z][0-9]:[A-Z][0-9])"
    replaceWith = map excelRngToIdxs toReplace
