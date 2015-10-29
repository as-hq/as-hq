module AS.Parsing.Common where

import Prelude
import Text.Regex.Posix
import Data.List (elemIndex)
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Either as E
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
import qualified Data.Map as M
import qualified Data.Text.Lazy as LA

import AS.Types.Core
import AS.Util

deleteEmpty = filter ((/=) "")

skip :: Parser a -> Parser String
skip p = p >> (return "")

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
replaceSubstrings m (x:xs) = replaceSubstrings (LA.unpack scrubbed) ys
  where 
    (y:ys)=L.sortBy sortStrList (x:xs) 
    scrubbed = LA.replace (LA.pack (fst y)) (LA.pack (snd y)) (LA.pack m) 
    --replace all occurrences of fst x with snd x in m
    --keep doing this for all (str,str) in list
    --pack: string to text

sortStrList :: (String,String) -> (String,String) -> Ordering
sortStrList (a1, b1) (a2, b2)
  | (show $ dropWhile isUpper a1) < (show $ dropWhile isUpper a2) = GT
  | (show $ dropWhile isUpper a1) > (show $ dropWhile isUpper a2) = LT
  | otherwise = EQ

rangeDiff :: (Coord, Coord) -> Dimensions
rangeDiff (a,b) = (col b - col a + 1, row b - row a + 1)

reshapeColArr :: [a] -> Dimensions -> [[a]]
reshapeColArr lst@(x:xs) (m,n) = 
  if (length lst) /= (m*n-m)
    then (every m lst):(reshapeColArr xs (m,n))
    else []


getDelimitedSubstring :: String -> String -> Int -> String
getDelimitedSubstring str delim n = T.unpack $ (!!) (T.splitOn (T.pack delim) (T.pack str)) n

getLine :: String -> Int -> String
getLine str n = getDelimitedSubstring str "\n" n

stripString :: String -> String
stripString = T.unpack . T.strip . T.pack 

-- note: cannot take empty strings
tryParseList :: Parser a -> [String] -> [Maybe a]
tryParseList p ss = map readOutput parsed
  where
    parsed = map (parse p "") ss
    readOutput (Left err) = Nothing
    readOutput (Right res) = Just res

-- note: can take empty strinsg
tryParseListNonIso :: Parser a -> [String] -> [a]
tryParseListNonIso p ss = E.rights parsed
  where
    ss' = filter ((/=) "") ss
    parsed = map (parse p "") ss'

tryParse p s = parse p "" (T.pack s)

containsAny :: [String] -> String -> Bool
containsAny lst s = any (flip L.isInfixOf s) lst