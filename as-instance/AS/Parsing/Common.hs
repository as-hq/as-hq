module AS.Parsing.Common where

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

(<++>) a b = (++) <$> a <*> b

(<:>) a b  = (:) <$> a <*> b

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
  (Index sheet a) -> [loc]
  (Range sheet (ul, lr)) -> [Index sheet (x,y) | x <- [(fst ul)..(fst lr)], y <- [(snd ul)..(snd lr)] ]

normalizeRanges :: [ASLocation] -> [ASLocation] 
normalizeRanges locs = do
  loc <- locs
  case loc of
    Range sheet (p1, p2) -> decomposeLocs loc
    Index _  _      -> return loc

getOffsets :: ASLocation -> [(Int,Int)]
getOffsets loc = case loc of 
    Index _ _ -> [(0,0)]
    Range _ ((a,b),(c,d)) -> [(x,y)|x<-[0..(c-a)], y<-[0..(d-b)]]

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


replaceAliases :: String -> [ASFunc] -> (String, [(String, String, String)])
replaceAliases cmd [] = (cmd, [])
replaceAliases cmd matches = 
  (replaceSubstrings cmd (map toReplacingImports presentStubs), 
  map (\f-> (unpack (aSFuncImportName f), unpack (aSFuncImportCommand f), unpack (aSFuncLocation f))) presentStubs)
    where 
      toReplacingImports = (\f->(unpack (aSFuncAlias f), unpack (aSFuncReplace f)))
      presentStubs = filter (\x -> isInfixOf (unpack (aSFuncAlias x)) cmd) matches