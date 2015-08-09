module AS.Parsing.Regex where

import Prelude
import AS.Types as Ty
import AS.Parsing.Common
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

toListStr :: ASLanguage -> [String] -> String
toListStr lang lst  = end ++ (L.intercalate delim lst) ++ start
  where
    (end, delim, start) = case lang of 
      R     -> ("c(", ",", ")")
      Python-> ("[", ",", "]")
      OCaml -> ("[", ";", "]")
      SQL   -> ("[", ",", "]")

getBlockDelim :: ASLanguage -> String
getBlockDelim lang = case lang of 
  R     -> ""
  Python-> ""
  OCaml -> ";;"
  SQL   -> ""

getInlineDelim :: ASLanguage -> String
getInlineDelim lang = case lang of 
  R     -> ";"
  Python-> ";"
  OCaml -> ";;"
  SQL   -> ";"

jsonDeserialize :: ASLanguage -> String -> String -> String
jsonDeserialize lang objType jsonRep = 
	let 
		dlm = getBlockDelim lang
	in case lang of 
    R       -> objType ++ "$(" ++ jsonRep ++ ")" ++ dlm
    Python  -> objType ++ ".deserialize(" ++ jsonRep ++ ")" ++ dlm
    OCaml   -> "Serialization# " ++ objType ++ " " ++ jsonRep ++ dlm
    SQL     -> objType ++ ".deserialize(" ++ jsonRep ++ ")" ++ dlm

showValue :: ASLanguage -> ASValue -> String
showValue lang v = case v of
  ValueImage path 	-> "PLOT"--ADDED, open file here?
  ValueNaN () 		-> "Undefined"
  ValueS s 			-> show s
  ValueD d 			-> show d
  ValueL l 			-> toListStr lang $ fmap (showValue lang) l
  StyledValue s v 	-> showValue lang v
  DisplayValue d v 	-> showValue lang v
  ObjectValue o js 	-> jsonDeserialize lang o js

showFilteredValue :: ASLanguage -> ASLocation -> ASValue -> String
showFilteredValue lang (Index sh i) (ValueL l) = showFilteredValue lang (Index sh i) (headOrNull l)
  where
    headOrNull [] = ValueNaN ()
    headOrNull (x:xs) = x
showFilteredValue lang _ a = showValue lang a 


-- excel -----------------------------------------

regexStr = "([A-Z]+[0-9]+:[A-Z]+[0-9]+)"
regexStrIdx = "([A-Z]+[0-9]+)"
regexStrDollars = "(\\${0,1}+[A-Z]+\\${0,1}+[0-9]+:+\\${0,1}+[A-Z]+\\${0,1}+[0-9]+)"
regexStrDollarsIdx = "\\${0,1}+[A-Z]+\\${0,1}++[0-9]+"

{-fromExcelRelativeString :: String -> Int -> Int -> String
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
      second = P.last spt -}

-- deletes all occurrences of $ from text, used for evalPy on relative references
deleteDollars :: String -> String
deleteDollars str = T.unpack $ T.concat $ T.splitOn (T.pack "$") (T.pack str)


toExcel :: ASLocation -> String
toExcel loc = case loc of 
  (Index sh a) -> indexToExcel a
  (Range sh a) -> (indexToExcel (fst a)) ++ ":" ++ (indexToExcel (snd a))

{-fromExcel :: String -> ASLocation
fromExcel str
  | elem ':' str = Range (excelToIndex $ P.head spt, excelToIndex $ P.last spt)
  | otherwise    = Index . excelToIndex $ str
    where
      spt = map unpack $ T.splitOn (pack ":") (pack str) -}

indexToExcel :: (Int, Int) -> String
indexToExcel idx = (['A'..'Z'] !! ((fst idx) - 1)):(show (snd idx))

excelToIndex :: String -> (Int, Int)
excelToIndex str = (toDigit (head str), read (tail str) :: Int)

-- THIS NEEDS TO EVENTUALLY CHANGE TO SUPPORT AA,AB etc, also true in other places
toDigit :: Char -> Int
toDigit x = fromJust (elemIndex x ['A'..'Z']) + 1

{-parseDependencies :: ASExpression -> [ASLocation]
parseDependencies expr =
  case expr of
    Expression e _ -> (map fromExcel rangeMatches) ++ (map fromExcel cellMatches)
      where
        rangeMatches = deleteEmpty $ regexList e regexStr
        cellMatches = deleteEmpty $ regexList noRangeExpr regexStrIdx
          where
            noRangeExpr = replaceSubstrings e (zip rangeMatches (repeat ""))
    Reference r _ -> [r]

-- takes in an ASExpression Range, row/col offset, parses expression, and gives list of parseDependencies, as well as new asexpression
parseDependenciesRelative:: ASExpression -> Int -> Int -> ([ASLocation],ASExpression)
parseDependenciesRelative xp rowOff colOff = (concat $ map (\m -> fromExcelRelativeLoc m rowOff colOff) matches, newExp)
  where 
    rangeMatches = deleteEmpty $ regexList (expression xp) regexStrDollars
    cellMatches = deleteEmpty $ regexList noRangeExpr regexStrDollarsIdx
      where
        noRangeExpr = replaceSubstrings (expression xp) (zip rangeMatches (repeat ""))  --get rid of range matches, then look for index matches
    matches = rangeMatches ++ cellMatches
    lang = language xp
    xp' = replaceSubstrings (expression xp) (zip matches (map (\m -> fromExcelRelativeString m rowOff colOff) matches ))
    newExp = Expression xp' lang


-- replaces any occurrences of A2:B4 with A2, keeps dollar signs
topLeft :: String-> String
topLeft str = tlString
  where 
    rangeMatches = deleteEmpty $ regexList str regexStrDollars
    tlString = replaceSubstrings str (zip rangeMatches (map tlFunc rangeMatches))
      where
        tlFunc m 
          |elem ':' m = first --this is the only case that will be used
          |otherwise = m
            where 
              spt = map unpack $ T.splitOn (pack ":") (pack m) 
              first = P.head spt -}


excelRngToIdxs :: ASLanguage -> String -> String
excelRngToIdxs lang rng
  | x1 /= x2 = toListStr lang $ map toListStr' [[x:(show y) | x<-[x1..x2]] | y<-[y1..y2]] 
  | otherwise = toListStr' [x:(show y) | x<-[x1..x2], y<-[y1..y2]]
    where
      spt = map T.unpack $ T.splitOn (T.pack ":") (T.pack rng) 
      x1 = (head (spt !! 0))
      x2 = (head (spt !! 1))
      y1 = (read (tail (spt !! 0))::Int)
      y2 = (read (tail (spt !! 1))::Int)
      toListStr' = toListStr lang

excelRangesToLists :: ASLanguage -> String -> String
excelRangesToLists lang str = replaceSubstrings str (zip toReplace replaceWith)
  where
    toReplace = deleteEmpty $ regexList str regexStr
    replaceWith = map (excelRngToIdxs lang) toReplace

excelRangesToIterables :: ASLanguage -> String -> String
excelRangesToIterables lang str = replaceSubstrings str (zip toReplace replaceWith)
  where
    toReplace = deleteEmpty $ regexList str regexStr
    replaceWith = map ((\x->"arr("++x++")") . excelRngToIdxs lang) toReplace

getExcelMatches :: String -> [String]
getExcelMatches xp = deleteEmpty $ regexList xp regexStr