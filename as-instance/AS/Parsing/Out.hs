module AS.Parsing.Out where

import Import hiding ((<|>))
import qualified Prelude as P
import Prelude ((!!), read)
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
toListStr lang lst  = end ++ (intercalate delim lst) ++ start
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
  ValueNaN () 		-> "Undefined"
  ValueS s 			-> show s
  ValueD d 			-> show d
  ValueL l 			-> toListStr lang $ fmap (showValue lang) l
  StyledValue s v 	-> showValue lang v
  DisplayValue d v 	-> showValue lang v
  ObjectValue o js 	-> jsonDeserialize lang o js
  otherwise -> "not cased on"

showFilteredValue :: ASLanguage -> ASValue -> String
showFilteredValue lang (ValueL l) = showFilteredValue lang (headOrNull l)
  where
    headOrNull [] = ValueNaN ()
    headOrNull (x:xs) = x
showFilteredValue lang v = showValue lang v


-- excel -----------------------------------------

-- regexStr = "([A-Z]+[0-9]+:[A-Z]+[0-9]+)"
-- regexStrIdx = "([A-Z]+[0-9]+)"
regexStrDollars = "(\\${0,1}+[A-Z]+\\${0,1}+[0-9]+:+\\${0,1}+[A-Z]+\\${0,1}+[0-9]+)"
regexStrDollarsIdx = "\\${0,1}+[A-Z]+\\${0,1}++[0-9]+"
regexStrSheet  ="[A-Za-z0-9_-]+\\!{0,1}"++regexStrDollars -- letters, numbers, dashes, underscores allowed in sheet name
regexStrSheetIdx = "[A-Za-z0-9_-]+\\!{0,1}"++regexStrDollarsIdx


-- takes in an ASExpression Range, row/col offset, parses expression, and gives list of parseDependencies, as well as new asexpression
-- parseDependencies = fst $ parseDependenciesRelative loc 0 0 
parseDependenciesRelative:: ASLocation -> ASExpression -> Int -> Int -> ([ASLocation],ASExpression)
parseDependenciesRelative loc xp rowOff colOff = (concat $ map (\m -> fromExcelRelativeLoc (sheet loc) m rowOff colOff) matches, newExp)
  where 
    matches = getMatches xp
    lang = language xp
    xp' = replaceSubstrings (expression xp) (zip matches (map (\m -> fromExcelRelativeString m rowOff colOff) matches ))
    newExp = Expression xp' lang

getMatches :: ASExpression -> [String]
getMatches xp = matches
  where
    sheetRangeMatches = deleteEmpty $ regexList (expression xp) regexStrSheet
    sheetIdxMatches = deleteEmpty $ regexList noSheetRangeExpr regexStrSheetIdx
      where 
        noSheetRangeExpr = replaceSubstrings (expression xp) (zip sheetRangeMatches (repeat ""))
    noSheets = replaceSubstrings (expression xp) (zip (sheetRangeMatches ++ sheetIdxMatches) (repeat ""))
    rangeMatches = deleteEmpty $ regexList noSheets regexStrDollars
    cellMatches = deleteEmpty $ regexList noRangeExpr regexStrDollarsIdx
      where
        noRangeExpr = replaceSubstrings noSheets (zip rangeMatches (repeat ""))  
    matches = rangeMatches ++ cellMatches ++ sheetRangeMatches ++ sheetIdxMatches

-- only works for A-Z, needs changing
fromExcelRelativeString :: String -> Int -> Int -> String
fromExcelRelativeString str row col 
  | elem '!' str =  (fst (spt str "!")) ++ "!" ++ (fromExcelRelativeString (snd (spt str "!")) row col)
  | elem ':' str =  (fromExcelRelativeString (fst (spt str ":")) row col) ++ ":" ++ (fromExcelRelativeString (snd (spt str ":")) row col)
  | otherwise = func str row col
    where  
      func ('$':letter:'$':num) row col = str --letter:num 
      func ('$':letter:num) row col = '$':(letter:(show ((P.read num::Int)+col))) 
      func (letter:'$':num:"") row col = (['A'..'Z']!!((toDigit letter)+row-1)):("$"++(num:""))  
      func (letter:num) row col = (['A'..'Z']!!((toDigit letter)+row-1)):(show ((P.read num::Int)+col))
      
-- splits a string at a character; assumes that only one instance of character
spt :: String -> String -> (String,String)
spt str char = (P.head split, P.last split)
  where 
    split = map T.unpack $ T.splitOn (T.pack char) (T.pack str) 

-- ignore sheet name parameter if Sheet1!A1, otherwise, include it
-- takes in string ("$A$1", "A2:A5"), offset row, offset col, and gives a list of the correct ASLocations that they correspond to (relative references)
fromExcelRelativeLoc :: String -> String -> Int -> Int -> [ASLocation]
fromExcelRelativeLoc sheet str row col 
  | elem '!' str = fromExcelRelativeLoc sheetName (snd (spt str "!")) row col 
  | elem ':' str = decomposeLocs $ Range sheet (first,second)
  | otherwise = [func str row col] 
    where 
      func ('$':letter:'$':num) row col = Index sheet ((toDigit letter),(P.read num::Int))
      func ('$':letter:num) row col = Index sheet ((toDigit letter),(P.read num::Int)+col)
      func (letter:'$':num) row col = Index sheet ((toDigit letter)+row,(P.read num::Int))
      func (letter:num) row col = Index sheet ((toDigit letter)+row,(P.read num::Int)+col)
      first = Ty.index ((fromExcelRelativeLoc sheet (fst (spt str ":")) row col)!!0)
      second = Ty.index ((fromExcelRelativeLoc sheet (snd (spt str ":")) row col)!!0)
      sheetName = fst (spt str "!")

-- depracated, used for sql?
toExcel :: ASLocation -> String
toExcel loc = case loc of 
  (Index sheet a) -> indexToExcel a
  (Range sheet a) -> (indexToExcel (fst a)) ++ ":" ++ (indexToExcel (snd a))

indexToExcel :: (Int, Int) -> String
indexToExcel idx = (['A'..'Z'] !! ((fst idx) - 1)):(show (snd idx))

excelToIndex :: String -> (Int, Int)
excelToIndex str = (toDigit (P.head str), P.read (P.tail str) :: Int)

-- THIS NEEDS TO EVENTUALLY CHANGE TO SUPPORT AA,AB etc, also true in other places
toDigit :: Char -> Int
toDigit x = fromJust (elemIndex x ['A'..'Z']) + 1


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
              first = P.head spt

excelRngToIdxs :: ASLanguage -> String -> String
excelRngToIdxs lang rng
  | x1 /= x2 = toListStr lang $ map toListStr' [[x:(show y) | x<-[x1..x2]] | y<-[y1..y2]] 
  | otherwise = toListStr' [x:(show y) | x<-[x1..x2], y<-[y1..y2]]
    where
      spt = map unpack $ T.splitOn (pack ":") (pack rng) 
      x1 = (P.head (spt !! 0))
      x2 = (P.head (spt !! 1))
      y1 = (read (P.tail (spt !! 0))::Int)
      y2 = (read (P.tail (spt !! 1))::Int)
      toListStr' = toListStr lang

excelRangesToLists :: ASLanguage -> String -> String
excelRangesToLists lang str = replaceSubstrings str (zip toReplace replaceWith)
  where
    toReplace = deleteEmpty $ regexList str regexStrDollars
    replaceWith = case lang of 
                    Python -> map ((\x->"arr("++x++")") . excelRngToIdxs lang) toReplace
                    otherwise -> map (excelRngToIdxs lang) toReplace

getExcelMatches :: String -> [String]
getExcelMatches xp = deleteEmpty $ regexList xp regexStrDollars

unpackExcelLocs :: ASValue -> [(Int,Int)] -- unpackExcelLocs x = [(1,2),(2,2),(1,3),(2,3)]
unpackExcelLocs (ValueL locs) = map (tup.format.lst) locs -- d=[ValueD a, ValueD b]
    where format= map (floor.dbl) -- format :: [ASValue] -> [Int]
          tup = \ints -> (ints!!0, ints!!1) -- tup :: [Int]-> (Int,Int)

unpackExcelExprs :: ASValue -> [String]
unpackExcelExprs (ValueL lst) = map str lst
unpackExcelExprs v = []

unpackExcelVals :: ASValue -> [ASValue]
unpackExcelVals (ValueL lst) = lst
unpackExcelVals v = []