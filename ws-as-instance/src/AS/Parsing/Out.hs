module AS.Parsing.Out where

import Prelude
import Text.Regex.Posix
import Data.List (elemIndex)
import Data.Maybe
import Data.Char as C
import qualified Data.Text as T
import qualified Data.List as L
import Text.Parsec
import Text.Parsec.Text
import Control.Applicative hiding ((<|>), many)
import qualified Data.Map as M
import qualified Data.Text.Lazy (replace)

import AS.Types as Ty
import AS.Parsing.Common
import AS.Util

toListStr :: ASLanguage -> [String] -> String
toListStr lang lst  = end ++ (L.intercalate delim lst) ++ start
  where
    (end, delim, start) = case lang of 
      R     -> ("c(", ",", ")")
      Python-> ("[", ",", "]")
      OCaml -> ("[", ";", "]")
      SQL   -> ("[", ",", "]")
      Excel -> ("[", ",", "]")

modifiedLists :: ASLanguage -> String -> String  
modifiedLists lang str = case lang of
  Python -> "arr(" ++ str ++ ")"
  Excel  -> "arr(" ++ str ++ ")"
  otherwise -> str

getBlockDelim :: ASLanguage -> String
getBlockDelim lang = case lang of 
  R     -> ""
  Python-> ""
  OCaml -> ";;"
  SQL   -> ""
  CPP   -> ""
  Java  -> ""
  Excel -> ""

getInlineDelim :: ASLanguage -> String
getInlineDelim lang = case lang of 
  R     -> ";"
  Python-> ";"
  OCaml -> ";;"
  SQL   -> ";"
  CPP   -> ";"
  Java  -> ";"
  Excel -> ";"

jsonDeserialize :: ASLanguage -> String -> String -> String
jsonDeserialize lang objType jsonRep = 
	let 
		dlm = getBlockDelim lang
	in case lang of 
    R       -> objType ++ "$(" ++ jsonRep ++ ")" ++ dlm
    Python  -> objType ++ ".deserialize(" ++ jsonRep ++ ")" ++ dlm
    OCaml   -> "Serialization# " ++ objType ++ " " ++ jsonRep ++ dlm
    SQL     -> objType ++ ".deserialize(" ++ jsonRep ++ ")" ++ dlm

bool :: ASLanguage -> String -> String
bool lang str = case lang of 
  Python -> str
  R -> ((C.toLower (head str)): (tail str))
  OCaml -> ((C.toLower (head str)): (tail str))
  SQL -> str
  CPP -> ((C.toLower (head str)): (tail str))
  Java -> ((C.toLower (head str)): (tail str))
  Excel -> str

showValue :: ASLanguage -> ASValue -> String
showValue lang v = case v of
  ValueNaN () 		-> "Undefined"
  ValueS s 			-> show s
  ValueI i      -> show i
  ValueD d 			-> show d
  ValueB b      -> bool lang $ show b
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

-------------------------------------------------------------------------------------------------------------------------------------------------
-- General parsing functions

-- P.parse (parseNext P.digit) "" "abc123"
-- Right ("abc",'1')
parseNext :: Parser t -> Parser (String, t)
parseNext a = do 
  r1 <- manyTill anyChar (lookAhead $ try a) -- need the try, otherwise it won't work
  r2 <- a -- result of parser a 
  return (r1,r2)

-- P.parse (parseMatches (P.string "12")) "" "1212ab12"
-- Right ["12","12","12"]
parseMatches :: Parser t -> Parser [t]
parseMatches a = many $ do
  (inter, next) <- try $ parseNext a --consumes input if it succeeds
  return next

-- P.parse (parseMatchesWithContext (P.string "12")) "" "1212ab12"
-- Right (["","","ab",""],["12","12","12"]) (alternating gives back str)
parseMatchesWithContext :: Parser t -> Parser ([String],[t])
parseMatchesWithContext a = do 
  matchesWithContext <- many $ try $ parseNext a 
  rest <- many anyChar
  let inter = (map fst matchesWithContext) ++ [rest]
      matches = (map snd matchesWithContext)
  return (inter,matches)

getMatchesWithContext :: String -> Parser t -> ([String],[t])
getMatchesWithContext target p = fromRight . (parse (parseMatchesWithContext p) "" ) . T.pack $ target
  where 
    fromRight (Right x) = x 

-- does no work with Parsec/actual parsing
replaceMatches :: ([String],[t]) -> (t -> String) -> String -> String
replaceMatches (inter,matches) f target = blend inter matchReplacings
  where
    blend [x] _ = x
    blend (x:xs) (y:ys) = x ++  y ++ blend xs ys
    matchReplacings = map f matches
-------------------------------------------------------------------------------------------------------------------------------------------------
-- Type for parsing Excel Locations
             -- d1,d2 = "" or "$"

exLocToString :: ExLoc -> String
exLocToString exLoc = case exLoc of
  ExSheet sheet rest -> sheet ++ "!" ++ (exLocToString rest)
  ExRange first second -> (exLocToString first) ++ ":" ++ (exLocToString second)
  ExIndex dol1 c dol2 r -> dol1 ++ c ++ dol2 ++ r

-- excel location to list of as indexes
exLocToASLocation :: ASLocation -> ExLoc -> ASLocation
exLocToASLocation loc exLoc = case exLoc of 
  ExSheet sh rest -> case (exLocToASLocation loc rest) of 
    Range _ a -> Range (locSheetId loc) a
    Index _ a -> Index (locSheetId loc) a
  ExRange f s -> Range (locSheetId loc) (index (exLocToASLocation loc f),index (exLocToASLocation loc s))
  ExIndex dol1 c dol2 r -> Index (locSheetId loc) (colStrToInt c, read r :: Int)
-------------------------------------------------------------------------------------------------------------------------------------------------
-- Parsers to match special excel characters

dollar :: Parser String
dollar = string "$" <|> string "" -- returns $ or ""; $ is not required for index

colon :: Parser String
colon = string ":" -- this character is necessary for range

exc :: Parser String
exc = string "!" -- required for sheet access

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Parsers to match excel locations in strings

-- matches a valid sheet name
sheetMatch :: Parser String
sheetMatch = many1 $ letter <|> digit <|> char '-' <|> char '_' <|> space

-- matches $AB15 type things
indexMatch :: Parser ExLoc
indexMatch = do
  a <- dollar
  col <- many1 letter 
  b <- dollar
  row <- many1 digit
  return $ ExIndex a col b row

-- matches index:index
rangeMatch :: Parser ExLoc
rangeMatch = do 
  topLeft <- indexMatch 
  colon
  bottomRight <- indexMatch
  return $ ExRange topLeft bottomRight

-- matches sheet reference; Sheet1!$A$11
sheetRefMatch :: Parser ExLoc
sheetRefMatch = do 
  name <- sheetMatch 
  exc
  loc <- (try rangeMatch) <|> indexMatch -- order matters
  return $ ExSheet name loc

excelMatch :: Parser ExLoc
excelMatch = (try sheetRefMatch) <|> (try rangeMatch) <|> indexMatch 
------------------------------------------------------------------------------------------------------------------------------------------------
-- Helper Functions

-- "AA" -> 27
colStrToInt :: String -> Int
colStrToInt "" = 0
colStrToInt (c:cs) = 26^(length(cs)) * coef + colStrToInt cs
  where
    coef = fromJust ( elemIndex (C.toUpper c) ['A'..'Z'] ) + 1

-- 27 -> "AA",  218332954 ->"RITESH"
intToColStr :: Int -> String
intToColStr x
  | x <= 26 = [['A'..'Z'] !! (x-1)]
  | otherwise = intToColStr d ++ [['A'..'Z'] !! m]
      where
        m = (x-1) `mod` 26
        d = (x-1) `div` 26

-- used in DB Ranges
indexToExcel :: (Int,Int) -> String
indexToExcel (c,r) = (intToColStr c) ++ (show r)

-- takes an excel location and an offset, and produces the new excel location (using relative range syntax)
-- ex. ExIndex $A3 (1,1) -> ExIndex $A4
-- doesn't do any work with Parsec/actual parsing
shiftExLoc :: (Int,Int) -> ExLoc -> ExLoc
shiftExLoc offset exLoc = case exLoc of
  ExSheet sh rest -> ExSheet sh (shiftExLoc offset rest)
  ExRange a b -> ExRange (shiftExLoc offset a) (shiftExLoc offset b)
  ExIndex dol1 c dol2 r -> ExIndex dol1 newCol dol2 newRow
    where
      newCol = case dol1 of
        "$" -> c --fixed
        ""  -> intToColStr $ (colStrToInt c) + (fst offset) --relative
      newRow = case dol2 of
        "$" -> r --fixed
        ""  -> show $ (read r :: Int) + (snd offset) --relative

shiftExLocs:: (Int,Int) -> [ExLoc] -> [ExLoc]
shiftExLocs offset exLocs = map (shiftExLoc offset) exLocs

-- return all dependencies for a particular excel location (takes in current location to deal with sheets)
-- ex. ExIndex A3 just returns [Index A3]
-- doesn't do any work with Parsec/actual parsing
dependenciesFromExceLLoc :: ASLocation -> ExLoc -> [ASLocation]
dependenciesFromExceLLoc loc exLoc = case exLoc of
  ExSheet sh rest -> [Index (locSheetId loc) (index dep) | dep <- dependenciesFromExceLLoc loc rest] --dependency locations are on other sheet
  ExRange a b -> decomposeLocs $ Range (locSheetId loc) ((toCol a, toRow a), (toCol b, toRow b)) -- any range has full dependency
    where
      toCol = colStrToInt.col
      toRow = read.row
  ExIndex dol1 c dol2 r -> [Index (locSheetId loc) ((colStrToInt c),(read r::Int))]

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Parse dependencies and replace relative expressions

getDependenciesAndExpressions :: ASLocation -> ASExpression -> [(Int,Int)] -> ([[ASLocation]],[ASExpression])
getDependenciesAndExpressions loc xp offsets = (newLocs,newExprs)
  where 
    origString = expression xp
    (inter,exLocs) = getMatchesWithContext origString excelMatch -- the only place that Parsec is used
    newLocs = getDependencies loc exLocs offsets 
    newStrings = [replaceMatches (inter, shiftExLocs off exLocs) exLocToString origString | off <- offsets]
    newExprs = map (\str -> Expression str (language xp)) newStrings

-- gets dependencies from a list of excel locs and a list of offsets (there's a [ASLocation] for each offset, in that order)
-- doesn't use Parsec/actual parsing
getDependencies :: ASLocation -> [ExLoc] -> [(Int,Int)] -> [[ASLocation]]
getDependencies loc matches offsets = [depsFromExcelLocs (shiftExLocs off matches) | off <- offsets]
  where
    depsFromExcelLocs m = normalizeRanges $ concat $ map (dependenciesFromExceLLoc loc) m

----------------------------------------------------------------------------------------------------------------------------------
-- Functions for excel sheet loading

unpackExcelLocs :: ASValue -> [(Int,Int)] 
unpackExcelLocs (ValueL locs) = map (tup.format.lst) locs -- d=[ValueD a, ValueD b]
    where format = map (floor.dbl) -- format :: [ASValue] -> [Int]
          tup = \ints -> (ints!!0, ints!!1) -- tup :: [Int]-> (Int,Int)

unpackExcelExprs :: ASValue -> [String]
unpackExcelExprs (ValueL l) = map str l
unpackExcelExprs v = []

unpackExcelVals :: ASValue -> [ASValue]
unpackExcelVals (ValueL l) = l
unpackExcelVals v = []