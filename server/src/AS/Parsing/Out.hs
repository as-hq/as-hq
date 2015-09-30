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

import AS.Types.Core
import AS.Types.Excel
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

-- ::ALEX:: refactor
asLocationToAsIndex :: ASReference -> ASLocation 
asLocationToAsIndex loc = case loc of 
  IndexRef i -> i

-- excel location to list of as indexes
exRefToASRef :: ASSheetId -> ExRef -> ASReference
exRefToASRef sheetid exRef = case exRef of 
  ExLocOrRangeRef (ExLoc1 (ExIndex dol1 c dol2 r)) -> IndexRef $ Index sheetid (colStrToInt c, read r :: Int)
  ExLocOrRangeRef (ExRange1 (ExRange f s)) -> RangeRef $ Range sheetid ((index . asLocationToAsIndex) (exRefToASRef sheetid $ ExLocOrRangeRef $ ExLoc1 $ f), (index . asLocationToAsIndex) (exRefToASRef sheetid $ ExLocOrRangeRef $ ExLoc1 $ s))
  ExSheetLocOrRangeRef sh rest -> case (exRefToASRef sheetid (ExLocOrRangeRef rest)) of 
    IndexRef (Index _ a) -> IndexRef $ Index (T.pack sh) a
    RangeRef (Range _ a) -> RangeRef $ Range (T.pack sh) a

-- does not consider sheetid
asRefToExRef :: ASReference -> ExRef
asRefToExRef (IndexRef (Index _ (a,b))) = ExLocOrRangeRef $ ExLoc1 $ ExIndex "" (intToColStr a) "" (intToColStr b)
asRefToExRef (RangeRef (Range s (i1, i2))) = ExLocOrRangeRef $ ExRange1 $ ExRange i1' i2'
  where
    ExLocOrRangeRef (ExLoc1 i1') = asRefToExRef $ IndexRef $ Index s i1
    ExLocOrRangeRef (ExLoc1 i2') = asRefToExRef $ IndexRef $ Index s i2
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

-- | matches $AB15 type things
indexMatch :: Parser ExRef
indexMatch = do
  a <- dollar
  col <- many1 letter 
  b <- dollar
  row <- many1 digit
  return $ ExLocOrRangeRef $ ExLoc1 $ ExIndex a col b row

-- | matches index:index
rangeMatch :: Parser ExRef
rangeMatch = do 
  ExLocOrRangeRef (ExLoc1 topLeft) <- indexMatch 
  colon
  ExLocOrRangeRef (ExLoc1 bottomRight) <- indexMatch
  return $ ExLocOrRangeRef $ ExRange1 $ ExRange topLeft bottomRight

-- | matches sheet reference, e.g., Sheet1!$A$11
sheetRefMatch :: Parser ExRef
sheetRefMatch = do 
  name <- sheetMatch 
  exc
  ExLocOrRangeRef lor <- (try rangeMatch) <|> indexMatch -- order matters
  return $ ExSheetLocOrRangeRef name lor

excelMatch :: Parser ExRef
excelMatch = (try sheetRefMatch) <|> (try rangeMatch) <|> (try indexMatch)
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
shiftExRef :: (Int,Int) -> ExRef -> ExRef
shiftExRef offset exRef = case exRef of
  ExLocOrRangeRef (ExLoc1 (ExIndex dol1 c dol2 r)) -> ExLocOrRangeRef $ ExLoc1 $ ExIndex dol1 newCol dol2 newRow
    where
      newCol = case dol1 of
        "$" -> c --fixed
        ""  -> intToColStr $ (colStrToInt c) + (fst offset) --relative
      newRow = case dol2 of
        "$" -> r --fixed
        ""  -> show $ (read r :: Int) + (snd offset) --relative
  ExLocOrRangeRef (ExRange1 (ExRange a b)) -> ExLocOrRangeRef $ ExRange1 $ ExRange a' b' 
      where 
        ExLocOrRangeRef (ExLoc1 a') = shiftExRef offset (ExLocOrRangeRef $ ExLoc1 $ a)
        ExLocOrRangeRef (ExLoc1 b') = shiftExRef offset (ExLocOrRangeRef $ ExLoc1 $ b)
  ExSheetLocOrRangeRef sh rest -> ExSheetLocOrRangeRef sh rest' 
      where 
        ExLocOrRangeRef rest' = shiftExRef offset (ExLocOrRangeRef rest)

shiftExRefs :: (Int,Int) -> [ExRef] -> [ExRef]
shiftExRefs offset exRefs = map (shiftExRef offset) exRefs

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Parse dependencies and replace relative expressions

-- | Returns the list of dependencies you get, and an expression with all the excel references replaced
getDependenciesAndExpressions :: ASSheetId -> ASExpression -> ([ASLocation], ASExpression)
getDependenciesAndExpressions sheetid xp = (newLocs, newExpr)
  where 
    origString = expression xp
    (inter, exRefs) = getMatchesWithContext origString excelMatch -- the only place that Parsec is used
    newLocs = getDependenciesFromExRefs sheetid exRefs
    newString = replaceMatches (inter, exRefs) showExcelRef origString
    newExpr = Expression newString (language xp)

-- gets dependencies from a list of excel locs
getDependenciesFromExRefs :: ASSheetId -> [ExRef] -> [ASLocation]
getDependenciesFromExRefs sheetid matches = concat $ map refToIndices $ map (exRefToASRef sheetid) matches

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

----------------------------------------------------------------------------------------------------------------------------------
-- Copy/paste

shiftCell :: (Int, Int) -> ASCell -> (ASCell, [ASLocation])
shiftCell offset (Cell loc (Expression str lang) v ts) = (shiftedCell, shiftedDeps)
  where
    sheetid = locSheetId loc
    shiftedLoc = shiftInd offset loc
    (inter,exRefs) = getMatchesWithContext str excelMatch
    shiftedExRefs = shiftExRefs offset exRefs
    shiftedDeps = getDependenciesFromExRefs sheetid shiftedExRefs
    newStr = replaceMatches (inter, shiftedExRefs) showExcelRef str
    shiftedXp = Expression newStr lang
    shiftedCell = Cell shiftedLoc shiftedXp v ts
shiftCell offset (Cell loc (Reference _ _) v ts) = (shiftedCell, []) -- for copying sublists
  where
    pseudoXp = Expression (showValue Python v) Python -- TODO get the damn language
    shiftedLoc = shiftInd offset loc 
    shiftedCell = Cell shiftedLoc pseudoXp v ts

-- shiftCellExpr :: (Int, Int) -> ASExpression -> 