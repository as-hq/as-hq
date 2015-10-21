module AS.Parsing.Out where

import Prelude
import Data.List (elemIndex)
import Data.Maybe
import Data.Char as C
import qualified Data.Text as T
import qualified Data.List as L
import Text.ParserCombinators.Parsec
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
  otherwise -> str

getBlockDelim :: ASLanguage -> String
getBlockDelim lang = case lang of
  R     -> ""
  Python-> ""
  OCaml -> ";;"
  SQL   -> ""
  Excel -> ""

getInlineDelim :: ASLanguage -> String
getInlineDelim lang = case lang of
  R     -> ";"
  Python-> ";"
  OCaml -> ";;"
  SQL   -> ";"
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

bool :: ASLanguage -> Bool -> String
bool lang b = case lang of
  Python-> show b
  R     -> map C.toUpper $ show b
  OCaml -> (\str -> (C.toLower (head str)):(tail str)) $ show b
  SQL   -> show b
  Excel -> show b

showValue :: ASLanguage -> ASValue -> String
showValue lang v = case v of
  ValueS s        -> show s
  ValueI i        -> show i
  ValueD d        -> show d
  ValueB b        -> bool lang b
  ValueL l        -> toListStr lang $ fmap (showValue lang) l
  ValueObject o js-> jsonDeserialize lang o js
  RList vals      -> showRList lang vals
  RDataFrame vals -> showRDataFrame lang vals

showRList :: ASLanguage -> [(RListKey, ASValue)] -> String
showRList lang l = case lang of
  R -> "list(" ++ (concat $ L.intersperse "," $ map showRPair l) ++ ")"

showRPair :: (String, ASValue) -> String
showRPair (key, val) = case key of
  "" -> showValue R val
  _ -> key ++ "=" ++ (showValue R val)

showRDataFrame :: ASLanguage -> [ASValue] -> String
showRDataFrame lang vals = case lang of
  R -> "data.frame(" ++ (concat $ L.intersperse "," fields) ++ ")"
    where fields = map showRPair $ splitNamesFromDataFrameValues vals

splitNamesFromDataFrameValues :: [ASValue] -> [(String, ASValue)]
splitNamesFromDataFrameValues vals = pairs
  where
    pairs = if (all isString names)
      then zip (map str names) (map (\(ValueL l) -> ValueL $ tail l) vals)
      else zip (repeat ("" :: String)) vals
    names = map (\(ValueL l) -> head l) vals


-------------------------------------------------------------------------------------------------------------------------------------------------
-- General parsing functions

-- P.parse (parseNext P.digit) "" "abc123" == Right ("abc",'1')
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
-- | needs better documentation. What are the arguments and what's getting returned? (-Alex, 10/8)
parseMatchesWithContext :: Parser t -> Parser ([String],[t])
parseMatchesWithContext a = do
  matchesWithContext <- many $ try $ parseNext a
  rest <- many anyChar
  let inter = (map fst matchesWithContext) ++ [rest]
      matches = (map snd matchesWithContext)
  return (inter,matches)

-- | needs better documentation. What are the arguments and what's getting returned? (-Alex, 10/8)
getMatchesWithContext :: String -> Parser t -> ([String],[t])
getMatchesWithContext target p = fromRight . (parse (parseMatchesWithContext p) "" ) $ target
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

asRefToAsIndex :: ASReference -> ASIndex
asRefToAsIndex loc = case loc of
  IndexRef i -> i

-- | Turns an Excel reference to an AlphaSheets reference. (first arg is the sheet of the
-- ref, unless it's a part of the ExRef)
exRefToASRef :: ASSheetId -> ExRef -> ASReference
exRefToASRef sheetid exRef = case exRef of
  ExLocOrRangeRef (ExLoc1 (ExIndex dol1 c dol2 r)) -> IndexRef $ Index sheetid (colStrToInt c, read r :: Int)
  ExLocOrRangeRef (ExLoc1 ExOutOfBounds) -> IndexRef OutOfBounds
  ExLocOrRangeRef (ExRange1 (ExRange f s)) -> RangeRef $ Range sheetid ((index . asRefToAsIndex) (exRefToASRef sheetid $ ExLocOrRangeRef $ ExLoc1 $ f), (index . asRefToAsIndex) (exRefToASRef sheetid $ ExLocOrRangeRef $ ExLoc1 $ s))
  ExSheetLocOrRangeRef sh rest -> case (exRefToASRef sheetid (ExLocOrRangeRef rest)) of
    IndexRef (Index _ a) -> IndexRef $ Index (T.pack sh) a
    RangeRef (Range _ a) -> RangeRef $ Range (T.pack sh) a

-- does not consider sheetid
asRefToExRef :: ASReference -> ExRef
asRefToExRef (IndexRef OutOfBounds) = ExLocOrRangeRef $ ExLoc1 ExOutOfBounds
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

outOfBoundsMatch :: Parser ExRef
outOfBoundsMatch = do 
  string "#REF!"
  return $ ExLocOrRangeRef $ ExLoc1 $ ExOutOfBounds

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
excelMatch = (try sheetRefMatch) <|> (try rangeMatch) <|> (try indexMatch) <|> (try outOfBoundsMatch)

------------------------------------------------------------------------------------------------------------------------------------------------
-- Helper Functions

-- takes an excel location and an offset, and produces the new excel location (using relative range syntax)
-- ex. ExIndex $A3 (1,1) -> ExIndex $A4
-- doesn't do any work with Parsec/actual parsing
shiftExRef :: (Int,Int) -> ExRef -> ExRef
shiftExRef offset exRef = case exRef of
  ExLocOrRangeRef (ExLoc1 (ExIndex dol1 c dol2 r)) -> ExLocOrRangeRef $ ExLoc1 $ ind
    where
      cVal = colStrToInt c
      rVal = (read r :: Int)
      newColVal = cVal + (if (dol1 == "$") then 0 else (fst offset))
      newRowVal = rVal + (if (dol2 == "$") then 0 else (snd offset))
      ind = if (newColVal >= 1 && newRowVal >= 1) 
        then (ExIndex dol1 (intToColStr newColVal) dol2 (show newRowVal)) 
        else (ExOutOfBounds)
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

-- | Returns the list of dependencies in ASExpression, and an expression with all the excel references replaced
getDependencies :: ASSheetId -> ASExpression -> [ASReference]
getDependencies sheetid xp = deps
  where
    origString = expression xp
    (_, exRefs) = getMatchesWithContext origString excelMatch -- the only place that Parsec is used
    deps = map (exRefToASRef sheetid) exRefs

---- | Takes in a list of ExRef's and converts them to a list of ASIndex's.
getASIndicesFromExRefs :: ASSheetId -> [ExRef] -> [ASIndex]
getASIndicesFromExRefs sheetid matches = concat $ map refToIndices $ map (exRefToASRef sheetid) matches

----------------------------------------------------------------------------------------------------------------------------------
-- Functions for excel sheet loading

unpackExcelLocs :: ASValue -> [(Int,Int)]
unpackExcelLocs (ValueL locs) = map (tup . format . toList) locs -- d=[ValueD a, ValueD b]
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

-- | Takes in an offset and a cell, and returns the cell you get when you shift the cell by
-- the offset. (The location changes, and the non-absolute references in the expression changes.)
shiftCell :: (Int, Int) -> ASCell -> ASCell
shiftCell offset (Cell loc (Expression str lang) v ts) = shiftedCell
  where
    shiftedLoc     = shiftInd offset loc
    (inter,exRefs) = getMatchesWithContext str excelMatch
    shiftedExRefs  = shiftExRefs offset exRefs
    newStr         = replaceMatches (inter, shiftedExRefs) showExcelRef str
    shiftedXp      = Expression newStr lang
    shiftedCell    = Cell shiftedLoc shiftedXp v ts
