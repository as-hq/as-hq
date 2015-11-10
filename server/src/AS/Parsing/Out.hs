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
  NoValue            -> showNull lang
  ValueS s           -> show s
  ValueI i           -> show i
  ValueD d           -> show d
  ValueB b           -> bool lang b
  ValueL l           -> toListStr lang $ fmap (showValue lang) l
  ValueObject _ o js -> jsonDeserialize lang o js
  RList vals         -> showRList lang vals
  RDataFrame vals    -> showRDataFrame lang vals
  _ -> error ("In showValue, failed to pattern match: " ++ (show v))

showNull :: ASLanguage -> String
showNull lang = case lang of 
  Python -> "None"
  SQL -> "None"
  R -> "NULL"

showRList :: ASLanguage -> [(RListKey, ASValue)] -> String
showRList lang l = case lang of
  R -> "list(" ++ (concat $ L.intersperse "," $ map showRPair l) ++ ")"

showRPair :: (RListKey, ASValue) -> String
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
-- Type for parsing Excel Locations
             -- d1,d2 = "" or "$"

-- | Turns an Excel reference to an AlphaSheets reference. (first arg is the sheet of the
-- ref, unless it's a part of the ExRef)
exRefToASRef :: ASSheetId -> ExRef -> ASReference
exRefToASRef sid exRef = case exRef of
  ExLocRef (ExIndex _ c r) _ _ -> IndexRef $ Index sid (colStrToInt c, read r :: Int)
  ExLocRef ExOutOfBounds _ _ -> IndexRef OutOfBounds
  ExRangeRef (ExRange f s) _ _ -> RangeRef $ Range sid (tl, br)
    where
      IndexRef (Index _ tl) = exRefToASRef sid $ ExLocRef f Nothing Nothing
      IndexRef (Index _ br) = exRefToASRef sid $ ExLocRef s Nothing Nothing

-- does not consider sheetid TODO 
-- #anand we actually need to convert asrefs to proper exRefs
-- i.e. this function will need to do a DB lookup with the sheetId to get the sheetName
asRefToExRef :: ASReference -> ExRef
asRefToExRef (IndexRef OutOfBounds) = ExLocRef ExOutOfBounds Nothing Nothing
asRefToExRef (IndexRef (Index _ (a,b))) = ExLocRef idx Nothing Nothing
  where idx = ExIndex REL_REL (intToColStr a) (intToColStr b)
asRefToExRef (RangeRef (Range s (i1, i2))) = ExRangeRef rng Nothing Nothing
  where
    ExLocRef i1' _ _ = asRefToExRef $ IndexRef $ Index s i1
    ExLocRef i2' _ _ = asRefToExRef $ IndexRef $ Index s i2
    rng = ExRange i1' i2'
-------------------------------------------------------------------------------------------------------------------------------------------------
-- Parsers to match special excel characters

readRefType :: Maybe Char -> Maybe Char -> RefType 
readRefType d1 d2 = case d1 of
  Nothing -> case d2 of 
    Nothing -> REL_REL
    Just _ -> REL_ABS
  Just _ -> case d2 of 
    Nothing -> ABS_REL
    Just _ -> ABS_ABS

dollar :: Parser Char
dollar = char  '$' -- returns $ or ""; $ is not required for index

colon :: Parser Char
colon = char ':' -- this character is necessary for range

exc :: Parser Char
exc = char '!' -- required for sheet access

pointer :: Parser Char
pointer = char  '@'

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Parsers to match excel locations in strings

-- matches a valid sheet name
nameMatch :: Parser (Maybe String)
nameMatch = (many $ noneOf ['!','$','@',':',' ']) >>= (\q -> exc >> return (rdName q))
  where 
    rdName "" = Nothing
    rdName s = Just s

sheetWorkbookMatch :: Parser (Maybe SheetName, Maybe WorkbookName)
sheetWorkbookMatch = do
  q1 <- option Nothing $ try nameMatch
  case q1 of 
    Nothing -> return (Nothing, Nothing)
    Just _ -> do
      q2 <- option Nothing $ try nameMatch
      return $ case q2 of 
        Nothing -> (q1, Nothing) -- sheet, nothing
        Just _ -> (q2, q1)       -- sheet is inner-most parsed (it's q2), so return the reverse order

-- | matches $AB15 type things
indexMatch :: Parser ExLoc
indexMatch = do
  a <- optionMaybe dollar
  col <- many1 letter
  b <- optionMaybe dollar
  row <- many1 digit
  return $ ExIndex (readRefType a b) col row

outOfBoundsMatch :: Parser ExLoc
outOfBoundsMatch = string "#REF!" >> return ExOutOfBounds

-- | matches index:index
rangeMatch :: Parser ExRange
rangeMatch = do
  tl <- indexMatch
  colon
  br <- indexMatch
  return $ ExRange tl br

refMatch :: Parser ExRef
refMatch = do
  (sh, wb) <- option (Nothing, Nothing) $ try sheetWorkbookMatch
  point <- optionMaybe $ try pointer
  rng <- optionMaybe $ try rangeMatch 
  idx <- optionMaybe $ try indexMatch
  ofb <- optionMaybe $ try outOfBoundsMatch
  case point of 
    Just _ -> case idx of 
      Just idx' -> return $ ExPointerRef idx' sh wb
      Nothing -> case ofb of 
        Just ofb' -> return $ ExLocRef ofb' sh wb
        Nothing -> fail "expected index reference when using pointer syntax"
    Nothing -> case rng of 
      Just rng' -> return $ ExRangeRef rng' sh wb
      Nothing -> case idx of 
        Just idx' -> return $ ExLocRef idx' sh wb
        Nothing -> case ofb of  
          Just ofb' -> return $ ExLocRef ofb' sh wb
          Nothing -> fail "expected valid excel A1:B4 style reference"


------------------------------------------------------------------------------------------------------------------------------------------------
-- Helper Functions

-- takes an excel location and an offset, and produces the new excel location (using relative range syntax)
-- ex. ExIndex $A3 (1,1) -> ExIndex $A4
-- doesn't do any work with Parsec/actual parsing
shiftExRef :: Offset -> ExRef -> ExRef
shiftExRef (dC, dR) exRef = case exRef of
  ExLocRef ExOutOfBounds _ _ -> exRef
  ExLocRef (ExIndex dType c r) _ _ -> exRef { exLoc = idx }
    where
      newColVal = shiftCol dC dType c
      newRowVal = shiftRow dR dType r
      idx = if (newColVal >= 1 && newRowVal >= 1) 
        then ExIndex dType (intToColStr newColVal) (show newRowVal) 
        else ExOutOfBounds
  ExRangeRef (ExRange f s) sh wb -> exRef { exRange = ExRange f' s' }
      where
        ExLocRef f' _ _ = shiftExRef (dC, dR) (ExLocRef f sh wb)
        ExLocRef s' _ _ = shiftExRef (dC, dR) (ExLocRef s sh wb)
  ExPointerRef l sh wb -> exRef { pointerLoc = l' }
      where ExLocRef l' _ _ = shiftExRef (dC, dR) (ExLocRef l sh wb)

shiftExRefs :: Offset -> [ExRef] -> [ExRef]
shiftExRefs offset exRefs = map (shiftExRef offset) exRefs

shiftCol :: Int -> RefType -> String -> Int
shiftCol dC rType c = newCVal
  where
    cVal = colStrToInt c
    newCVal = cVal + (case rType of
      ABS_ABS -> 0
      ABS_REL -> 0
      REL_ABS -> dC
      REL_REL -> dC )

shiftRow :: Int -> RefType -> String -> Int
shiftRow dR rType r = newRVal 
  where
    rVal = (read r :: Int)
    newRVal = rVal + (case rType of 
      ABS_ABS -> 0
      ABS_REL -> dR
      REL_ABS -> 0
      REL_REL -> dR )
----------------------------------------------------------------------------------------------------------------------------------
-- Functions for excel sheet loading

-- DEPRECATED

--unpackExcelLocs :: ASValue -> [(Int,Int)]
--unpackExcelLocs (ValueL locs) = map (tup . format . toList) locs -- d=[ValueD a, ValueD b]
--    where format = map (floor.dbl) -- format :: [ASValue] -> [Int]
--          tup = \ints -> (ints!!0, ints!!1) -- tup :: [Int]-> (Int,Int)

--unpackExcelExprs :: ASValue -> [String]
--unpackExcelExprs (ValueL l) = map str l
--unpackExcelExprs v = []

--unpackExcelVals :: ASValue -> [ASValue]
--unpackExcelVals (ValueL l) = l
--unpackExcelVals v = []
