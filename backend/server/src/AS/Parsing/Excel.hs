module AS.Parsing.Excel where

import Prelude()
import AS.Prelude

import Data.List (elemIndex)
import Data.Maybe
import Data.Char as C
import qualified Data.Text as T
import qualified Data.List as L
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
import Control.Monad ((>=>))
import Control.Lens hiding (noneOf)
import Control.Lens.TH
import qualified Data.Map as M
import qualified Data.Text.Lazy (replace)

import AS.Types.Cell
import AS.Types.Excel
import AS.Util

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Parsers to match special excel characters

readRefType :: Maybe Char -> RefType
readRefType d1 = case d1 of
  Nothing -> REL
  Just _ -> ABS

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
nameMatch = many ( noneOf ['!','$','@',':',' ']) >>= (\q -> exc >> return (rdName q))
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


--matches $A type things.
colMatch :: Parser ExCol
colMatch = do
  dol  <- optionMaybe dollar
  col <- many1 letter
  return $ ExInt (readRefType dol) $ colStrToCol col

--matches $1 type things.
rowMatch :: Parser ExRow
rowMatch = do
  dol  <- optionMaybe dollar
  row <- many1 digit
  return $ ExInt (readRefType dol) $ rowStrToRow row

-- | matches $AB15 type things
indexMatch :: Parser ExIndex
indexMatch = do
  xCol <- colMatch
  xRow <- rowMatch
  return $ ExIndex xCol xRow

outOfBoundsMatch :: Parser ExRef
outOfBoundsMatch = string "#REF!" >> return ExOutOfBounds
-- | Three cases for colRange matching

-- Matches A1:A type things.
colRangeA1ToAMatch :: Parser ExColRange
colRangeA1ToAMatch = do
  tl <- indexMatch
  colon
  r <- colMatch
  return $ ExColRange tl r

-- Matches A:A1 type things.
colRangeAToA1Match :: Parser ExColRange
colRangeAToA1Match = do
  r <- colMatch
  colon
  tl <- indexMatch
  return $ ExColRange tl r

-- Parses A:A as A$1:A
colRangeAToAMatch :: Parser ExColRange
colRangeAToAMatch = do
  l <- colMatch
  colon
  r <- colMatch
  return $ ExColRange (ExIndex l (ExInt ABS $ Row 1))  r

-- checks for matches to both both A:A and A1:A.
colRangeMatch :: Parser ExColRange
colRangeMatch = do
  -- order matters. AToA must be tried after AToA1
  colrngAToA1 <- optionMaybe $ try colRangeAToA1Match
  colrngA1ToA <- optionMaybe $ try colRangeA1ToAMatch
  case colrngAToA1 of
    Just a -> return a
    Nothing -> case colrngA1ToA of
           Just a -> return a
           Nothing -> colRangeAToAMatch

-- | matches index:index
rangeMatch :: Parser ExRange
rangeMatch = do
  tl <- indexMatch
  colon
  br <- indexMatch
  return $ ExRange tl br

refMatch :: Parser ExRef
refMatch = do
  point <- optionMaybe $ try pointer
  (sh, wb) <- option (Nothing, Nothing) $ try sheetWorkbookMatch
  rng <- optionMaybe $ try rangeMatch 
  colrng <- optionMaybe $ try colRangeMatch
  idx <- optionMaybe $ try indexMatch
  ofb <- optionMaybe $ try outOfBoundsMatch
  case point of 
    Just _ -> case idx of 
      Just idx' -> return $ ExPointerRef idx' sh wb
      Nothing -> case ofb of 
        Just ofb' -> return ExOutOfBounds
        Nothing -> fail "expected index reference when using pointer syntax"
    Nothing -> case rng of 
      Just rng' -> return $ ExRangeRef rng' sh wb
      Nothing -> case colrng of
        Just colrng' -> return $ ExColRangeRef colrng' sh wb
        Nothing -> case idx of 
          Just idx' -> return $ ExIndexRef idx' sh wb
          Nothing -> case ofb of  
            Just ofb' -> return ExOutOfBounds
            Nothing -> fail "expected valid excel A1:B4 format reference"

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
