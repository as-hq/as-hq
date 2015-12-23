module AS.Parsing.Excel where

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

import AS.Types.Cell
import AS.Types.Excel
import AS.Util

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Parsers to match special excel characters

readSingleRef :: Maybe Char -> SingleRefType
readSingleRef d1 = case d1 of
  Nothing -> REL
  Just _ -> ABS

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

-- | matches $AB15 type things
indexMatch :: Parser ExLoc
indexMatch = do
  a <- optionMaybe dollar
  col <- many1 letter
  b <- optionMaybe dollar
  row <- many1 digit
  return $ ExIndex (readRefType a b) col row

outOfBoundsMatch :: Parser ExRef
outOfBoundsMatch = string "#REF!" >> return ExOutOfBounds

colRangeMatch :: Parser ExColRange
colRangeMatch = do
  tl <- indexMatch
  colon
  dol  <- optionMaybe dollar
  r <- many1 letter
  return $ ExColRange tl (ExCol (readSingleRef dol) r)

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
      Nothing -> case idx of 
        Just idx' -> return $ ExLocRef idx' sh wb
        Nothing -> case ofb of  
          Just ofb' -> return ExOutOfBounds
          Nothing -> fail "expected valid excel A1:B4 format reference"


------------------------------------------------------------------------------------------------------------------------------------------------
-- Helper Functions

-- takes an excel location and an offset, and produces the new excel location (using relative range syntax)
-- ex. ExIndex $A3 (1,1) -> ExIndex $A4
-- doesn't do any work with Parsec/actual parsing
shiftExRef :: Offset -> ExRef -> ExRef
shiftExRef o exRef = case exRef of
  ExOutOfBounds -> ExOutOfBounds
  ExLocRef (ExIndex dType c r) _ _ -> exRef' 
    where
      newColVal = shiftCol (dX o) dType c
      newRowVal = shiftRow (dY o) dType r
      idx = if (newColVal >= 1 && newRowVal >= 1) 
        then Just $ ExIndex dType (intToColStr newColVal) (show newRowVal) 
        else Nothing
      exRef' = maybe ExOutOfBounds (\i -> exRef { exLoc = i }) idx
  ExRangeRef (ExRange f s) sh wb -> exRef' 
      where
        shiftedInds = (shiftExRef o (ExLocRef f sh wb), shiftExRef o (ExLocRef s sh wb))
        exRef' = case shiftedInds of 
          (ExLocRef f' _ _, ExLocRef s' _ _) -> exRef { exRange = ExRange f' s' }
          _ -> ExOutOfBounds
  ExPointerRef l sh wb -> exRef { pointerLoc = l' }
      where ExLocRef l' _ _ = shiftExRef o (ExLocRef l sh wb)

-- shifts absolute references too
shiftExRefForced :: Offset -> ExRef -> ExRef
shiftExRefForced o exRef = case exRef of
  ExOutOfBounds -> ExOutOfBounds
  ExLocRef (ExIndex dType c r) _ _ -> exRef' 
    where
      newColVal = shiftCol (dX o) REL_REL c
      newRowVal = shiftRow (dY o) REL_REL r
      idx = if (newColVal >= 1 && newRowVal >= 1) 
        then Just $ ExIndex dType (intToColStr newColVal) (show newRowVal) 
        else Nothing
      exRef' = maybe ExOutOfBounds (\i -> exRef { exLoc = i }) idx
  ExRangeRef (ExRange f s) sh wb -> exRef' 
      where
        shiftedInds = (shiftExRefForced o (ExLocRef f sh wb), shiftExRefForced o (ExLocRef s sh wb))
        exRef' = case shiftedInds of 
          (ExLocRef f' _ _, ExLocRef s' _ _) -> exRef { exRange = ExRange f' s' }
          _ -> ExOutOfBounds
  ExPointerRef l sh wb -> exRef { pointerLoc = l' }
      where ExLocRef l' _ _ = shiftExRefForced o (ExLocRef l sh wb)

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
