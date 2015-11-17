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

import AS.Types.Core
import AS.Types.Excel
import AS.Util

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Type for parsing Excel Locations
             -- d1,d2 = "" or "$"

-- | Turns an Excel reference to an AlphaSheets reference. (first arg is the sheet of the
-- ref, unless it's a part of the ExRef)
exRefToASRef :: ASSheetId -> ExRef -> ASReference
exRefToASRef sid exRef = case exRef of
  ExOutOfBounds -> OutOfBounds
  ExLocRef (ExIndex _ c r) sn wn -> IndexRef $ Index sid' (colStrToInt c, read r :: Int)
    where sid' = maybe sid id (sheetIdFromContext sn wn)
  ExRangeRef (ExRange f s) sn wn -> RangeRef $ Range sid' (tl, br)
    where
      sid' = maybe sid id (sheetIdFromContext sn wn)
      IndexRef (Index _ tl) = exRefToASRef sid' $ ExLocRef f sn Nothing
      IndexRef (Index _ br) = exRefToASRef sid' $ ExLocRef s sn Nothing
  ExPointerRef (ExIndex _ c r) sn wn -> IndexRef $ Pointer sid' (colStrToInt c, read r :: Int)
    where sid' = maybe sid id (sheetIdFromContext sn wn)

asRefToExRef :: ASReference -> ExRef
asRefToExRef OutOfBounds = ExOutOfBounds
asRefToExRef (IndexRef (Index sid (a,b))) = ExLocRef idx sname Nothing
  where idx = ExIndex REL_REL (intToColStr a) (show b)
        sname = sheetIdToSheetName sid
asRefToExRef (IndexRef (Pointer sid (a,b))) = ExPointerRef idx sname Nothing
  where idx = ExIndex REL_REL (intToColStr a) (show b)
        sname = sheetIdToSheetName sid
asRefToExRef (RangeRef (Range s (i1, i2))) = ExRangeRef rng Nothing Nothing
  where
    ExLocRef i1' _ _ = asRefToExRef . IndexRef $ Index s i1
    ExLocRef i2' _ _ = asRefToExRef . IndexRef $ Index s i2
    rng = ExRange i1' i2'

-- #incomplete we should actually be looking in the db. For now, with the current UX of
-- equating sheet names and sheet id's with the dialog box, 
sheetIdToSheetName :: ASSheetId -> Maybe SheetName
sheetIdToSheetName = Just . T.unpack

-- #incomplete lol. just returns sheet name from sheet id for now. 
sheetIdFromContext :: Maybe SheetName -> Maybe WorkbookName -> Maybe ASSheetId
sheetIdFromContext (Just sn) _ = Just $ T.pack sn
sheetIdFromContext _ _ = Nothing

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

outOfBoundsMatch :: Parser ExRef
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
        Just ofb' -> return ExOutOfBounds
        Nothing -> fail "expected index reference when using pointer syntax"
    Nothing -> case rng of 
      Just rng' -> return $ ExRangeRef rng' sh wb
      Nothing -> case idx of 
        Just idx' -> return $ ExLocRef idx' sh wb
        Nothing -> case ofb of  
          Just ofb' -> return ExOutOfBounds
          Nothing -> fail "expected valid excel A1:B4 style reference"


------------------------------------------------------------------------------------------------------------------------------------------------
-- Helper Functions

-- takes an excel location and an offset, and produces the new excel location (using relative range syntax)
-- ex. ExIndex $A3 (1,1) -> ExIndex $A4
-- doesn't do any work with Parsec/actual parsing
shiftExRef :: Offset -> ExRef -> ExRef
shiftExRef (dC, dR) exRef = case exRef of
  ExOutOfBounds -> exRef
  ExLocRef (ExIndex dType c r) _ _ -> exRef' 
    where
      newColVal = shiftCol dC dType c
      newRowVal = shiftRow dR dType r
      idx = if (newColVal >= 1 && newRowVal >= 1) 
        then Just $ ExIndex dType (intToColStr newColVal) (show newRowVal) 
        else Nothing
      exRef' = maybe ExOutOfBounds (\i -> exRef { exLoc = i }) idx
  ExRangeRef (ExRange f s) sh wb -> exRef' 
      where
        shiftedInds = (shiftExRef (dC, dR) (ExLocRef f sh wb), shiftExRef (dC, dR) (ExLocRef s sh wb))
        exRef' = case shiftedInds of 
          (ExLocRef f' _ _, ExLocRef s' _ _) -> exRef { exRange = ExRange f' s' }
          _ -> ExOutOfBounds
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
