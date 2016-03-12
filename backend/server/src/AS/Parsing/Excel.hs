{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module AS.Parsing.Excel where

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Word8 as W
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as BU

import Prelude()
import AS.Prelude hiding (takeWhile)
import AS.Types.Cell
import AS.Types.Excel
import AS.Util

import qualified AS.Parsing.Common as PC

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Conversions between various Excel-related types

-- | Converts a ByteString representing an Excel column to a Col datatype. 
-- Example: AA -> 27.
colStrToCol :: ByteString -> Col
colStrToCol = Col . colStrToInt

-- Tail-recursive helper for colStrToCol.
colStrToInt :: ByteString -> Int 
colStrToInt = colStrToInt' 0 
  where
    colStrToInt' :: Int -> ByteString -> Int
    colStrToInt' !acc "" = acc
    colStrToInt' !acc !b = colStrToInt' (acc + 26^(B.length tail) * coef) tail
      where
        !head = BU.unsafeHead b
        !tail = BU.unsafeTail b
        !coef = fromIntegral $ W.toUpper head - 64 -- so that A -> 1, B -> 2, etc.

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Simple Word8 parsers based on single characters, using efficient Bytestring parsers.

dollar :: Parser ByteString
dollar = string "$"

exc :: Parser ByteString
exc = string "!"

pointer :: Parser ByteString
pointer = string "@"

colon :: Parser ByteString
colon = string ":"

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Parsers for individual parts of expressions

-- | Takes a ByteString representing either '' or '$' and returns a RefType.
readRefType :: ByteString -> RefType
readRefType d1 = case d1 of
  "" -> REL
  _  -> ABS

-- | This parser matches strings of form "$RITESH" to column numbers, represented as ExCols. 
colMatch :: Parser ExCol
colMatch = do
  dol <- option "" dollar
  col <- takeWhile1 isLetter
  return $! ExItem (readRefType dol) $ colStrToCol col

-- | This parser matches strings of the form "$14211" to rows, represented as ExRows. 
rowMatch :: Parser ExRow
rowMatch = do
  dol  <- option "" dollar
  row <- AC.decimal
  return $! ExItem (readRefType dol) $ Row row

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Parsers for location types

-- | Parser for an ExIndex, which matches $AB15.
indexMatch :: Parser ExIndex
indexMatch = do
  xCol <- colMatch
  xRow <- rowMatch
  return $! makeExIndex xCol xRow

-- | Parser matching a finite ASRange (ASIndex:ASIndex), such as A1:B3.
finiteRangeMatch :: Parser ExRange
finiteRangeMatch = do
  tl <- indexMatch
  colon
  br <- indexMatch
  return $! makeFiniteExRange tl br

outOfBoundsMatch :: Parser ExRef
outOfBoundsMatch = string "#REF!" >> return ExOutOfBounds

-- | Parser matching A1:A (Index:Column) column ranges. 
colRangeA1ToAMatch :: Parser ExRange
colRangeA1ToAMatch = do
  tl <- indexMatch
  colon
  r <- colMatch
  return $! makeExColRange tl r

-- | Parser matching column ranges of the form A:A1 (reverse).
colRangeAToA1Match :: Parser ExRange
colRangeAToA1Match = do
  r <- colMatch
  colon
  tl <- indexMatch
  return $! makeExColRange tl r

-- | Parser matching column ranges of the form A:A (column:column). 
-- Due to the column range type, we parse A:A as A$1:A. 
colRangeAToAMatch :: Parser ExRange
colRangeAToAMatch = do
  l <- colMatch
  colon
  r <- colMatch
  return $! makeExColRange (l, ExItem ABS $ Row 1) r

-- | Parser that matches all column ranges; checks for matches to both A:A and A1:A.
-- The ordering of the or's matters here. 
colRangeMatch :: Parser ExRange
colRangeMatch = colRangeAToA1Match <|> colRangeA1ToAMatch <|> colRangeAToAMatch

-- | Parser matching template expressions, which includes sampling expressions. 
-- These expressions (for now) must begin with !, and have the rest between {}
templateMatch :: Parser ExTemplateExpr
templateMatch = do
  exc *> string "{" *> PC.spaces
  n <- AC.decimal
  PC.spaces *> string "," *> PC.spaces
  idx <- indexMatch
  PC.spaces
  return $! ExSampleExpr n idx

-- | Parser matching either a finite ExRange or an Infinite ExRange.
-- It is important that try finiteRangeMatch is called before try colRangeMatch.
rangeMatch :: Parser ExRange
rangeMatch = finiteRangeMatch <|> colRangeMatch

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Sheet and workbook parsers

-- | Parser matching a valid sheet name. It also consumes the !. 
nameMatch :: Parser ByteString
nameMatch = takeWhile1 (not . badWord8) <* exc
  where
    -- These Word8's match up to !, $, @, :, and ' ' 
    -- Following attoparsec advice to use custom byte-checking
    badWord8 w = w == _exclam || w == _dollar || w == _at || w == _colon || w == _space

-- | Parser matching a (workbook,  sheet) pair, delimited by ! (which will be consumed).
sheetWorkbookMatch :: Parser (Maybe SheetName, Maybe WorkbookName)
sheetWorkbookMatch = do
  name1 <- optional nameMatch
  case name1 of
    Nothing -> return (Nothing, Nothing)
    Just n1 -> do 
      name2 <- optional nameMatch
      case name2 of 
        Nothing -> return (C.unpack <$> name1, Nothing)
        Just n2  -> return (C.unpack <$> name2, C.unpack <$> name1)

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Top-level parsers

data InnerReference = 
    InnerRange ExRange
  | InnerTemplate ExTemplateExpr
  | InnerIndex ExIndex

innerRefMatch :: Parser InnerReference
innerRefMatch = 
      InnerRange    <$> rangeMatch
  <|> InnerTemplate <$> templateMatch
  <|> InnerIndex    <$> indexMatch

-- | Putting it all together, a parser matching an ExRef.
refMatch :: Parser ExRef
refMatch = do
  point <- optional pointer
  (sh, wb) <- sheetWorkbookMatch
  innerRef <- optional innerRefMatch
  case innerRef of 
    Just ref -> case point of 
      Just _ -> case ref of 
        InnerIndex idx -> return $! ExPointerRef idx sh wb
        _ -> fail "expected index reference when using pointer syntax"
      Nothing -> return $! case ref of 
        InnerRange rng    -> ExRangeRef rng sh wb
        InnerTemplate t   -> ExTemplateRef t sh wb
        InnerIndex idx    -> ExIndexRef idx sh wb
    Nothing ->  outOfBoundsMatch <?> "expected valid excel A1:B4 format"