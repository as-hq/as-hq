{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module AS.Parsing.Excel where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Word8 as W
import Data.Attoparsec.Combinator
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

----------------------------------------------------------------------------------------------------
-- Conversions between various Excel-related types

-- | Converts a ByteString representing an Excel column to a Col datatype. 
-- Example: AA -> 27.
colStrToCol :: ByteString -> Col
colStrToCol = Col . colStrToInt

-- | Tail-recursive helper for colStrToCol.
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

----------------------------------------------------------------------------------------------------
-- Simple Word8 parsers based on single characters, using efficient Bytestring parsers.

dollar :: Parser ByteString
dollar = string "$"

exc :: Parser ByteString
exc = string "!"

pointer :: Parser ByteString
pointer = string "@"

colon :: Parser ByteString
colon = string ":"

----------------------------------------------------------------------------------------------------
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
  -- Do not follow by a letter
  next <- peekWord8 
  case next of
    Nothing -> return ()
    Just w  -> when (isLetter w) $ fail "letter after row"
  return $! ExItem (readRefType dol) $ Row row


----------------------------------------------------------------------------------------------------
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
-- These expressions (for now) must begin with !, and have the rest between {}. 
templateMatch :: Parser ExTemplateExpr
templateMatch = do
  exc *> string "{" *> PC.spaces
  n <- AC.decimal
  PC.betweenSpaces $ string ","
  idx <- indexMatch
  PC.spaces *> string "}"
  return $! ExSampleExpr n idx

-- | Parser matching either a finite ExRange or an Infinite ExRange.
-- It is important that try finiteRangeMatch is called before try colRangeMatch.
rangeMatch :: Parser ExRange
rangeMatch = finiteRangeMatch <|> colRangeMatch


----------------------------------------------------------------------------------------------------
-- Sheet and workbook parsers

-- | Parser matching a valid sheet name. It also consumes the !. 
-- Applying this parser on "hello!" would return "hello".
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

----------------------------------------------------------------------------------------------------
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

---- | Putting it all together, a parser matching an ExRef.
--refMatch :: Parser ExRef
--refMatch = do
--  point <- optional pointer
--  (sh, wb) <- sheetWorkbookMatch
--  innerRef <- optional innerRefMatch
--  case innerRef of 
--    Just ref -> case point of 
--      Just _ -> case ref of 
--        InnerIndex idx -> return $! ExPointerRef idx sh wb
--        _ -> fail "expected index reference when using pointer syntax"
--      Nothing -> return $! case ref of 
--        InnerRange rng    -> ExRangeRef rng sh wb
--        InnerTemplate t   -> ExTemplateRef t sh wb
--        InnerIndex idx    -> ExIndexRef idx sh wb
--    Nothing ->  outOfBoundsMatch <?> "expected valid excel A1:B4 format"

----------------------------------------------------------------------------------------------------

-- | Helper for refMatch that checks for a byte not being a letter or dollar.
notLetterOrDollar :: Word8 -> Bool
notLetterOrDollar w = not $ isLetter w ||  w == 36

-- | Helper for refMatch that checks for end of input, or for not dollar/digit.
endOrNotDollarDigit :: Maybe Word8 -> Bool
endOrNotDollarDigit Nothing = True 
endOrNotDollarDigit (Just w) = not $ isDigit w || w == 36


refMatch :: Parser ExRef
refMatch = do 
  w <- peekWord8'
  case w of
    64 -> do 
      -- If the first byte is a pointer, we consume it and go immediately to matching a
      -- pointer, which doesn't involve any range matching; only matching an index. 
      anyWord8 
      (sh, wb) <- sheetWorkbookMatch
      idx <- indexMatch
      return $! ExPointerRef idx sh wb
    33 -> do 
      -- If the first byte is a !, then go to a template match.
      exc *> string "{" *> PC.spaces
      n <- AC.decimal
      PC.betweenSpaces $ string ","
      (sh, wb) <- sheetWorkbookMatch
      idx <- indexMatch
      PC.spaces *> string "}"
      return $! ExTemplateRef (ExSampleExpr n idx) sh wb
    _  -> if notLetterOrDollar w 
      -- If not @, !, letter, or $ as the starting byte, we cannot have an ExRef.
      then fail "bad start"
      else do
        -- Try matching a sheet and workbook
        (sh, wb) <- sheetWorkbookMatch
        -- Try matching $AB-type ByteStrings, and then check for a colon
        d1 <- option "" dollar 
        col1 <- takeWhile1 isLetter
        let !colIndex = ExItem (readRefType d1) $ colStrToCol col1
        possiblyColon1 <- peekWord8'
        case possiblyColon1 of 
          58 -> do 
            -- If we came across a colon after A, then we must be in the A:A1 or A:A column
            -- range cases. In both cases we try to look for a dollar and a column number.
            anyWord8
            d2 <- option "" dollar 
            col2 <- takeWhile1 isLetter
            let !secondColIndex = ExItem (readRefType d2) $ colStrToCol col2
            afterCol <- peekWord8
            if endOrNotDollarDigit afterCol
              then do 
                -- This is the A:A case; this is either end of input, or the end of feasible 
                -- ref parsing. 
                let !colRangeAA = makeExColRange (colIndex, ExItem ABS $ Row 1) secondColIndex
                return $! ExRangeRef colRangeAA sh wb
              else do 
                -- The only case left is the A:A1 case, so we try to proceed by looking for a 
                -- dollar and then  a row number.
                d3  <- option "" dollar
                row1 <- AC.decimal
                let !secondRowIndex = ExItem (readRefType d3) $ Row row1
                let !bottomIndex = makeExIndex secondColIndex secondRowIndex
                let !aToA1ColRange = makeExColRange bottomIndex colIndex
                return $! ExRangeRef aToA1ColRange sh wb
          _  -> do 
            -- If we do not have a column after A, then we are in the A1:A column type, the
            -- A1:A2 range type, or the A1 index type. All of them start with possibly consuming 
            -- a dollar and looking for a row number.
            d2  <- option "" dollar
            row1 <- AC.decimal
            let !rowIndex = ExItem (readRefType d2) $ Row row1
            let !firstIndex = makeExIndex colIndex rowIndex
            possiblyColon2 <- peekWord8
            case possiblyColon2 of
              Just 58 -> do 
                -- If we came across a colon after the A1 case, then we must either be in the A1:A 
                -- or the A1:A2 cases. Both of them start out by possibly consuming a dollar and
                -- looking for a column string.
                anyWord8 -- consume the colon
                d3 <- option "" dollar 
                col2 <- takeWhile1 isLetter
                let !colIndexLower = ExItem (readRefType d3) $ colStrToCol col2
                afterCol <- peekWord8
                if endOrNotDollarDigit afterCol
                  then do 
                    -- If we have consumed A1:A and have reached end of input, or the 
                    -- next byte is not a dollar/digit, we are done.
                    let !a1toARange = makeExColRange firstIndex colIndexLower 
                    return $! ExRangeRef a1toARange sh wb
                  else do 
                    -- If the byte right after consuming A1:A is a letter or dollar, 
                    -- we will attempt to parse this as a finite range (A1:A2). We start
                    -- by looking for a dollar and row number.
                    d4 <- option "" dollar
                    row2 <- AC.decimal 
                    let !rowIndexLower = ExItem (readRefType d4) $ Row row2
                    let !secondIndex  = makeExIndex colIndexLower rowIndexLower
                    let !a1toA2Range = makeFiniteExRange firstIndex secondIndex
                    return $! ExRangeRef a1toA2Range sh wb
              _ -> do 
                -- If none of the above worked, we have a valid ExIndex from before (the top
                -- left one that was already parsed, no backtracking needed.) 
                -- Make sure that the next byte is not a letter
                case possiblyColon2 of
                  Nothing -> return ()
                  Just n' -> when (isLetter n') $ fail "letter after row"
                return $! ExIndexRef firstIndex sh wb



