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


data InnerReference = 
    InnerRange ExRange
  | InnerTemplate ExTemplateExpr
  | InnerIndex ExIndex

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Top-level parsers

innerRefMatch :: Parser InnerReference
innerRefMatch = 
      InnerRange    <$> try rangeMatch
  <|> InnerTemplate <$> try templateMatch
  <|> InnerIndex    <$> try indexMatch

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Individual reference parsers

readRefType :: Maybe Char -> RefType
readRefType d1 = case d1 of
  Nothing -> REL
  Just _ -> ABS

-- matches a valid sheet name
nameMatch :: Parser String
nameMatch = many1 (noneOf ['!','$','@',':',' ']) <* exc

sheetWorkbookMatch :: Parser (Maybe SheetName, Maybe WorkbookName)
sheetWorkbookMatch = do
  q1 <- optionMaybe $ try nameMatch
  case q1 of 
    Nothing -> return (Nothing, Nothing)
    Just _ -> do
      q2 <- optionMaybe $ try nameMatch
      return $ case q2 of 
        Nothing -> (q1, Nothing) -- sheet, nothing
        Just _ -> (q2, q1)       -- sheet is inner-most parsed (it's q2), so return the reverse order


--matches strings of form "$RITESH" to cols.
colMatch :: Parser ExCol
colMatch = do
  dol <- optionMaybe dollar
  col <- many1 letter
  return $ ExItem (readRefType dol) $ colStrToCol col

--matches strings of form "$14211" to rows.
--will not match the 1 in "1d" to a row.
--Important Note: should only be used in IndexMatch.
--We should NEVER match generic numbers to rows. Timchu, 2/15/16.
rowMatch :: Parser ExRow
rowMatch = do
  dol  <- optionMaybe dollar
  row <- many1 digit
  notFollowedBy letter -- prevents 1d from being interpreted as Excel Row.
  return $ ExItem (readRefType dol) $ rowStrToRow row

-- | matches $AB15 type things
-- We are currently matching (letters)(digits)(notletter) to Excel.
-- There is still an issue where some python functions and variables, like
-- plot3 or var2, will be interpreted as Excel.
-- | matches $AB15 type things
indexMatch :: Parser ExIndex
indexMatch = do
  xCol <- colMatch
  xRow <- rowMatch
  return $ makeExIndex xCol xRow

outOfBoundsMatch :: Parser ExRef
outOfBoundsMatch = string "#REF!" >> return ExOutOfBounds
-- | Three cases for colRange matching
-- Matches A1:A type things.
colRangeA1ToAMatch :: Parser ExRange
colRangeA1ToAMatch = do
  tl <- indexMatch
  colon
  r <- colMatch
  return $ makeExColRange tl r

-- Matches A:A1 type things.
colRangeAToA1Match :: Parser ExRange
colRangeAToA1Match = do
  r <- colMatch
  colon
  tl <- indexMatch
  return $ makeExColRange tl r

-- Parses A:A as A$1:A
colRangeAToAMatch :: Parser ExRange
colRangeAToAMatch = do
  l <- colMatch
  colon
  r <- colMatch
  let tl = (l, (ExItem ABS $ Row 1))
  return $ makeExColRange tl r

-- checks for matches to both both A:A and A1:A.
colRangeMatch :: Parser ExRange
colRangeMatch = do
  -- order matters. AToA must be tried after AToA1
  colrngAToA1 <- optionMaybe $ try colRangeAToA1Match
  colrngA1ToA <- optionMaybe $ try colRangeA1ToAMatch
  case colrngAToA1 of
    Just a -> return a
    Nothing -> case colrngA1ToA of
           Just a -> return a
           Nothing -> colRangeAToAMatch

-- | matches index:index. example: A1:B4
finiteRangeMatch :: Parser ExRange
finiteRangeMatch = do
  tl <- indexMatch
  colon
  br <- indexMatch
  return $ makeFiniteExRange tl br

templateMatch :: Parser ExTemplateExpr
templateMatch = do
  op <- oneOf templateOps
  between (char '{') (char '}') $ case op of 
    '!' -> do
      spaces
      n <- many1 digit
      spaces >> char ',' >> spaces
      idx <- indexMatch
      spaces
      return $ ExSampleExpr ($read n :: Int) idx

-- Parses either a finite exrange or an infinite exrange.
-- It is important that try finiteRangeMatch is called before try colRangeMatch.
rangeMatch :: Parser ExRange
rangeMatch = do
  rng    <- optionMaybe $ try finiteRangeMatch
  colrng <- optionMaybe $ try colRangeMatch
  case rng of
    Just rng' -> return $ rng'
    Nothing -> case colrng of
                 Just colrng' -> return $ colrng'
                 Nothing -> fail "Not a range."

refMatch :: Parser ExRef
refMatch = do
  point <- optionMaybe $ try pointer
  (sh, wb) <- sheetWorkbookMatch
  innerRef <- optionMaybe innerRefMatch
  case innerRef of 
    Just ref -> case point of 
      Just _ -> case ref of 
        InnerIndex idx -> return $ ExPointerRef idx sh wb
        _ -> fail "expected index reference when using pointer syntax"
      Nothing -> return $ case ref of 
        InnerRange rng    -> ExRangeRef rng sh wb
        InnerTemplate t   -> ExTemplateRef t sh wb
        InnerIndex idx    -> ExIndexRef idx sh wb
    Nothing -> 
          outOfBoundsMatch
      <?> "expected valid excel A1:B4 format reference"

templateOps :: [Char]
templateOps = ['!']

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Token parsers

dollar :: Parser Char
dollar = char '$' -- returns $ or ""; $ is not required for index

colon :: Parser Char
colon = char ':' -- this character is necessary for range

exc :: Parser Char
exc = char '!' -- required for sheet access

pointer :: Parser Char
pointer = char  '@'