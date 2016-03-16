{-# LANGUAGE OverloadedStrings, TemplateHaskell, BangPatterns #-}

module AS.Parsing.Substitutions where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Word8 as W
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as BU
import Data.Attoparsec.Combinator (lookAhead)

import Prelude()
import AS.Prelude hiding (takeWhile)
import AS.Types.Cell
import AS.Types.Excel
import AS.Types.Locations
import AS.Types.Sheets
import AS.Types.Shift
import AS.Parsing.Excel
import AS.Util

import AS.Kernels.Excel.Compiler (literal)
import qualified AS.Parsing.Common as PC

----------------------------------------------------------------------------------------------------
-- General parsing functions for Excel

-- | Represents the ByteString until (before) an ExRef, and the ExRef itself
data UntilExRef = UntilExRef {
  previousBS :: !ByteString,
  nextRef :: !ExRef
}

-- | A version of liftM2 that is strict in the result of its first action.
liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  !x <- a
  y <- b
  return (f x y)
{-# INLINE liftM2' #-}

-- | A version of manyTill that returns not only the intermediate list of values from parsing p, 
-- but also the result of parsing end. Avoids having to use (lookAhead x) for end. Values
-- returned by p are forced to WHNF.
manyTillAndResult :: Parser a -> Parser b -> Parser ([a], b)
manyTillAndResult p end = scan
  where success = end >>= (\bResult -> return ([], bResult))
        add !aResult (aResults, bResult) = (aResult:aResults, bResult)
        scan = success `mplus` liftM2' add p scan

-- | Parser for extractring the first Excel reference that's not within quotes from a string. 
-- Returns the bytes before the Excel reference as well as the reference itself. 
-- Will fail if the input has no Excel refs. 
getFirstExcelRef :: Parser UntilExRef
getFirstExcelRef = do 
  (bs, ref) <- manyTillAndResult notExcelParser refMatch
  return $! UntilExRef (B.concat bs) ref

-- | Parses a ByteString until the first ExRef match that isn't within quotes. All of the previous
-- bytes are kept the same, and we use the given function to replace the first ExRef with another 
-- ByteString. We return the initial bytes + replaced ExRef bytes. 
-- Assumes that you're not starting in the middle of a quote. 
parseNext :: (ExRef -> ByteString) -> Parser ByteString
parseNext f = do 
  (UntilExRef bs ref) <- getFirstExcelRef
  return $! B.append bs (f ref)
{-# INLINE parseNext #-}

-- | This parser is applied "in between" refMatches. It tries to consume as much as possible
-- given that the refMatch didn't succeed. Instead of only consuming the next character, and
-- allowing getFirstExcelRef to try refMatch again, it consumes immediate letters, numbers, !, @.
-- If it comes across a quote/apostrophe, it continues until the closing quote/apostrophe. If both
-- of the previous fail (for example, on a space), we consume that one character. This parser is
-- intended as an optimization to PC.quotedStringEscaped <|> (B.singleton <$> anyWord8).
notExcelParser :: Parser ByteString
notExcelParser = parser
  where parser  = PC.quotedStringEscaped <|> 
                  (takeWhile1 goodWord8) <|> 
                  (B.singleton <$> anyWord8)
        goodWord8 w = isLetter w || isDigit w || w == _exclam || w == _at

-- | Replaces all ExRefs in a ByteString that aren't in quotes/apostrophes. 
excelParser :: (ExRef -> ByteString) -> ByteString -> ByteString
excelParser f = $fromRight . parseOnly parser
  where
    parser = do 
      excels <- many' $ parseNext f
      rest <- takeByteString
      return $! B.append (B.concat excels) rest
{-# INLINE excelParser #-}

-- | Returns all of the UntilExRefs, and the last piece of the ByteString after all ExRefs
-- have been consumed.
getAllExcelRefs :: Parser ([UntilExRef], ByteString)
getAllExcelRefs = do 
  excels <-  many' $ getFirstExcelRef
  rest <- takeByteString
  return $! (excels, rest)

----------------------------------------------------------------------------------------------------
-- Replacing and extracting from an ASExpression

-- | Given a replacer function for Excel references, replace all Excel references in a given 
-- expression with an application of this replacer function.
replaceRefs :: (ExRef -> String) -> ASExpression -> ASExpression
replaceRefs f xp@(Expression _ lang) 
  | isExcelLiteral xp = xp
  | otherwise = xp & expression %~ modifyExpression
      where modifyExpression str = C.unpack $ excelParser (C.pack . f) (C.pack str) 
{-# INLINE replaceRefs #-}

replaceRefsIO :: (ExRef -> IO String) -> ASExpression -> IO ASExpression
replaceRefsIO f = return . replaceRefs (unsafePerformIO . f) 
{-# INLINE replaceRefsIO #-}

isExcelLiteral :: ASExpression -> Bool
isExcelLiteral (Expression xp lang) = (lang == Excel) && parsedCorrectly
  where parsedCorrectly = case parseOnly literal $ C.pack xp of
                            Right _ -> True
                            Left _  -> False

-- | Returns the list of Excel references in an ASExpression. 
getExcelReferences :: ASExpression -> [ExRef]
getExcelReferences xp@(Expression str lang)
  | isExcelLiteral xp = []
  | otherwise = (map nextRef . $fromRight) parseResult
      where parseResult = parseOnly (many getFirstExcelRef) (C.pack str)

-- | This is a specialized function for use in Eval/Core so that you don't call replaceRefsIO
-- in function and then getExcelReferences in a child function (and do 2x the parsing work).
getSubstitutedXpAndReferences :: (ExRef -> IO String) -> ASExpression -> IO (ASExpression, [ExRef])
getSubstitutedXpAndReferences f (Expression xp lang) = do 
  let transform r = C.pack <$> f r
  let (xs, last) = $fromRight $ parseOnly getAllExcelRefs (C.pack xp)
  newStr <- B.concat <$> (forM xs $ \(UntilExRef b r) -> B.append b <$> transform r)
  let finalStr = B.append newStr last
  return $! (Expression (C.unpack finalStr) lang, map nextRef xs)

-- | Returns the list of dependencies in ASExpression. 
-- #needsrefactor not all ASReferences are valid references for the graph.
getDependencies :: ASSheetId -> ASExpression -> [ASReference]
getDependencies sheetId = map (convertInvalidRef . exRefToASRef sheetId) . getExcelReferences
  where 
    convertInvalidRef r = case r of 
      TemplateRef t -> case t of 
        SampleExpr _ idx -> IndexRef idx
      _ -> r

----------------------------------------------------------------------------------------------------
-- Copy/paste and Cut/paste

-- | Takes in an offset and a cell, and returns the cell you get when you shift the cell by
-- the offset. (The location changes, and the non-absolute references in the expression changes.)
shiftExpression :: Offset -> ASExpression -> ASExpression
shiftExpression offset = replaceRefs (show . shiftExRefNF offset)

-- | Shift the cell's location, and shift all references satisfying the condition passed in. 
shiftCell :: Offset -> ASCell -> Maybe ASCell
shiftCell offset c = ((c &) . modifyXp . set cellLocation) <$> mLoc
  where modifyXp = ((cellExpression %~ shiftExpression offset) .)
        mLoc  = shiftByOffsetWithBoundsCheck offset $ c^.cellLocation

----------------------------------------------------------------------------------------------------
