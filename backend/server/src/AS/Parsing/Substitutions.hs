{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module AS.Parsing.Substitutions where

import Control.Applicative
import Control.Lens
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

import AS.Kernels.Excel.Compiler (formula)
import qualified AS.Parsing.Common as PC

-------------------------------------------------------------------------------------------------------------------------------------------------
-- General parsing functions

-- | Parses a ByteString until the first ExRef match that isn't within quotes. All of the previous
-- bytes are kept the same, and we use the given function to replace the first ExRef with another 
-- ByteString. We return the initial bytes + replaced ExRef bytes. 
-- Assumes that you're not starting in the middle of a quote. 
parseNext :: (ExRef -> ByteString) -> Parser ByteString
parseNext f = do 
  (bs, ref) <- getFirstExcelRef
  return $! B.append bs (f ref)

getFirstExcelRef :: Parser (ByteString, ExRef)
getFirstExcelRef = do 
  let notRefParser = PC.quotedStringEscaped <|> (B.singleton <$> anyWord8)
  bs <- B.concat <$> manyTill' notRefParser (lookAhead refMatch)
  ref <- refMatch
  return (bs, ref)

-- | Replaces all ExRefs in a ByteString that aren't in quotes/apostrophes. 
excelParser :: (ExRef -> ByteString) -> ByteString -> ByteString
excelParser f = $fromRight . parseOnly parser
  where
    parser = do 
      excels <- many' $ parseNext f
      rest <- takeByteString
      return $! B.append (B.concat excels) rest

replaceRefs :: (ExRef -> String) -> ASExpression -> ASExpression
replaceRefs f xp = xp & expression %~ modifyExpression
  where modifyExpression str = C.unpack $ excelParser (C.pack . f) (C.pack str) 

replaceRefsIO :: (ExRef -> IO String) -> ASExpression -> IO ASExpression
replaceRefsIO f = return . replaceRefs (unsafePerformIO . f) 

-------------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers

-- | Returns the list of Excel references in an ASExpression. 
getExcelReferences :: ASExpression -> [ExRef]
getExcelReferences (Expression xp lang) = (map snd . $fromRight) parseResult
  where parseResult = parseOnly (many getFirstExcelRef) (C.pack xp)

-- | Returns the list of dependencies in ASExpression. 
-- #needsrefactor not all ASReferences are valid references for the graph.
getDependencies :: ASSheetId -> ASExpression -> [ASReference]
getDependencies sheetId = map (convertInvalidRef . exRefToASRef sheetId) . getExcelReferences
  where 
    convertInvalidRef r = case r of 
      TemplateRef t -> case t of 
        SampleExpr _ idx -> IndexRef idx
      _ -> r

----------------------------------------------------------------------------------------------------------------------------------
-- Copy/paste and Cut/paste

-- | Takes in an offset and a cell, and returns the cell you get when you shift the cell by
-- the offset. (The location changes, and the non-absolute references in the expression changes.)
shiftExpression :: Offset -> ASExpression -> ASExpression
shiftExpression offset = replaceRefs (show . shiftExRefNF offset)

-- | Shift the cell's location, and shift all references satisfying the condition passed in. 
shiftCell :: Offset -> ASCell -> Maybe ASCell
shiftCell offset c = ((c &) . ((cellExpression %~ shiftExpression offset) .) . set cellLocation) <$> mLoc
  where mLoc  = shiftByOffsetWithBoundsCheck offset $ c^.cellLocation
