{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module AS.Parsing.Common where

import Control.Applicative
import System.IO.Unsafe (unsafePerformIO)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Word8 as W
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString.Internal as BI

import Prelude()
import AS.Prelude hiding (takeWhile)
import AS.Util
import qualified AS.LanguageDefs as LD

-------------------------------------------------------------------------------------------------------------------------
-- Util parsers

-- | Parses a bunch of spaces (at least zero) using an efficient ByteString parser.
spaces :: Parser ByteString
spaces = takeWhile (== _space)
{-# INLINE spaces #-}

-- | Parser that matches a given string input.
string' :: String -> Parser ByteString
string' s = string $ C.pack s
{-# INLINE string' #-}

-- | Parser that applies another parser between matching two strings. Returns the value
-- of the intermediate parser.
between' :: String -> String -> Parser a -> Parser a
between' start end p = string' start *> p <* string' end
{-# INLINE between' #-}

-- | Apply a parser between a bunch of spaces. Similar to the above.
betweenSpaces :: Parser a -> Parser a
betweenSpaces p = spaces *> p <* spaces
{-# INLINE betweenSpaces #-}

-- | Parses one or more occurrences of p, separated by op. Returns a value obtained by a left 
-- associative application of all functions returned by op to the values returned by p. 
-- It's used in the Excel compiler to allow for multiple prefixes (--, ++).
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do { !x <- p; rest x }
  where
    rest x = do { f <- op; y <- p; rest $! f x y } <|> return x
{-# INLINE chainl1 #-}

-------------------------------------------------------------------------------------------------------------------------
-- String escaping parsers

-- | This helper function replaces bytes with their escaped versions
-- old = ['b',  'n',  'f',  'r',  't',  '\\', '\'', '\"', '/']
-- new = ['\b', '\n', '\f', '\r', '\t', '\\', '\'', '\"', '/']
replaceEscaped :: Word8 -> Word8
replaceEscaped 98 = 8
replaceEscaped 110 = 10
replaceEscaped 102 = 12
replaceEscaped 114 = 13
replaceEscaped 116 = 9
replaceEscaped x = x
{-# INLINE replaceEscaped #-}

-- | This helper function replaces \\t with \t, among other things, to unescape a ByteString. 
-- It seems easiest to do this imperatively, advacing Ptrs and keeping track of the current
-- and last position to look for backslashes, and replace the byte right after a backslash, 
-- while filtering out the backslashes. It may be that we could use many (someParser) instead
-- of mapping over the resulting ByteString, but attoparsec recommends sticking to ByteString 
-- methods (100x faster).
unescapeRaw :: ByteString -> ByteString
unescapeRaw ps@(BI.PS x s l)
  | B.null ps = ps
  | otherwise = unsafePerformIO $ BI.createAndTrim l $ \p -> withForeignPtr x $ \f -> do 
      t <- go (f `plusPtr` s) p (f `plusPtr` (s + l)) False
      return $! t `minusPtr` p -- actual length
  where
    go !advancingPtr !accumPtr !endPtr !lastWordWasBackslash
      | advancingPtr == endPtr  = return accumPtr
      | lastWordWasBackslash = do 
          curWord <- peek advancingPtr
          let replacedWord = replaceEscaped curWord 
          poke accumPtr replacedWord 
          go (advancingPtr `plusPtr` 1) (accumPtr `plusPtr` 1) endPtr False
      | otherwise = do 
          curWord <- peek advancingPtr 
          if curWord == _backslash
            then go (advancingPtr `plusPtr` 1) accumPtr endPtr True
            else do 
              poke accumPtr curWord 
              go (advancingPtr `plusPtr` 1) (accumPtr `plusPtr` 1) endPtr False
{-# INLINE unescapeRaw #-}

-- | Parser that escapes the ByteString in between two Word8's. If the given byte is backslash, 
-- it will parse a backslash and unescape everything in the middle until it finds another backslash. 
-- It returns the unescaped ByteString in the middle.
unescapeBetween :: Word8 -> Parser ByteString
unescapeBetween w = word8 w *> (unescapeRaw <$> takeWhile (/= w)) <* word8 w
{-# INLINE unescapeBetween #-}

-- | Matches an escaped string and returns the unescaped version. 
-- E.g. "\"hello\"" -> "hello", "\"hello\\t\"" -> "hello\t".
-- Will fail immediately if the first character isn't \" or \'.
unescapedString :: Parser ByteString
unescapedString = handleQuote <|> handleApostrophe
  where
    handleQuote      = unescapeBetween _quotedbl
    handleApostrophe = unescapeBetween _quotesingle
{-# INLINE unescapedString #-}

-- | Parses a ByteString in between two Word8's and returns Byte + ByteString + Byte.
-- Note that the concatenation is an O(n) operation.
surround :: Word8 -> Parser ByteString -> Parser ByteString
surround w p = do 
  word8 w
  middle <- p
  word8 w
  return $! ((B.cons w) . ((flip B.snoc) w)) middle
{-# INLINE surround #-}

-- | Unescaping parser, but retain the initial surrounding quotes/apostrophes. 
quotedStringEscaped :: Parser ByteString
quotedStringEscaped = quoteString <|> apostropheString
  where
    quoteString      = surround _quotedbl $ unescapeBetween  _quotedbl
    apostropheString = surround _quotesingle $ unescapeBetween _quotesingle
{-# INLINE quotedStringEscaped #-}

-------------------------------------------------------------------------------------------------------------------------
-- Integer, Float, and Bool parsers

-- | Parser that matches a (signed) integer, using Attoparsec. 
integer :: Parser Integer
integer = AC.signed AC.decimal
{-# INLINE integer #-}

-- | Parser that matches a signed double (rational numbers).
float :: Parser Double
float = AC.double
{-# INLINE float #-}

-- | Parses a boolean by trying to match (case-insensitively) true or false.
bool :: Parser Bool
bool = readBool <$> (AC.stringCI "true" <|> AC.stringCI "false")
{-# INLINE bool #-}

readBool :: ByteString -> Bool
readBool b = case BU.unsafeHead b of
  116 -> True
  84  -> True
  102 -> False
  70  -> False
