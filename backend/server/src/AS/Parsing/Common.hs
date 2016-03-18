{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module AS.Parsing.Common where

import Control.Applicative
import Control.Monad
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
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci

import Prelude()
import AS.Prelude hiding (takeWhile)
import AS.Util
import qualified AS.LanguageDefs as LD

-------------------------------------------------------------------------------------------------------------------------
-- Util parsers

-- | Parses a bunch of spaces (at least zero) using an efficient ByteString parser.
spaces :: Parser ByteString
spaces = takeWhile (== _space)

-- | Parser that matches a given string input.
string' :: String -> Parser ByteString
string' = string . C.pack

-- | Parser that applies another parser between matching two strings. Returns the value
-- of the intermediate parser.
between' :: String -> String -> Parser a -> Parser a
between' start end p = string' start *> p <* string' end

-- | Apply a parser between a bunch of spaces. Similar to the above.
betweenSpaces :: Parser a -> Parser a
betweenSpaces p = spaces *> p <* spaces

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

-- | This helper function replaces bytes with their escaped versions. For example, it will 
-- replace b with \b. The full old/new byte list is below. 
-- old = ['b',  'n',  'f',  'r',  't',  '\\', '\'', '\"', '/']
-- new = ['\b', '\n', '\f', '\r', '\t', '\\', '\'', '\"', '/']
replaceEscaped :: Word8 -> Word8
replaceEscaped 98 = 8
replaceEscaped 110 = 10
replaceEscaped 102 = 12
replaceEscaped 114 = 13
replaceEscaped 116 = 9
replaceEscaped x = x

-- | This helper function replaces \\t (two characters) with \t (one character), among other things, 
-- to unescape a ByteString. It seems easiest to do this imperatively, advancing Ptrs and keeping 
-- track of the current/last position to look for backslashes, and replace the byte next to a \, 
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

-- | Parser that escapes the ByteString in between two Word8's. It keeps parsing in the middle until
-- it gets to an instance of the input word that isn't precendented by an escape ('\\'). At this 
-- point, we either reached the end without an ending word (failure), or the next character is the 
-- input word. The returned ByteString is the unescaped middle result. 
-- Example: "\"hello\"" -> "hello", "\"hello\"hi\"hello\"" -> "hello", 
-- "\"cPickle(\\\"Y2Lg==\\\")\"" -> "cPickle(\"Y2Lg==\")"
unescapeBetween :: Word8 -> Parser ByteString
unescapeBetween w = word8 w *> (unescapeRaw <$> scan False f) <* word8 w
  where f !lastWordWasBackslash !byte 
          | (not lastWordWasBackslash) && byte == w = Nothing
          -- ^ if the last word isn't a backslash, and we match w, stop parsing.
          | otherwise = Just $! byte == _backslash -- update state

-- | Matches an escaped string and returns the unescaped version. 
-- E.g. "\"hello\"" -> "hello", "\"hello\\t\"" -> "hello\t".
-- Will fail immediately if the first character isn't \" or \'.
unescapedString :: Parser ByteString
unescapedString = handleQuote <|> handleApostrophe
  where
    handleQuote      = unescapeBetween _quotedbl
    handleApostrophe = unescapeBetween _quotesingle

-- | Inserts a ByteString in between two Word8's and returns Byte + ByteString + Byte.
-- Note that the concatenation is an O(n) operation.
surround :: Word8 -> ByteString -> ByteString
surround w = (B.cons w) . ((flip B.snoc) w)

-- | Unescaping parser, but retain the initial surrounding quotes/apostrophes. 
quotedStringEscaped :: Parser ByteString
quotedStringEscaped = quoteString <|> apostropheString
  where
    quoteString      = surround _quotedbl <$> unescapeBetween  _quotedbl
    apostropheString = surround _quotesingle <$> unescapeBetween _quotesingle

-------------------------------------------------------------------------------------------------------------------------
-- Integer, Float, and Bool parsers

-- | Checks if a byte is e or E.
isLetterE :: Word8 -> Bool
isLetterE w = w == 101 || w == 69

-- | Parser that matches a (signed) integer, using Attoparsec. 
integer :: Parser Integer
integer = AC.signed AC.decimal

-- | Unpacked Scientific notation type; SP c e = c * 10 ^ e.
data SP = SP !Integer {-# UNPACK #-}!Int

-- | Parser that matches a signed double (rational numbers). This is almost the same as the 
-- efficient ByteString-oriented function called "double" in Attoparsec source, but we fix
-- (1) You're allowed to start without a number before the decimal point (.25)
-- (2) Integers will not be parsed as valid floats; "2" will fail, not return 2.0.
-- Note that 22e4 will parse as a double here (for now).
float :: Parser Double
float = do
  let minus = 45
      plus  = 43
  sign <- peekWord8'
  let !positive = sign == plus || sign /= minus
  when (sign == plus || sign == minus) $ void anyWord8 -- consume the plus or minus
  possiblyDot <- peekWord8
  -- If the first byte after the sign is a dot (46), set the integer part to 0
  n <- case possiblyDot of 
    Just 46 -> return 0
    _   -> AC.decimal
  -- Given a bunch of digits after the decimal point, construct a SP object
  -- Example: "12345" -> SP 12345 (-5)
  let f fracDigits = SP (B.foldl' step n fracDigits) (negate $ B.length fracDigits)
      step a w = a * 10 + fromIntegral (w - 48) -- '1' = 49, '2'= 50, etc.
  dotty <- peekWord8
  let signedCoeff !c | positive  = c
                     | otherwise = -c
      toSignedDbl !c !e = Sci.toRealFloat $ Sci.scientific (signedCoeff c) e
      afterE = satisfy isLetterE
      -- Parse a bunch of decimal digits (which is how much to raise the exponent by)
      modifyAfterE !c !e = ((toSignedDbl c) . (e +)) <$> (AC.signed AC.decimal)
      -- If there's no 'e' and the current exponent is zero, we have an int; shortCircuit
      possiblyFailAsInt !c !e !dot = case dot of
        Just 46 -> return $ toSignedDbl c e
        _       -> fail "actually an int"
      -- If there's an e, parse it away and increase the exponent; otherwise just return the
      -- current result as a double
      exponent !c !e !dot = (afterE *> modifyAfterE c e) <|> possiblyFailAsInt c e dot
  SP c e <- case dotty of
              Just 46 -> anyWord8 *> (f <$> takeWhile AC.isDigit_w8)
              -- ^ consume the dot, keep consuming the frac digits, and transform the sci. value
              _       -> return $ SP n 0
  exponent c e dotty

-- | Parses a boolean by trying to match (case-insensitively) true or false.
bool :: Parser Bool
bool = readBool <$> (AC.stringCI "true" <|> AC.stringCI "false")

-- | Only used as a helper for the above; if the first letter is t or T, return true; if it's 
-- f or F return false.
readBool :: ByteString -> Bool
readBool b = case BU.unsafeHead b of
  116 -> True
  84  -> True
  102 -> False
  70  -> False
