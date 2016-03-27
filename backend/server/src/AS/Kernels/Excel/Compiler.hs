{-# LANGUAGE OverloadedStrings #-}

module AS.Kernels.Excel.Compiler where

import Data.Time.Calendar
import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Attoparsec.Expr
import Data.ByteString (ByteString)
import Data.Word8 as W
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.Attoparsec.Combinator
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as BU

import AS.Prelude hiding (Prefix, Infix, takeWhile)
import AS.Config.Constants
import AS.Types.CellProps
import AS.Types.Errors
import AS.Types.Excel
import AS.Types.Formats
import AS.Parsing.Excel (refMatch)
import qualified AS.Parsing.Common as PC
import AS.Util

----------------------------------------------------------------------------------------------------------------------------------------------
-- Top-level parsers.

parseFormula :: ByteString -> ThrowsError ContextualFormula
parseFormula s = either (const $ Left ExcelSyntaxError) return parseResult
  where parseResult = parseOnly eitherLiteralFormula s

eitherLiteralFormula :: Parser ContextualFormula
eitherLiteralFormula = formula <|> (SimpleFormula <$> stringLiteral) <|> 
                       (SimpleFormula <$> literal) <|> (SimpleFormula <$> emptyExpr)

emptyExpr :: Parser Formula
emptyExpr = (endOfInput <?> "not at end") >> (return $ Basic $ Var EBlank)

-- | Parser for an Excel literal. Fails if the first character is '='.
literal :: Parser Formula
literal = do
  lookAhead $ notWord8 61 -- fail if first character is '='
  rest <- takeByteString
  let val = parseOnly justNumOrBool rest
  return $! case val of
    Left _ -> Basic . Var . EValueS . C.unpack $ rest
    Right v -> v

stringLiteral :: Parser Formula
stringLiteral = do
  string "'"
  rest <- takeByteString
  return $! Basic . Var . EValueS . C.unpack $ rest 

-- | Parser for Excel formulas
formula :: Parser ContextualFormula
formula =  do
  opener <- option " " $ string "{"
  symbol "="
  f <- expr
  case opener of
    "{" -> string "}" >> endOfInput >> (return $ ArrayFormula f)
    _ -> endOfInput >> (return $ SimpleFormula f)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Language definition

-- | Parser for expressions, using a port of Parsec's expression parser.
expr :: Parser Formula
expr    =   buildExpressionParser table leaf <?> "expression"

-- | Table to build expression grammar for Excel. This is using Data.Attoparsec.Expr which doesn't
-- look completely optimized, uses lazy folds etc. It's a small part of the perf output, so I won't
-- bother improving it now.
table :: OperatorTable ByteString Formula
table   = [ [binary ":" AssocLeft]
          , [prefixRepeated $ choice [pos, neg]]
          , [postfix "%"]
          , [binary "^" AssocLeft]
          , [binary "*" AssocLeft, binary "/"  AssocLeft ]
          , [binary "+" AssocLeft, binary "-"  AssocLeft ]
          , [binary "&" AssocLeft]
          , [binary "=" AssocLeft
              , binary "<>" AssocLeft
              , binary "<="  AssocLeft
              , binary ">=" AssocLeft
              , binary "<" AssocLeft
              , binary ">" AssocLeft
            ]
          ]

binary :: ByteString -> Assoc -> Operator ByteString Formula
binary name = Infix $ do 
  reservedOp name 
  return $ \x y -> Basic $ Fun (C.unpack name) [x, y]

prefix :: ByteString -> Operator ByteString Formula
prefix name = Prefix $ do 
  reservedOp name 
  return $ \x -> Basic $ Fun (C.unpack name) [x]

postfix :: ByteString -> Operator ByteString Formula
postfix name = Postfix $ do 
  reservedOp name
  return $ \x -> Basic $ Fun (C.unpack name) [x]

-- NOTE: this parser will not check if there exists a higher-precedence operator that's a superset
-- the purpose of the ++ "p" is to distinguish infix and prefix operators with the same name, 
-- eg (-). See functions :: M.Map String FuncDescriptor in Lib.hs for where this is used. 
topLevelPrefixParser op = string op >> return (\x -> Basic $ Fun ((C.unpack op) ++ "p") [x])
pos = topLevelPrefixParser "+"
neg = topLevelPrefixParser "-"
prefixRepeated p = Prefix . PC.chainl1 p $ return (.)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Lexing parsers

-- | Parses a ByteString, and skips spaces at the end
symbol :: ByteString -> Parser ByteString
symbol s = string s <* PC.spaces

-- | Wraps a parser between parentheses; returns the value of the parser
-- Accounts for spacing inside the ()
parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

-- | Parses zero or more occurances of the input parser, separated by commas
-- Evaluates each result from the parser to WHNF.
commaSep :: Parser a ->  Parser [a]
commaSep p = sepBy' p (string ",")

-- | Parser for natural numbers (positive). TODO: support hexadecimal and octal.
natural :: Parser Integer
natural = AC.decimal

-- | Parser for identifiers for the Excel language. Fails on reserved words and
-- matches words starting with specified characters, with a specified alphabet.
identifier :: Parser ByteString
identifier = do 
  name <- ident
  if isReservedName name
    then fail "reserved word"
    else return name

-- | Excel does not have any reserved names (I think) 
isReservedName :: ByteString -> Bool 
isReservedName b = False

-- | Parser for a valid identifier; used above. Neglects reserved words in the language.
ident :: Parser ByteString
ident = do 
  c <- takeWhile $ \w -> isLetter w 
          || w == _underscore 
          || w == _numbersign
  -- ^ starting bytes can be letters, _, or #. 
  -- Following attoparsec's advice to use hand-rolled byte predicates.
  cs <- takeWhile $ \w -> isAlphaNum w 
          || w == _numbersign 
          || w == _quotesingle 
          || w == _exclam 
          || w == _period
  -- ^ ending letters can be alphanumerics, _, ', !, or .
  return $! C.append c cs

-- | Parser for a reserved operator with a given name. 
-- TODO: make sure that the name is not followed by a valid opLetter (":!#%&*+./<=>?@\\^|-~")
reservedOp :: ByteString -> Parser ByteString
reservedOp = string

----------------------------------------------------------------------------------------------------------------------------------------------
-- AST node parsers

-- | Parser matching terms (leafs of expressions in the Excel AST)
leaf :: Parser Formula
leaf = PC.betweenSpaces leaf'

-- | Helper for the above, applying a bunch of sub-parsers in order
leaf' :: Parser Formula
leaf'    =  parens expr
        <|> functionApplication
        <|> arrayConst
        <|> excelValue
        -- <|> referenceIntersection
        <|> cellReference
        <|> blankValue
        <?> "simple expression"

-- | Parser matching words (digits and letters)
word :: Parser ByteString
word = takeWhile $ \w -> isLetter w || isDigit w

--referenceIntersection :: Parser Formula
--referenceIntersection = do
--  c1 <- cellReference
--  takeWhile1 (== _space) -- at least one space
--  c2 <- cellReference
--  return $! Basic $ Fun " " [c1, c2]

cellReference :: Parser Formula
cellReference = fmap (Basic . Ref) refMatch

-- | Function application
functionApplication :: Parser Formula
functionApplication = do
  i <- option "tuple" identifier 
  fs <- parens (PC.spaces >> return []) <|> (parens $ commaSep expr)
  return $! Basic $ Fun (C.unpack i) fs

----------------------------------------------------------------------------------------------------------------------------------------------
-- Array formulas

-- | Delimits with spaces
delimit :: Char -> Parser ()
delimit c = void $ PC.spaces >> PC.string' [c] >> PC.spaces

-- | Parses full array formula
arrayConst :: Parser Formula
arrayConst = do
  string "{" >> PC.spaces
  fs <- arrayContents
  PC.spaces >> string "}"
  return $! ArrayConst fs

-- | Parses array contents by building up array elements.
arrayContents :: Parser [[BasicFormula]]
arrayContents = do
  elm  <- arrayElement
  elms <- option [] $ delimit ';' >> arrayContents 
  return $! elm:elms

-- | Parses array element as either a row or single basic formula.
arrayElement :: Parser [BasicFormula]
arrayElement = arrayRow <|> fmap (\(Basic f) -> [f]) expr

-- | Parses row-type array element
arrayRow :: Parser [BasicFormula]
arrayRow = do
  rowOpener <- option " " $ string "{"
  bfs <- arrayRowContents
  case rowOpener of
    "{" -> string "}"
    _   -> return " "
  return $! bfs

-- | Parses row contents by building up row elements (basic formulas).
arrayRowContents :: Parser [BasicFormula]
arrayRowContents = do
  (Basic xp) <- expr
  xps <- option [] $ string "," >> arrayRowContents 
  -- ^ no need to delimit, 'expr' already checks spaces
  return $! xp:xps

----------------------------------------------------------------------------------------------------------------------------------------------
-- Parsing values.

-- Note: there is room for some future optimization here. Notice that the float and integer 
-- parsers overlap quite a bit, and so do the money/date/percent ones. They can probably be 
-- unrolled a bit. 

percentToDecimal :: Double -> Double
percentToDecimal = (/ 100)

dateToDecimal :: Integer -> Integer -> Integer -> Double
dateToDecimal month day year = fromInteger dateDouble
  where 
    year' | year < 30                = 2000 + year
          | 30 <= year && year < 100 = 1900 + year
          | otherwise = year
    d    = fromGregorian (fromInteger year') (fromInteger month) (fromInteger day)
    base = fromGregorian 1900 1 1
    dateDouble = diffDays d base

floatOrInteger :: Parser Double
floatOrInteger = PC.float <|> (fromInteger <$> PC.integer)

money :: Parser (Formatted Double)
money = do 
  PC.betweenSpaces $ string "$"
  f <- floatOrInteger 
  return $! Formatted f $ Just (Format Money Nothing)

percentage :: Parser (Formatted Double)
percentage = do
  PC.spaces
  p <- floatOrInteger
  string "%" >> PC.spaces
  return $! Formatted (percentToDecimal p) $ Just (Format Percentage Nothing)

date :: Parser (Formatted Double)
date = do 
  month <- natural
  string "/"
  day <- natural
  year <- option current_year $ string "/" >> natural
  if month > 12 || day > 31
    then fail "Invalid date"
    else return $! Formatted (dateToDecimal month day year) $ Just (Format Date Nothing)

formattedFloat :: Parser (Formatted Double)
formattedFloat = money <|> percentage <|> date <|> (return <$> PC.float)

formattedFloatToEValue :: Formatted Double -> EValue
formattedFloatToEValue (Formatted d f) = EValueNum $ Formatted (EValueD d) f

excelValue :: Parser Formula
excelValue = fmap (Basic . Var) $
      (formattedFloatToEValue <$> formattedFloat)
  <|> ((EValueNum . return . EValueI . fromInteger) <$> PC.integer)
  <|> (EValueB <$> PC.bool)
  <|> (EValueS . C.unpack <$> PC.unescapedString)

numOrBool :: Parser Formula
numOrBool = fmap (Basic . Var) $
      (formattedFloatToEValue <$> formattedFloat)
  <|> ((EValueNum . return . EValueI . fromInteger) <$> PC.integer)
  <|> (EValueB <$> PC.bool)

justNumOrBool :: Parser Formula
justNumOrBool = numOrBool <* endOfInput 

blankValue :: Parser Formula
blankValue = PC.spaces >> (return . Basic $ Var EMissing)
