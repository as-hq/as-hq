module AS.Kernels.Excel.Compiler where

import AS.Types.Excel
import AS.Types.CellProps
import AS.Types.Errors
import AS.Parsing.Excel (refMatch)
import qualified AS.Parsing.Common as C

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding ((<|>), many)

import Data.List (elemIndices)
import Data.Char (toUpper,toLower)
import Data.Time.Calendar

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Top-level parsers.

parseFormula :: String -> ThrowsError ContextualFormula
parseFormula s = either (\_ -> Left ExcelSyntaxError) return $ parse eitherLiteralFormula "" s

eitherLiteralFormula :: Parser ContextualFormula
eitherLiteralFormula =
      (try formula)
  <|> (SimpleFormula <$> try literal)
  <|> (SimpleFormula <$> emptyExpr)

emptyExpr :: Parser Formula
emptyExpr = do 
  eof
  return $ Basic . Var $ EBlank 

literal :: Parser Formula
literal = do
  firstChar <- noneOf ['='] -- if the first char is =, it's a formula. 
  restChars <- manyTill anyChar (try eof)
  let str = firstChar:restChars
  let val = parse justNumOrBool "" str
  return $ case val of
    (Left _) -> Basic . Var $ EValueS str
    (Right v) -> v

-- | Formulas.
formula :: Parser ContextualFormula
formula =  do
  opener <- option ' ' $ try (char '{')
  symbol "="
  f <- expr
  case opener of
    '{' -> char '}' >> eof >> (return $ ArrayFormula f)
    _ -> eof >> (return $ SimpleFormula f)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Language definition

-- | Expressions.
expr :: Parser Formula
expr    =   buildExpressionParser table leaf
        <?> "expression"

-- | Table to build expression grammar.
table :: OperatorTable Char () Formula
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

-- | Helper.
binary :: String -> Assoc -> Operator Char () Formula
binary name assoc
  = Infix (do{ reservedOp name; return $ \x y -> Basic $ Fun name [x,y] }) assoc

-- | Helper.
prefix :: String -> Operator Char () Formula
prefix name
  = Prefix (do{ reservedOp name; return $ \x -> Basic $ Fun name [x] })

-- NOTE: this parser will not check if there exists a higher-precedence operator that's a superset
-- the purpose of the ++ "p" is to distinguish infix and prefix operators with the same name, 
-- eg (-). See functions :: M.Map String FuncDescriptor in Lib.hs for where this is used. 
topLevelPrefixParser op = string op >> return (\x -> Basic $ Fun (op ++ "p") [x])

pos = topLevelPrefixParser "+"

neg = topLevelPrefixParser "-"

prefixRepeated p = Prefix . chainl1 p $ return (.)

-- | Helper.
postfix :: String -> Operator Char () Formula
postfix name
  = Postfix (do{ reservedOp name; return $ \x -> Basic $ Fun name [x] })


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Lexer

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser excelLang

excelLang = LanguageDef
   { commentStart    = ""
   , commentEnd      = ""
   , commentLine     = ""
   , nestedComments  = True
   , identStart      = letter <|> oneOf "_#" 
   , identLetter     = alphaNum <|> oneOf "_':!."
   , opStart         = opLetter emptyDef
   , opLetter        = oneOf ":!#%&*+./<=>?@\\^|-~"
   , reservedOpNames = []
   , reservedNames   = []
   , caseSensitive   = True
   }

whiteSpace    = P.whiteSpace lexer
lexeme        = P.lexeme lexer
symbol        = P.symbol lexer
natural       = P.natural lexer
integer       = P.integer lexer
float         = C.float'
parens        = P.parens lexer
semi          = P.semi lexer
semiSep       = P.semiSep lexer
commaSep      = P.commaSep lexer
identifier    = P.identifier lexer
reserved      = P.reserved lexer
reservedOp    = P.reservedOp lexer
stringLiteral = P.stringLiteral lexer


----------------------------------------------------------------------------------------------------------------------------------------------
-- | AST node parsers.

-- | Terms (leafs of expressions).
leaf' :: Parser Formula
leaf'    =  try (parens expr)
        <|> try functionApplication
        <|> try arrayConst
        <|> try excelValue
        <|> try referenceIntersection
        <|> try cellReference
        <|> blankValue
        <?> "simple expression"

leaf :: Parser Formula
leaf = do
  spaces
  f <- leaf'
  spaces
  return f

word :: Parser String
word = many1 (letter <|> digit)

separator :: Parser ()
separator = skipMany1 space


referenceIntersection :: Parser Formula
referenceIntersection = do
  c1 <- cellReference
  many1 (char ' ')
  c2 <- cellReference
  return . Basic $ Fun " " [c1,c2]


cellReference :: Parser Formula
cellReference = fmap (Basic . Ref) refMatch

-- | Function application.
functionApplication :: Parser Formula
functionApplication = do
  i <- option "tuple" $ try identifier
  fs <- parens (try (spaces >> return []) <|> (commaSep expr))
  return . Basic $ Fun i fs


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Array formulas.

-- | Parses full array formula.
arrayConst :: Parser Formula
arrayConst = do
  char '{' >> spaces
  fs <- arrayContents
  spaces >> char '}'
  return $ ArrayConst fs

-- | Parses array contents by building up array elements.
arrayContents :: Parser [[BasicFormula]]
arrayContents = do
  elm  <- arrayElement
  elms <- option [] $ try (delimit ';' >> arrayContents) -- option terminates the recursion
  return (elm:elms)

-- | Parses array element as either a row or single basic formula.
arrayElement :: Parser [BasicFormula]
arrayElement = arrayRow <|> fmap (\(Basic f) -> [f]) expr

-- | Parses row-type array element.
arrayRow :: Parser [BasicFormula]
arrayRow =
  do
    rowOpener <- option ' ' $ try (char '{')
    bfs <- arrayRowContents
    case rowOpener of
      '{' -> char '}'
      _   -> return ' '
    return bfs

-- | Parses row contents by building up row elements (basic formulas).
arrayRowContents :: Parser [BasicFormula]
arrayRowContents =
  do
    (Basic xp) <- expr
    xps <- option [] $ try (char ',' >> arrayRowContents) -- no need to delimit, 'expr' already checks spaces
    return (xp:xps)


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Common parsers

-- | delimits with spaces.
delimit :: Char -> Parser ()
delimit c = spaces >> char c >> spaces

-- | Excel-specific optionMaybe
optionMaybe' p
  = option Nothing (fmap Just p)


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Parsing values.

-- | "tRuE" and "true" and "TRUE" all parse to true in Excel.
readBool :: String -> Bool
readBool str = case (map toUpper str) of
  "TRUE"  -> True
  "FALSE" -> False

str :: Parser String
str = C.quotedString


-- dirty, dirty hack to turn integer "123" into float "0.123"
integerToDecimal :: Integer -> Double
integerToDecimal i = read ("0." ++ (show i)) :: Double

percentToDecimal :: Double -> Double
percentToDecimal p = p / 100

dateToDecimal :: Integer -> Integer -> Integer -> Double
dateToDecimal month day year = fromInteger dateDouble
  where 
    year' | year < 30                = 2000 + year
          | 30 <= year && year < 100 = 1900 + year
          | otherwise = year
    d    = fromGregorian (fromInteger year') (fromInteger month) (fromInteger day)
    base = fromGregorian 1900 1 1
    dateDouble = diffDays d base

float' :: Parser Double
float' = 
      (try float)
  <|> (try $ do {i <- integer; char '.'; return $ fromInteger i} )
  <|> (try $ spaces >> char '.' >> integerToDecimal <$> integer)

floatOrInteger :: Parser Double
floatOrInteger = (try float') <|> (fromInteger <$> integer)

money :: Parser (Formatted Double)
money = do 
  spaces >> char '$' >> spaces
  f <- floatOrInteger 
  return $ Formatted f $ Just Money

percentage :: Parser (Formatted Double)
percentage = do
  spaces
  p <- floatOrInteger
  char '%' >> spaces
  return $ Formatted (percentToDecimal p) $ Just Percentage

date :: Parser (Formatted Double)
date = do 
  month <- natural
  char '/'
  day <- natural
  char '/'
  year <- natural
  if (month > 12 || day > 31)
    then fail "Invalid date"
    else return $ Formatted (dateToDecimal month day year) $ Just Date

formattedFloat :: Parser (Formatted Double)
formattedFloat = (try money) <|> (try percentage) <|> (try date) <|> (try $ return <$> float')

formattedFloatToEValue :: Formatted Double -> EValue
formattedFloatToEValue (Formatted d f) = EValueNum $ Formatted (EValueD d) f

excelValue :: Parser Formula
excelValue = fmap (Basic . Var) $
      try $ (formattedFloatToEValue <$> formattedFloat)
  <|> try ((EValueNum . return . EValueI . fromInteger) <$> integer)
  <|> try (EValueB <$> C.bool)
  <|> try (EValueS <$> str)

numOrBool :: Parser Formula
numOrBool = fmap (Basic . Var) $
      try (formattedFloatToEValue <$> formattedFloat)
  <|> try ((EValueNum . return . EValueI . fromInteger) <$> integer)
  <|> try (EValueB <$> C.bool)

justNumOrBool :: Parser Formula
justNumOrBool = do 
  v <- numOrBool
  eof
  return v

blankValue :: Parser Formula
blankValue = spaces >> (return . Basic $ Var EMissing)