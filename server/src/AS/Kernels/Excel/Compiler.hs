module AS.Kernels.Excel.Compiler where

import AS.Types.Core hiding (str,error,SyntaxError)
import AS.Types.Excel
import AS.Parsing.Out (excelMatch)

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding ((<|>), many)

import Data.List (elemIndices)
import Data.Char (toUpper,toLower)

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Top-level parsers.

parseFormula :: String -> ThrowsError ContextualFormula
parseFormula s = either (\_ -> Left ExcelSyntaxError) return $ parse eitherLiteralFormula "" s

eitherLiteralFormula :: Parser ContextualFormula
eitherLiteralFormula =
      (try formula)
  <|> (SimpleFormula <$> try literal)

literal :: Parser Formula
literal = do
  spaces
  firstChar <- noneOf ['=']
  restChars <- manyTill anyChar (try eof)
  let str = firstChar:restChars
  let val = parse entireExcelValue "" str
  return $ case val of
    (Left _) -> Basic . Var $ EValueS str
    (Right v) -> v

-- | Formulas.
formula :: Parser ContextualFormula
formula =  do
  spaces
  opener <- option ' ' $ try (char '{')
  spaces >> symbol "=" >> spaces
  f <- expr
  case opener of
    '{' -> char '}' >> eof >> (return $ ArrayFormula f)
    _ -> eof >> (return $ SimpleFormula f)


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Language definition

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
topLevelPrefixParser op = string op >> return (\x -> Basic $ Fun op [x])

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
   { commentStart   = ""
   , commentEnd     = ""
   , commentLine    = ""
   , nestedComments = True
   , identStart     = letter <|> char '_' <|> char '#'
   , identLetter    = alphaNum <|> oneOf "_':!"
   , opStart        = opLetter emptyDef
   , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
   , reservedOpNames= []
   , reservedNames  = []
   , caseSensitive  = True
   }

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
integer   = P.integer lexer
float     = P.float lexer
parens    = P.parens lexer
semi      = P.semi lexer
semiSep   = P.semiSep lexer
commaSep  = P.commaSep lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
stringLiteral = P.stringLiteral lexer


----------------------------------------------------------------------------------------------------------------------------------------------
-- | AST node parsers.

-- | Terms (leafs of expressions).
leaf' :: Parser Formula
leaf'    =  parens expr
        <|> arrayConst
        <|> try excelValue
        <|> try referenceIntersection
        <|> try cellReference
        <|> functionApplication
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
cellReference = fmap (Basic . Ref) excelMatch

-- | Function application.
functionApplication :: Parser Formula
functionApplication = do
  i <- option "tuple" $ try identifier
  fs <- parens (commaSep expr)
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

-- | Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- | Match the string 's', accepting either lowercase or uppercase form of each character 
caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

bool :: Parser Bool
bool = fmap readBool $ caseInsensitiveString "TRUE" <|> caseInsensitiveString "FALSE"

str :: Parser String
str = (quoteString <|> apostropheString)
  where
    quoteString      = quotes $ many $ escaped <|> noneOf ['"']
    apostropheString = apostrophes $ many $ escaped <|> noneOf ['\'']
    quotes           = between quote quote
    quote            = char '"' --
    apostrophes      = between apostrophe apostrophe
    apostrophe       = char '\'' -- TODO apostrophes also
    escaped          = char '\\' >> choice (zipWith escapedChar codes replacements)
    escapedChar code replacement = char code >> return replacement
    codes            = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
    replacements     = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

excelValue :: Parser Formula
excelValue = fmap (Basic . Var) $
      try ((EValueNum . EValueD) <$> float)
  <|> try ((EValueNum . EValueI . fromInteger) <$> integer)
  <|> try (EValueB <$> bool)
  <|> try (EValueS <$> str)

entireExcelValue :: Parser Formula
entireExcelValue = do 
  v <- excelValue
  eof
  return v

blankValue :: Parser Formula
blankValue = spaces >> (return . Basic $ Var EMissing)