module AS.Kernels.Excel.Compiler where

import AS.Types.Core hiding (str,error)
import AS.Types.Excel


import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding ((<|>), many)

import Data.List (elemIndices)


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Top-level parsers.

parseFormula :: String -> String -> String -> Either ExcelError ContextualFormula
parseFormula sheetName sourceName s
  = case (parse formula sourceName s) of
      (Left parseError)
        -> Left SyntaxError
      (Right (f, isArray))
        -> Right $ (addSheetName sheetName f, isArray)

-- | Formulas.
formula :: Parser ContextualFormula
formula =  do
  spaces
  opener <- option ' ' $ try (char '{')
  spaces >> symbol "=" >> spaces
  f <- expr
  case opener of 
    '{' -> char '}' >> return (f, True)
    _ -> return (f, False)


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Language definition 

-- | Expressions.
expr :: Parser Formula
expr    =   buildExpressionParser table leaf
        <?> "expression"

-- | Table to build expression grammar.        
table :: OperatorTable Char () Formula
table   = [ [prefixRepeated $ choice [pos, neg]]
          , [postfix "%"]
          , [binary "^" AssocLeft]
          , [binary "*" AssocLeft, binary "/"  AssocLeft ]
          , [binary "+" AssocLeft, binary "-"  AssocLeft ]
          , [binary "&" AssocLeft]
          , [binary "=" AssocLeft, binary "<>" AssocLeft, binary "<="  AssocLeft , binary ">=" AssocLeft, binary "<" AssocLeft, binary ">" AssocLeft ]
          ]

-- | Helper.
binary :: String -> Assoc -> Operator Char () Formula        
binary name assoc 
  = Infix (do{ reservedOp name; return $ \x y -> Basic $ Fun name [x,y] }) assoc

-- | Helper.
prefix :: String -> Operator Char () Formula        
prefix name       
  = Prefix (do{ reservedOp name; return $ \x -> Basic $ Fun name [x] })

-- NOTE: this parser assumes its operator takes highest precedence
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

parseCellRef :: String -> String -> CellRef
parseCellRef sourceName s
  = case (parse sheetCellReference sourceName s) of
      (Left parseError)
        -> error $ show parseError
      (Right cellref)
        ->  cellref

sheetCellReference :: Parser CellRef
sheetCellReference = do
  sheetName <- sepBy1 word separator
  char '!'
  ls <- many1 upper
  ds <- many1 digit
  return $ CellRef (head sheetName) (toColNr ls) (toRowNr ds)

referenceIntersection :: Parser Formula
referenceIntersection = do
  c1 <- cellReference
  many1 (char ' ')
  c2 <- cellReference 
  return . Basic $ Fun " " [c1,c2]

singleCellReference :: Parser CellRef
singleCellReference = do
  ls <- many1 upper
  ds  <- many1 digit
  return $ CellRef {sheet="", colNr=toColNr ls, rowNr=toRowNr ds}

cellReference :: Parser Formula
cellReference = do 
  c1 <- (try (sheetCellReference) <|> singleCellReference) 
  c2 <- optionMaybe' $ do
    char ':'
    singleCellReference
  return . Basic . Ref $ Loc c1 c2
  
-- | Function application.
functionApplication :: Parser Formula
functionApplication = do
  i <- option "union" $ try identifier
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
  elm <- arrayElement
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
      _ -> return ' '
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

readBool :: String -> Bool
readBool str = case str of 
  "TRUE" -> True
  "FALSE" -> False

bool :: Parser Bool
bool = fmap readBool $ string "TRUE" <|> string "FALSE"

str :: Parser String
str = (quoteString <|> apostropheString)
  where
    quoteString     = quotes $ many $ escaped <|> noneOf ['"']
    apostropheString  = apostrophes $ many $ escaped <|> noneOf ['\'']
    quotes = between quote quote
    quote = char '"' -- 
    apostrophes = between apostrophe apostrophe
    apostrophe = char '\'' -- TODO apostrophes also
    escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
    escapedChar code replacement = char code >> return replacement
    codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

excelValue :: Parser Formula
excelValue = fmap (Basic . Var) $
      try (EValueD <$> float)
  <|> try (EValueI <$> integer)
  <|> try (EValueB <$> bool)
  <|> try (EValueS <$> str)

blankValue :: Parser Formula
blankValue = spaces >> (return . Basic $ Var EBlank)


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Utilities

-- | Add sheetname to all CellRef elements on Formula
addSheetName :: String -> Formula -> Formula
addSheetName name (Basic f) = Basic $ addSheetNameBasic name f
addSheetName name (ArrayConst rows) = ArrayConst $ map (\r -> map (addSheetNameBasic name) r) rows

addSheetNameBasic :: String -> BasicFormula -> BasicFormula
addSheetNameBasic _ v@(Var _) = v
addSheetNameBasic name (Fun s formulas) = Fun s (map (addSheetName name) formulas)
addSheetNameBasic name (Ref (Loc a Nothing)) = Ref $ Loc (addSheetNameCellRef name a) Nothing
addSheetNameBasic name (Ref (Loc a (Just b))) = Ref $ Loc (addSheetNameCellRef name a) (Just (addSheetNameCellRef name b))

-- | Add sheet name to CellRef
addSheetNameCellRef name a 
  = case (sheet a) of
    "" -> CellRef name (colNr a) (rowNr a)
    _ -> a

-- | Infinite sequence of letter combinations that identify spreadsheet columns.
columns :: [String]  
columns 
  = [ xs++[x] | xs <- "":columns, x <- ['A'..'Z'] ]

-- | Convert a column name into a column number. For instance, "A" becomes
--   0 and "AA" becomes 26.
toColNr :: String -> Int
toColNr colName
  = head (elemIndices colName columns)

-- | Convert a row name into a row number. For instance, "1" becomes
--   0 and "10" becomes 9.
toRowNr :: String -> Int
toRowNr rowName
  = (read rowName)-1
