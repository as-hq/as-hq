module AS.Parsing.Common where

import Prelude()
import AS.Prelude

import Text.Regex.Posix
import Data.List (elemIndex)
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Either as E
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Applicative hiding ((<|>), many)
import qualified Data.Map as M
import qualified Data.Text.Lazy as LA


import AS.Util
import qualified AS.LanguageDefs as LD

-- takes text, pattern, returns list of occurences
regexList :: String -> String -> [String]
regexList _ "" = []
regexList "" _ = []
regexList text pattern = match:(regexList rest pattern)
  where
    matchTuple = text =~ pattern :: (String, String, String)
    match = (\(_,b,_)->b) matchTuple
    rest = (\(_,_,c)->c) matchTuple

-- replaceSubstrings needs to correctly sort its input
replaceSubstrings :: String -> [(String, String)] -> String
replaceSubstrings "" _ = ""
replaceSubstrings m [] = m
replaceSubstrings m (x:xs) = replaceSubstrings (LA.unpack scrubbed) ys
  where 
    (y:ys)=L.sortBy sortStrList (x:xs) 
    scrubbed = LA.replace (LA.pack (fst y)) (LA.pack (snd y)) (LA.pack m) 
    --replace all occurrences of fst x with snd x in m
    --keep doing this for all (str,str) in list
    --pack: string to text

sortStrList :: (String,String) -> (String,String) -> Ordering
sortStrList (a1, b1) (a2, b2)
  | (show $ dropWhile isUpper a1) < (show $ dropWhile isUpper a2) = GT
  | (show $ dropWhile isUpper a1) > (show $ dropWhile isUpper a2) = LT
  | otherwise = EQ

-------------------------------------------------------------------------------------------------------------------------
-- Parse utils

deleteEmpty = filter ((/=) "")

getDelimitedSubstring :: String -> String -> Int -> String
getDelimitedSubstring str delim n = T.unpack $ (!!) (T.splitOn (T.pack delim) (T.pack str)) n

getLine :: String -> Int -> String
getLine str n = getDelimitedSubstring str "\n" n

stripString :: String -> String
stripString = T.unpack . T.strip . T.pack 

-- note: cannot take empty strings
tryParseList :: Parser a -> [String] -> [Maybe a]
tryParseList p ss = map readOutput parsed
  where
    parsed = map (parse p "") ss
    readOutput (Left err) = Nothing
    readOutput (Right res) = Just res

-- note: can take empty strinsg
tryParseListNonIso :: Parser a -> [String] -> [a]
tryParseListNonIso p ss = E.rights parsed
  where
    ss' = filter ((/=) "") ss
    parsed = map (parse p "") ss'

tryParse p s = parse p "" (T.pack s)

containsAny :: [String] -> String -> Bool
containsAny lst s = any (flip L.isInfixOf s) lst

skip :: Parser a -> Parser String
skip p = p >> (return "")

-------------------------------------------------------------------------------------------------------------------------
-- Value parsers

-- | Because Haskell's float lexer doesn't parse negative floats out of the box. <__<
float' :: Parser Double
float' = fmap rd $ integer <++> decimal <++> exponent
  where (<++>) a b = (++) <$> a <*> b
        (<:>) a b  = (:) <$> a <*> b
        plus       = char '+' *> number
        number     = many1 digit
        minus      = char '-' <:> number
        integer    = plus <|> minus <|> number
        rd         = $read :: String -> Double
        decimal    = char '.' <:> number
        exponent   = option "" $ oneOf "eE" <:> integer

-- | Matches an escaped string and returns the unescaped version. E.g. 
-- "\"hello" -> "hello
quotedString :: Parser String
quotedString = (quoteString <|> apostropheString)
  where
    quoteString      = quotes $ many $ escaped <|> noneOf ['"']
    apostropheString = apostrophes $ many $ escaped <|> noneOf ['\'']
    quotes           = between quote quote
    quote            = char '"' --
    apostrophes      = between apostrophe apostrophe
    apostrophe       = char '\'' -- TODO apostrophes also
    escaped          = char '\\' >> choice (zipWith escapedChar codes replacements)
    escapedChar code replacement = char code >> return replacement
    codes            = ['b',  'n',  'f',  'r',  't',  '\\', '\'', '\"', '/']
    replacements     = ['\b', '\n', '\f', '\r', '\t', '\\', '\'', '\"', '/']

-- | Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- | Match the string 's', accepting either lowercase or uppercase form of each character 
caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

bool :: Parser Bool
bool = LD.readBool <$> (caseInsensitiveString "TRUE" <|> caseInsensitiveString "FALSE")