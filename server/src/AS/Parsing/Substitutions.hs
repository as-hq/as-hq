module AS.Parsing.Substitutions where

import Prelude

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
import AS.Parsing.Excel
import AS.Types.Excel
import AS.Kernels.Excel.Compiler (formula)
import AS.Types.Core
import AS.Util

-------------------------------------------------------------------------------------------------------------------------------------------------
-- General parsing functions

-- | Finds the next parser match that ISN'T in a quoted string, and return 
-- (everything before the match, the match). Assumes you're not starting in the middle of a quote
parseNextUnquoted :: Parser t -> Parser (String, t)
parseNextUnquoted a = do
  r1 <- manyTill (quotedStringEscaped <|> charToStrParser anyChar) (lookAhead $ try a) -- need the try, otherwise it won't work
  r2 <- a -- result of parser a
  return (concat r1, r2)

charToStrParser :: Parser Char -> Parser String
charToStrParser = (fmap (\c -> [c]))

-- | Like quotedString in Util.hs. Matches a quoted string, and returns it exactly. 
-- #needsrefactor this can almost definitely be implemented more cleanly. If you see this
-- and figure out how, please change this and post to #codefeedback. 
quotedStringEscaped :: Parser String
quotedStringEscaped = (quoteString <|> apostropheString)
  where
    quoteString = do 
      char '"'
      body <- many $ escaped <|> (charToStrParser (noneOf ['"']))
      char '"'
      return ("\"" ++ (concat body) ++ "\"")
    apostropheString = do 
      char '\''
      body <- many $ escaped <|> (charToStrParser (noneOf ['\'']))
      char '\''
      return ("'" ++ (concat body) ++ "'")
    escaped = do 
      char '\\' 
      escChar <- choice (zipWith escapedChar codes replacements)
      return ['\\', escChar]
    escapedChar code replacement = char code >> return replacement
    codes            = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
    replacements     = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

-- | Alternatingly gives back matches in string and the surrounding parts of the matches. 
-- e.g., parse (parseMatchesWithContext (P.string "12")) "" "1212ab12" gives
-- Right (["","","ab",""],["12","12","12"]) (alternatingly gives back str)
parseUnquotedMatchesWithContext :: Parser t -> Parser ([String],[t])
parseUnquotedMatchesWithContext a = do
  matchesWithContext <- many $ try $ parseNextUnquoted a
  rest <- many anyChar
  let inter = (map fst matchesWithContext) ++ [rest]
      matches = (map snd matchesWithContext)
  return (inter,matches)

getUnquotedMatchesWithContext :: ASExpression -> Parser t -> ([String],[t])
getUnquotedMatchesWithContext xp p = 
  if (isExcelLiteral)
    then ([str], [])
    else (fromRight . (parse (parseUnquotedMatchesWithContext p) "") $ str)
  where
    lang = xpLanguage xp
    str = xpString xp
    fromRight (Right x) = x
    isExcelLiteral = (lang == Excel) && parsedCorrectly
    parsedCorrectly = case (parse formula "" str) of 
      Right _ -> False 
      Left  _ -> True


-- does no work with Parsec/actual parsing
replaceMatches :: ([String],[t]) -> (t -> String) -> String -> String
replaceMatches (inter,matches) f target = blend inter matchReplacings
  where
    blend [x] _ = x
    blend (x:xs) (y:ys) = x ++  y ++ blend xs ys
    matchReplacings = map f matches


-------------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers

-- | Returns the list of dependencies in ASExpression. 
getDependencies :: ASSheetId -> ASExpression -> [ASReference]
getDependencies sheetid xp = deps
  where
    (_, exRefs) = getUnquotedMatchesWithContext xp refMatch -- the only place that Parsec is used
    deps = map (exRefToASRef sheetid) exRefs

-- | Takes in a list of ExRef's and converts them to a list of ASIndex's.
getASRefsFromExRefs :: ASSheetId -> [ExRef] -> [ASReference]
getASRefsFromExRefs sheetid matches = map (exRefToASRef sheetid) matches


----------------------------------------------------------------------------------------------------------------------------------
-- Copy/paste

-- | Takes in an offset and a cell, and returns the cell you get when you shift the cell by
-- the offset. (The location changes, and the non-absolute references in the expression changes.)
shiftCell :: Offset -> ASCell -> ASCell
shiftCell offset (Cell loc xp@(Expression str lang) v ts) = shiftedCell
  where
    shiftedLoc     = shiftInd offset loc
    (inter,exRefs) = getUnquotedMatchesWithContext xp refMatch
    shiftedExRefs  = shiftExRefs offset exRefs
    newStr         = replaceMatches (inter, shiftedExRefs) show str
    shiftedXp      = Expression newStr lang
    shiftedCell    = Cell shiftedLoc shiftedXp v ts