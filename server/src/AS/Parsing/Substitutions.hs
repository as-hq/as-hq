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
-- Right (["","","ab",""],["12","12","12"])
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

-- | Reconstructs a string from context (see description in parseUnquotedMatchesWithContext)
blend :: [String] -> [String] -> String
blend [] [] = ""
blend x [] = concat x
blend [] y = concat y
blend (x:xs) (y:ys) = x ++ y ++ (blend xs ys)

replaceRefs :: (ExRef -> String) -> ASExpression -> ASExpression
replaceRefs f xp = xp'
  where 
    (inter, exRefs) = getUnquotedMatchesWithContext xp refMatch
    exRefs'         = map f exRefs 
    expression'     = blend inter exRefs'
    xp' = case xp of
      Expression _ lang -> Expression expression' lang
      Coupled _ l t r   -> Coupled expression' l t r

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
-- Copy/paste and Cut/paste

-- | Takes in an offset and a cell, and returns the cell you get when you shift the cell by
-- the offset. (The location changes, and the non-absolute references in the expression changes.)
shiftCell :: Offset -> ASCell -> ASCell
shiftCell offset (Cell loc xp v ts) = cell'
  where
    loc'  = shiftInd offset loc
    xp'   = replaceRefs (show . (shiftExRef offset)) xp
    cell' = Cell loc' xp' v ts

-- | Offsets all the references in an expression that are contained in the cut range (passed in as 
-- the argument "from"). 
shiftExpressionForCut :: ASRange -> Offset -> ASExpression -> ASExpression
shiftExpressionForCut from offset xp = xp' 
  where 
    fromSid     = rangeSheetId from
    shouldShift = (rangeContainsRef from) . (exRefToASRef fromSid)
    shiftFunc   = \ref -> if (shouldShift ref) then (shiftExRefForced offset ref) else ref
    xp'         = replaceRefs (show . shiftFunc) xp

-- | Shift the cell's location, and shift all references satisfying the condition passed in. 
replaceCellLocs :: (ASIndex -> ASIndex) -> ASCell -> ASCell
replaceCellLocs f c = c { cellLocation = f $ cellLocation c }

replaceCellExpressions :: (ASExpression -> ASExpression) -> ASCell -> ASCell
replaceCellExpressions f c = c { cellExpression = f $ cellExpression c }