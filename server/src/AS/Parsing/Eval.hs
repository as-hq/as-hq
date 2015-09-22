module AS.Parsing.Eval where

import Prelude
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T
import qualified Data.List as L

import AS.Types.Core
import AS.Eval.Lang as LA
import AS.Parsing.Common as C

printed :: ASLanguage -> Parser String -- TODO generalize beyond python
printed lang = case lang of 
    Python -> do
        manyTill anyChar (try (string "print("))
        arg <- manyTill anyChar (try (char ')'))
        return arg

--consumeMatchedParens :: Int -> Parser (String, String)
--consumeMatchedParens nestLevel = do
--    char '('
--    argFst <- manyTill anyChar (lookAhead (char '('))
--    argMid <- try consumeMatchedParens
--    argSnd <- manyTill anyChar (lookAhead (char ')'))
--    char ')'
--    return $ "(" ++ argFst ++ argMid ++ argSnd ++ ")"

--parensArgument :: Parser String 
--parensArgument = do
--    char '('
--    arg <- try consumeMatchedParens
--    char ')'
--    return arg

replacePrintStmt :: ASLanguage -> Parser (String, String) -- TODO generalize
replacePrintStmt lang = case lang of 
    Python -> do
        prePrint <- manyTill anyChar (try (string "print("))
        arg <- manyTill anyChar (try (char ')' >> spaces >> eof))
        return (prePrint, prePrint ++ "result = " ++ arg)

tryPrintingLast :: ASLanguage -> String -> Either () (String, String)
tryPrintingLast lang str = 
    let 
        (initLines, lastLine) = LA.splitLastCmd lang str
    in if (L.isInfixOf "=" lastLine) || (L.isInfixOf "return" lastLine)
        then (Left ())
        else (Right (C.stripString initLines, LA.printCmd lang lastLine))

-- returns (repl record string, repl eval string)
getReplExpressions :: ASLanguage -> String -> (String, String)
getReplExpressions lang str = readOutput . (parse (replacePrintStmt lang) "") . T.pack $ str
    where 
        readOutput (Right v) = v
        readOutput (Left e) = case (tryPrintingLast lang str) of
            (Right v) -> v
            (Left _) -> (str, str)