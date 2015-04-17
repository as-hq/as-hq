module Dispatch where

import qualified Handler.EvalRepl as R (evalPy)
import qualified Data.Map as M (insert, empty)
import qualified Dag as D
import qualified Database as DB
import Text.ParserCombinators.Parsec

toValue :: String -> ASValue

toLocation :: String -> ASLocation
toLocation = parse locationParser
  where
    letterIndex :: Char -> Int
    letterIndex x = ord x - ord 'a'

    locationParser :: Parser (Int, Int)
    locationParser = do
      firstIdx <- fmap letterIndex $ letter
      secondIdx <- fmap read $ many digit
      return (firstIdx, secondIdx)

evalCellSeq :: [ASCell] -> IO [ASValue]
evalCellSeq = evalChain M.empty
  where
    evalChain :: M.Map ASLocation ASValue -> [ASCell] -> IO [ASValue]
    evalChain mp (c:cs) = do
      cv <- R.evalPy mp (expression c)
      let newMp = M.insert (cellLocation c) cv mp
      rest <- evalChain newMp cs
      return (cv:rest)

evalCells :: [ASLocation] -> IO [ASCell]
evalCells locs = do
  ancestors <- D.getSetAncestors locs
  cells <- DB.getCells ancestors
  results <- evalCellSeq cells
  let newCells = zip3 (map cellLocation cell) (map cellExpression cell) results
  DB.saveResults newCells
  return newCells

updateCell :: ASLocation -> ASExpression -> IO [ASCell]
updateCell loc xp = do
  descendants <- D.getSetDescendants [loc]
  cell <- DB.getCell loc
  DB.setCell (loc, xp, nilASValue)
  evalCells descendants

cellValues :: [ASLocation] -> IO (M.Map ASLocation ASValue)
cellValues locs = do
  cells <- DB.getCells locs
  return $ fromList $ map (\cell -> (cellLocation cell, cellValue cell)) cells

evalRepl :: String -> IO ASValue
evalRepl xp = cellValues deps >>= (\vals -> return $ R.evalPy vals expr)
  where deps = cellDependencies expr
        expr = Expression xp
