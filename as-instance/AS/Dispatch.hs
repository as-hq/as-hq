module AS.Dispatch where

import AS.Types
import qualified AS.Eval.Py as R (evalPy)
import qualified Data.Map as M (insert, empty)
import qualified AS.DAG as D
import qualified AS.DB as DB
import Text.ParserCombinators.Parsec

evalCellSeq :: [ASCell] -> IO [ASValue]
evalCellSeq = evalChain M.empty
  where
    evalChain :: M.Map ASLocation ASValue -> [ASCell] -> IO [ASValue]
    evalChain mp (c:cs) = do
      cv <- R.evalPy mp (cellExpression c)
      let newMp = M.insert (cellLocation c) cv mp
      rest <- evalChain newMp cs
      return (cv:rest)

evalCells :: [ASLocation] -> IO [ASCell]
evalCells locs = do
  ancestors <- D.getSetAncestors locs
  cells <- DB.getCells ancestors
  results <- evalCellSeq cells
  let newCells = Cell <$>
                 ZipList (map cellLocation cell) <*>
                 ZipList (map cellExpression cell) <*>
                 ZipList results
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
