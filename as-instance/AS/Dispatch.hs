module AS.Dispatch where

import AS.Types
import AS.Parsing
import Import
import qualified AS.Eval.Py as R (evalPy)
import qualified Data.Map as M
import qualified AS.DAG as D
import qualified AS.DB as DB
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Control.Applicative
 
evalCellSeq :: [ASCell] -> Handler [ASValue]
evalCellSeq = evalChain M.empty
  where
    evalChain :: M.Map ASLocation ASValue -> [ASCell] -> IO [ASValue]
    evalChain mp (c:cs) = do
      cv <- R.evalPy mp (cellExpression c)
      let newMp = M.insert (cellLocation c) cv mp
      rest <- evalChain newMp cs
      return (cv:rest)

evalCells :: [ASLocation] -> Handler [ASCell]
evalCells locs = do
  ancestors <- D.getSetAncestors locs
  cells <- DB.getCells ancestors
  results <- evalCellSeq cells
  let newCells = Cell <$>
                 ZipList (map cellLocation cells) <*>
                 ZipList (map cellExpression cells) <*>
                 ZipList results
  DB.setCells newCells
  return newCells

updateCell :: ASLocation -> ASExpression -> Handler [ASCell]
updateCell loc xp = do
  descendants <- D.getSetDescendants [loc]
  cell <- DB.getCell loc
  DB.setCell . Cell loc xp (ValueS "NaN")
  evalCells descendants

cellValues :: [ASLocation] -> Handler (M.Map ASLocation ASValue)
cellValues locs = do
  cells <- DB.getCells locs
  return $ fromList $ map (\cell -> (cellLocation cell, cellValue cell)) cells

evalRepl :: String -> Handler ASValue
evalRepl xp = cellValues deps >>= (\vals -> return $ R.evalPy vals expr)
  where deps = parseDependencies expr
        expr = Expression xp
