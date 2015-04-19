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
    evalChain :: M.Map ASLocation ASValue -> [ASCell] -> Handler [ASValue]
    evalChain mp (c:cs) = do
      cv <- R.evalPy mp (cellExpression c)
      let newMp = M.insert (cellLocation c) cv mp
      rest <- evalChain newMp cs
      return (cv:rest)

--TODO change dbGetSetAncestors to have no flattening
evalCells :: [ASLocation] -> Handler (Maybe [ASCell])
evalCells locs = do
  ancestors <- fmap concat $ D.dbGetSetAncestors locs
  cells <- DB.getCells ancestors
  if any isNothing cells
    then return Nothing
    else do
      let filterCells = map (\(Just x) -> x) cells
      results <- evalCellSeq filterCells
      let ZipList newCells = Cell <$>
                             ZipList (map cellLocation filterCells) <*>
                             ZipList (map cellExpression filterCells) <*>
                             ZipList results
      DB.setCells newCells
      return $ Just newCells

updateCell :: ASLocation -> ASExpression -> Handler (Maybe [ASCell])
updateCell loc xp = do
  descendants <- fmap concat $ D.dbGetSetDescendants [loc]
  cell <- DB.getCell loc
  DB.setCell $ Cell loc xp (ValueS "NaN")
  evalCells descendants

cellValues :: [ASLocation] -> Handler (Maybe (M.Map ASLocation ASValue))
cellValues locs = do
  cells <- DB.getCells locs
  if any isNothing cells
    then return Nothing
    else return $ Just $ M.fromList $ map (\cell -> (cellLocation cell, cellValue cell)) $ map (\(Just x) -> x) $ cells

{-- TODO
evalRepl :: String -> Handler (Maybe ASValue)
evalRepl xp = cellValues deps >>= ((flip R.evalPy) expr)
  where deps = parseDependencies expr
        expr = Expression xp
        --}
