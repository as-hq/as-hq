module AS.Dispatch where

import AS.Types
import AS.Parsing
import Import
import qualified AS.Eval.Py as R (evalPy)
import qualified Data.Map as M
import qualified AS.DAG as D
import qualified AS.DB as DB
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Control.Applicative
 
evalCellSeq :: [ASCell] -> Handler [ASValue]
evalCellSeq = evalChain M.empty
  where
    evalChain :: M.Map ASLocation ASValue -> [ASCell] -> Handler [ASValue]
    evalChain _ [] = return []
    evalChain mp (c:cs) = do
      cv <- R.evalPy mp (cellExpression c) --TODO: handle c being range
      $(logInfo) $ (fromString $ show cv)
      let newMp = M.insert (cellLocation c) cv mp
      rest <- evalChain newMp cs
      return (cv:rest)

--TODO change dbGetSetAncestors to have no flattening
evalCells :: [ASLocation] -> Handler (Maybe [ASCell])
evalCells [] = return $ Just []
evalCells locs = do
  ancestors <- fmap (concat . (zipWith (:) locs)) $ D.dbGetSetAncestors locs
  $(logInfo) $ "ancestors computed: " ++ (fromString $ show ancestors)
  cells <- DB.getCells ancestors
  $(logInfo) $ "got cells: " ++ (fromString $ show cells)
  if any isNothing cells
    then return Nothing
    else do
      let filterCells = map (\(Just x) -> x) cells
      $(logInfo) $ "filtered cells: " ++ (fromString $ show filterCells)
      results <- evalCellSeq filterCells
      let ZipList newCells = Cell <$>
                             ZipList (map cellLocation filterCells) <*>
                             ZipList (map cellExpression filterCells) <*>
                             ZipList results
      DB.setCells newCells
      return $ Just newCells

cellValues :: [ASLocation] -> Handler (Maybe (M.Map ASLocation ASValue))
cellValues locs = do
  cells <- DB.getCells locs
  if any isNothing cells
    then return Nothing
    else return $ Just $ M.fromList $ map (\cell -> (cellLocation cell, cellValue cell)) $ map (\(Just x) -> x) $ cells

updateCell :: (ASLocation, ASExpression) -> Handler ()
updateCell (loc, xp) =
  DB.dbUpdateLocationDependencies (loc, deps) >> (DB.setCell $ Cell loc xp (ValueNaN ()))
    where 
      deps = normalizeRanges $ parseDependencies xp

reevaluateCell :: ASLocation -> ASExpression -> Handler (Maybe [ASCell])
reevaluateCell loc xp = do
  let rdeps = normalizeRanges $ parseDependencies xp
  descendants <- fmap concat $ D.dbGetSetDescendants $ [loc]
  evalCells (rdeps ++ [loc] ++ descendants)

propagateCell :: ASLocation -> ASExpression -> Handler (Maybe [ASCell])
propagateCell loc xp = do
  updateCell (loc, xp)
  case loc of
    Range (p1, p2) -> do
      let ele = decomposeLocs loc
      mapM_ (\eleLoc -> updateCell (eleLoc, Expression ((expression xp)++"["++(show $ fromJust $ elemIndex eleLoc ele)++"]"))) ele --TODO expression
    otherwise -> return ()
  reevaluateCell loc xp
{-- TODO
evalRepl :: String -> Handler (Maybe ASValue)
evalRepl xp = cellValues deps >>= ((flip R.evalPy) expr)
  where deps = parseDependencies expr
        expr = Expression xp
        --}
