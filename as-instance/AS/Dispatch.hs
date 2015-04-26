module AS.Dispatch where

import AS.Types
import AS.Parsing
import Import
import qualified AS.Eval.Py as R (evalExpression)
import qualified Data.Map as M
import qualified AS.DAG as D
import qualified AS.DB as DB
import Data.List (elemIndex, tail)
import qualified Data.List (head)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Control.Applicative

evalCellSeq :: [ASCell] -> Handler [ASCell]
evalCellSeq = evalChain M.empty
  where
    evalChain :: M.Map ASLocation ASValue -> [ASCell] -> Handler [ASCell]
    evalChain _ [] = return []
    evalChain mp (c:cs) = do
      let xp  = cellExpression c
          loc = cellLocation c
      cv <- R.evalExpression mp xp
      additionalCells <- case loc of
        Index (a, b) -> case cv of
          ValueL lst -> createListCells (Index (a, b)) lst
          otherwise -> return []
        otherwise -> return []
      $(logInfo) $ (fromString $ show cv)
      let newMp = M.insert (cellLocation c) cv mp
      rest <- evalChain newMp cs
      return $ [Cell loc xp cv] ++ additionalCells ++ rest

createListCells :: ASLocation -> [ASValue] -> Handler [ASCell]
createListCells (Index (a, b)) [] = return []
createListCells (Index (a, b)) (x:xs) =
  case x of
    ValueL lst ->
      do
        a <- mapM process $ tail matchedFirstRow
        b <- mapM processList $ tail matched
        return $ a ++ concat b
      where
        numCols = length lst
        matchedFirstRow = zip [(a + i, b) | i <- [0..numCols-1]] lst
        processList ((col, row), ValueL val) = mapM process $ zip [(col + i, row) | i <- [0..numCols-1]] val
    otherwise ->
      mapM process $ tail matched
  where
    process ((col, row), val) = do
      let loc  = Index (col, row)
          cell = Cell loc (Reference origLoc (col - a, row - b)) val
      DB.dbUpdateLocationDependencies (loc, [origLoc])
      DB.setCell cell
      return cell
    matched  = zip [(a, b + i) | i <- [0..numRows-1]] values
    origLoc  = Index (a, b)
    numRows  = length values
    values   = x:xs


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
      DB.setCells results
      return $ Just results

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

createRangeCells :: (ASLocation, ASExpression) -> Handler ()
createRangeCells (loc, xp) =
  case loc of
    Range (p1, p2) -> do
      let ele = decomposeLocs loc
      mapM_ (\eleLoc -> updateCell (eleLoc, Expression ((expression xp)++"["++(show $ fromJust $ elemIndex eleLoc ele)++"]"))) ele --TODO expression
    otherwise -> return ()

reevaluateCell :: (ASLocation, ASExpression) -> Handler (Maybe [ASCell])
reevaluateCell (loc, xp) = do
  let rdeps = normalizeRanges $ parseDependencies xp
  descendants <- fmap concat $ D.dbGetSetDescendants $ [loc]
  evalCells (rdeps ++ [loc] ++ descendants)

propagateCell :: ASLocation -> ASExpression -> Handler (Maybe [ASCell])
propagateCell loc xp = do
  updateCell (loc, xp)
  createRangeCells (loc, xp)
  reevaluateCell (loc, xp)

{-- TODO
evalRepl :: String -> Handler (Maybe ASValue)
evalRepl xp = cellValues deps >>= ((flip R.evalPy) expr)
  where deps = parseDependencies expr
        expr = Expression xp
        --}
