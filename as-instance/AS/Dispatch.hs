module AS.Dispatch where

import AS.Types
import AS.Parsing
import Import
import Prelude ((!!)) --ADDED FOR RANGES
import qualified AS.Eval as R (evalExpression)
import qualified Data.Map as M
import qualified AS.DAG as D
import qualified AS.DB as DB
import Data.List (elemIndex, tail, init)
import qualified Data.List (head,last) --ADDED FOR RANGES
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Control.Applicative

evalCellSeq :: [ASCell] -> Handler [ASCell]
evalCellSeq = evalChain M.empty

evalChain :: M.Map ASLocation ASValue -> [ASCell] -> Handler [ASCell]
evalChain _ [] = return []
evalChain mp (c:cs) = do
  let xp  = Expression (removeBrackets $ deleteDollars $ expression $ cellExpression c) (language $ cellExpression c) --ADDED FOR RANGES
      loc = cellLocation c
  $(logInfo) $ "EVALPY EXPRESSION: " ++ (fromString $ (expression xp))
  cv <- R.evalExpression mp xp 
  additionalCells <- case loc of
    Index (a, b) -> case cv of
      ValueL lst -> createListCells (Index (a, b)) lst
      otherwise -> return [] 
    otherwise -> return []
  $(logInfo) $ (fromString $ show cv)
  let newMp = M.insert (cellLocation c) cv mp
  rest <- evalChain newMp cs
  return $ [Cell loc (cellExpression c) cv] ++ additionalCells ++ rest --the cell should contain $ signs in expression

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
  ancestors <- fmap reverse $ D.dbGetSetAncestors locs
  $(logInfo) $ "ancestors computed: " ++ (fromString $ show ancestors)
  cells <- DB.getCells ancestors
  locsCells <- DB.getCells locs
  $(logInfo) $ "got cells: " ++ (fromString $ show cells)
  if any isNothing cells
    then return Nothing
    else do
      let filterCells = map (\(Just x) -> x) cells
          filterLocsCells = map (\(Just x) -> x) locsCells
      $(logInfo) $ "filtered cells: " ++ (fromString $ show filterCells)
      results <- evalChain (M.fromList $ map (\c -> (cellLocation c, cellValue c)) $ filterCells) filterLocsCells
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
  case loc of
    Index a -> do
      DB.dbUpdateLocationDependencies (loc, deps) >> (DB.setCell $ Cell loc xp (ValueNaN ()))
      where 
        deps = normalizeRanges $ parseDependencies xp
    Range ((a,b),(c,d)) ->  --CHANGED FOR RANGES
      if (Data.List.head (expression xp) == '{') && (Data.List.last (expression xp) == '}') --check for array formula
        then do 
          $(logInfo) $ "DB ARRAY BITCH"
          let topLeftExpr = Expression (topLeft $ expression xp) (language xp)
          let deps = [fst (parseDependenciesRelative topLeftExpr rowOff colOff) | rowOff<-[0..c-a], colOff<-[0..d-b]]
          let exps = [snd (parseDependenciesRelative topLeftExpr rowOff colOff) | rowOff<-[0..c-a], colOff<-[0..d-b]]
          let locs = [Index (row,col) | row <-[a..c], col<-[b..d]]
          DB.dbUpdateLocationDepsBatch (zip locs deps) >> DB.setCells [Cell (locs!!i) (exps!!i) (ValueNaN ()) | i<-[0..(c-a+1)*(d-b+1)-1]] 
          $(logInfo) $ "DB ARRAY: "++ (fromString $ show exps) --DB expressions have $ signs
        else do 
          $(logInfo) $ "DB DOLLAR BITCH"
          let deps = [fst (parseDependenciesRelative xp rowOff colOff) | rowOff<-[0..c-a], colOff<-[0..d-b]]
          let exps = [snd (parseDependenciesRelative xp rowOff colOff) | rowOff<-[0..c-a], colOff<-[0..d-b]]
          let locs = [Index (row,col) | row <-[a..c], col<-[b..d]]
          DB.dbUpdateLocationDepsBatch (zip locs deps) >> DB.setCells [Cell (locs!!i) (exps!!i) (ValueNaN ()) | i<-[0..(c-a+1)*(d-b+1)-1]] 
          $(logInfo) $ "DB DOLLARS: "++ (fromString $ show exps) --DB expressions have $ signs

reevaluateCell :: (ASLocation, ASExpression) -> Handler (Maybe [ASCell])
reevaluateCell (loc, xp) = do
  descendants <- D.dbGetSetDescendants $ decomposeLocs loc --CHANGED FOR RANGES
  $(logInfo) $ "Descendants being calculated: " ++ (fromString $ show descendants)
  evalCells descendants

propagateCell :: ASLocation -> ASExpression -> Handler (Maybe [ASCell])
propagateCell loc xp = do
  updateCell (loc, xp)
  reevaluateCell (loc, xp)