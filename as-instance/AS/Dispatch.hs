module AS.Dispatch where

import AS.Types
import Import
import Prelude ((!!)) 
import qualified AS.Eval    as R (evalExpression)
import qualified Data.Map   as M
import qualified AS.DAG     as D
import qualified AS.DB      as DB
import qualified Data.List  as DL (head,last,tail) 
import AS.Parsing.Common
import AS.Parsing.Out
import AS.Parsing.In
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Control.Applicative

propagateCell :: ASLocation -> ASExpression -> Handler (Maybe [ASCell])
propagateCell loc xp = updateCell (loc, xp) >> reevaluateCell (loc, xp)

updateCell :: (ASLocation, ASExpression) -> Handler ()
updateCell (loc, xp) = 
  case loc of
    Index _ _-> do
      DB.dbUpdateLocationDependencies (loc, deps) >> (DB.setCell $ Cell loc xp (ValueNaN ()))
      $(logInfo) $ "(1) updated Index cell"
      where 
        deps = normalizeRanges $ fst $ parseDependenciesRelative loc xp 0 0
    Range sheet ((a,b),(c,d)) ->  do --neglect array formulas for now
          let deps = [fst (parseDependenciesRelative loc xp rowOff colOff) | rowOff<-[0..c-a], colOff<-[0..d-b]]
          let exps = [snd (parseDependenciesRelative loc xp rowOff colOff) | rowOff<-[0..c-a], colOff<-[0..d-b]]
          $(logInfo) $ "range exps: " ++ (fromString $ show exps)
          let locs = [Index sheet (row,col) | row <-[a..c], col<-[b..d]]
          DB.dbUpdateLocationDepsBatch (zip locs deps) >> DB.setCells [Cell (locs!!i) (exps!!i) (ValueNaN ()) | i<-[0..(c-a+1)*(d-b+1)-1]] 
          $(logInfo) $ "(1) updated Range cell"

reevaluateCell :: (ASLocation, ASExpression) -> Handler (Maybe [ASCell])
reevaluateCell (loc, xp) = do
  descendants <- D.dbGetSetDescendants $ decomposeLocs loc
  $(logInfo) $ "Descendants being calculated: " ++ (fromString $ show descendants)
  results <- evalCells descendants
  $(logInfo) $ "(2) cell reevaluated"
  DB.setCells $ fromJust results -- set cells here, not in eval cells
  return results

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
      return $ Just results

evalChain :: M.Map ASLocation ASValue -> [ASCell] -> Handler [ASCell]
evalChain _ [] = return []
evalChain mp (c:cs) = do
  let xp  = cellExpression c
      loc = cellLocation c
  $(logInfo) $ "EVALPY EXPRESSION: " ++ (fromString $ (expression xp))
  cv <- R.evalExpression mp xp loc -- eval expression needs to know current sheet
  additionalCells <- case loc of
    Index sheet (a, b) -> case cv of
      ValueL lst -> createListCells (Index sheet (a, b)) lst
      otherwise -> return [] 
    otherwise -> return []
  excelCells <- case cv of 
    ExcelSheet l e v-> createExcelCells cv loc
    otherwise -> return []
  $(logInfo) $ "Parsing returns: " ++ (fromString $ show cv)
  let newMp = M.insert (cellLocation c) cv mp
  rest <- evalChain newMp cs
  ret <- case cv of 
    ExcelSheet l e v -> return $ additionalCells ++ excelCells ++ rest 
    otherwise -> return $ [Cell loc (cellExpression c) cv] ++ additionalCells ++ excelCells ++ rest 
  return ret

-- way too much non-batching?
createListCells :: ASLocation -> [ASValue] -> Handler [ASCell]
createListCells (Index sheet (a, b)) [] = return []
createListCells (Index sheet (a, b)) (x:xs) =
  case x of
    ValueL lst ->
      do
        cellTriples <- mapM process $ DL.tail matchedFirstRow
        let cells = map (\(cell,_,_)->cell) cellTriples
        DB.setCells cells
        DB.dbUpdateLocationDepsBatch $ map (\(_, loc, origLoc) -> (loc, [origLoc])) cellTriples
        listCellTriplesUnflat <- mapM processList $ DL.tail matched
        let listCellTriples = concat listCellTriplesUnflat
        let listCells = map(\(cell,_,_)->cell) listCellTriples
        DB.setCells listCells
        DB.dbUpdateLocationDepsBatch $ map (\(_, loc, origLoc) -> (loc, [origLoc])) listCellTriples
        return $ cells ++ listCells
      where
        numCols = length lst
        matchedFirstRow = zip [(a + i, b) | i <- [0..numCols-1]] lst
        processList ((col, row), ValueL val) = mapM process $ zip [(col + i, row) | i <- [0..numCols-1]] val
    otherwise -> do
      result <- mapM process $ DL.tail matched
      $(logInfo) $ "created list cell on otherwise"
      return $ map (\(cell,_,_)->cell) result
  where
    process ((col, row), val) = do
      $(logInfo) $ "created list cell"
      let loc  = Index sheet (col, row)
          cell = Cell loc (Reference origLoc (col - a, row - b)) val
      return (cell, loc, origLoc)
    matched  = zip [(a, b + i) | i <- [0..numRows-1]] values
    origLoc  = Index sheet (a, b)
    numRows  = length values
    values   = x:xs

createExcelCells :: ASValue -> ASLocation -> Handler [ASCell]
createExcelCells v l = case v of
  ExcelSheet locs exprs vals -> do
    return [Cell (Index (sheet l) (realLocs!!i)) (Expression (realExprs!!i) Python) (realVals!!i) | i<-[0..length realLocs-1]]
      where
        realLocs = unpackExcelLocs locs
        realExprs = unpackExcelExprs exprs
        realVals = unpackExcelVals vals




