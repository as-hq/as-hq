module AS.Dispatch where

import AS.Types
import Prelude 
import qualified AS.Eval    as R (evalExpression,evalExcel)
import qualified Data.Map   as M
import qualified AS.DAG     as DAG
import qualified AS.DB      as DB
import qualified Data.List  as L (head,last,tail,length) 
import AS.Parsing.Common
import AS.Parsing.Out
import AS.Parsing.In
import AS.Util
import Data.Maybe (fromJust, isNothing)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Control.Applicative

import Data.Time.Clock
import Data.Text as T (unpack,pack)

---------------------- handlers ----------------------------------------

handleEval :: ASPayload -> IO ASMessage
handleEval (PayloadC cell) = do
  result <- propagateCell (cellLocation cell) (cellExpression cell)
  case result of 
    Nothing -> return failureMessage
    Just cells -> return $ Message NoAction Success (PayloadCL cells)

---------------------- helpers -----------------------------------------

propagateCell :: ASLocation -> ASExpression -> IO (Maybe [ASCell])
propagateCell loc xp = do
  printTimed "just started propagate cell"
  if ((language xp)==Excel)
    then do
      printTimed "before excel first eval"
      newXp <- R.evalExcel xp
      printTimed "after excel first eval"
      updateCell (loc, newXp) 
      cells <- reevaluateCell (loc, newXp)
      return $ Just $ map (\(Cell l (Expression e Excel) v ) -> (Cell l xp v)) (fromJust cells)
    else updateCell (loc,xp) >> reevaluateCell (loc, xp) 
 
updateCell :: (ASLocation, ASExpression) -> IO ()
updateCell (loc, xp) = do
  let (deps,exprs) = getDependenciesAndExpressions loc xp (getOffsets loc)
  let locs = decomposeLocs loc
  DB.updateDAG (zip deps locs)
  printTimed "updated deps in update cell"
  DB.setCells $ map (\(l,e,v)-> Cell l e v) (zip3 locs exprs (repeat (ValueNaN ()) ))  
  printTimed "set null cells in update cell"
  return ()

reevaluateCell :: (ASLocation, ASExpression) -> IO (Maybe [ASCell])
reevaluateCell (loc, xp) = do
  descendants <- DAG.getDescendants $ decomposeLocs loc
  printTimed "got descendants from DB in reevaluateCell"
  putStrLn $ "descendants: " ++ (show descendants)
  results <- evalCells descendants
  printTimed "finished all eval, back in reevaluateCell"
  DB.setCells $ fromJust results -- set cells here, not in eval cells
  printTimed "set actual cells in DB"
  return results

evalCells :: [ASLocation] -> IO (Maybe [ASCell])
evalCells [] = return $ Just []
evalCells locs = do
  ancestors <- DAG.getImmediateAncestors locs
  printTimed "got immediate ancestor locations from DB relations"
  printTimed $ "ancestors " ++ (show ancestors)
  cells <- DB.getCells ancestors
  printTimed $ "cells " ++ (show cells)
  locsCells <- DB.getCells locs
  printTimed $ "locsCells " ++ (show locsCells)
  printTimed "got ancestor cells"
  if any isNothing cells 
    then return Nothing
    else do
      let filterCells = map (\(Just x) -> x) cells
          filterLocsCells = map (\(Just x) -> x) locsCells
      results <- evalChain (M.fromList $ map (\c -> (cellLocation c, cellValue c)) $ filterCells) filterLocsCells
      return $ Just results

evalChain :: M.Map ASLocation ASValue -> [ASCell] -> IO [ASCell]
evalChain _ [] = return []
evalChain mp (c:cs) = do
  let xp  = cellExpression c
      loc = cellLocation c
  cv <- R.evalExpression loc mp xp 
  otherCells <- additionalCells loc cv 
  let newMp = M.insert (cellLocation c) cv mp
  rest <- evalChain newMp cs
  ret <- case cv of 
    ExcelSheet l e v -> return $ otherCells ++ rest -- don't include the cell itself for excel sheet loading
    otherwise -> return $ [Cell loc (cellExpression c) cv] ++ otherCells ++ rest 
  return ret

additionalCells :: ASLocation -> ASValue -> IO [ASCell]
additionalCells loc cv = do
  listCells <- case loc of
    Index sheet (a, b) -> case cv of
      ValueL lstValues -> createListCells (Index sheet (a, b)) lstValues
      otherwise -> return [] 
    otherwise -> return []
  excelCells <- case cv of 
    ExcelSheet l e v-> createExcelCells cv loc
    otherwise -> return []
  return $ listCells ++ excelCells

-- not currently handling [[[]]] type things
createListCells :: ASLocation -> [ASValue] -> IO [ASCell]
createListCells (Index sheet (a,b)) [] = return []
createListCells (Index sheet (a,b)) values = do 
  let origLoc = Index sheet (a,b)
  let vals = concat $ map lst values
  let locs = map (Index sheet) (concat $ [(shift (values!!row) row (a,b)) | row <- [0..(length values)-1]])
  let exprs = map (\(Index _ (x,y)) -> Reference origLoc (x-a,y-b)) locs
  let cells = L.tail $ map (\(l,e,v) -> Cell l e v) (zip3 locs exprs vals)
  DB.updateDAG (zip (repeat [origLoc]) (L.tail locs))
  return cells
    where
      shift (ValueL v) r (a,b) = [(a+c,b+r) | c<-[0..length(v)-1] ]
      shift other r (a,b)  = [(a,b+r)]
    

createExcelCells :: ASValue -> ASLocation -> IO [ASCell]
createExcelCells v l = case v of
  ExcelSheet locs exprs vals -> do
    return [Cell (Index (sheet l) (realLocs!!i)) (Expression (realExprs!!i) Python) (realVals!!i) | i<-[0..length realLocs-1]]
      where
        realLocs = unpackExcelLocs locs
        realExprs = unpackExcelExprs exprs
        realVals = unpackExcelVals vals
  otherwise -> return []


---------------------- primitives -----------------------------------------------

evaluatePrimitive :: ASCell -> IO ASCell
evaluatePrimitive cell = DB.setCell cell >> return cell

insertCellImmediate :: ASCell -> IO ()
insertCellImmediate cell = do
  let val = parseValue (language $ cellExpression cell) ((\(ValueS str) -> str) $ cellValue cell)
  let locs = decomposeLocs (cellLocation cell)
  let cells' = map (\loc -> Cell loc (cellExpression cell) val) locs
  DB.setCells cells'
  return ()
