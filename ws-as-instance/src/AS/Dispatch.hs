module AS.Dispatch where
import Prelude 

import AS.Types
import qualified AS.Eval    as R (evalExpression,evalExcel)
import qualified AS.DAG     as D
import qualified AS.DB      as DB
import AS.Parsing.Common
import AS.Parsing.Out
import AS.Parsing.In
import AS.Util

import Data.Maybe (fromJust, isNothing)
import qualified Data.Map   as M
import qualified Data.List  as L (head,last,tail,length) 
import Data.Text as T (unpack,pack)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Control.Applicative
import Data.Time.Clock

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
  if ((language xp)==Excel)
    then do
      printTimed "EVAL Excel"
      newXp <- R.evalExcel xp
      updateCell (loc, newXp) 

      cells <- reevaluateCell (loc, newXp)
      printTimed "EVAL excel finished"
      return $ Just $ map (\(Cell l (Expression e Excel) v ) -> (Cell l xp v)) (fromJust cells)
    else updateCell (loc,xp) >> reevaluateCell (loc, xp) 
 
updateCell :: (ASLocation, ASExpression) -> IO ()
updateCell (loc, xp) = do
  let offsets = getOffsets loc
  let (deps,exprs) = getDependenciesAndExpressions loc xp offsets
  let locs = decomposeLocs loc

  printTimed "DB starting update"
  DB.dbUpdateLocationDepsBatch (zip locs deps)
  DB.setCells $ map (\(l,e,v)-> Cell l e v) (zip3 locs exprs (repeat (ValueNaN ()) ))  

  printTimed "DB finished update"
  return ()

reevaluateCell :: (ASLocation, ASExpression) -> IO (Maybe [ASCell])
reevaluateCell (loc, xp) = do
  descendants <- D.dbGetSetDescendants $ decomposeLocs loc
  printTimed "DAG computed descendants"
  
  results <- evalCells descendants
  printTimed "EVAL cell reevaluated"

  DB.setCells $ fromJust results -- set cells here, not in eval cells
  printTimed "DB set cells finished"
  return results

evalCells :: [ASLocation] -> IO (Maybe [ASCell])
evalCells [] = return $ Just []
evalCells locs = do
  ancestors <- fmap reverse $ D.dbGetSetAncestors locs
  printTimed "DAG ancestors computed"

  cells <- DB.getCells ancestors
  locsCells <- DB.getCells locs
  printTimed "DB ancestors retrieved"

  if any isNothing cells -- needed to ensure correct order
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
  cv <- R.evalExpression loc mp xp -- eval expression needs to know current sheet
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
    ExcelSheet l e v -> return $ createExcelCells cv loc
    otherwise -> return []
  return $ listCells ++ excelCells

-- not currently handling [[[]]] type things
createListCells :: ASLocation -> [ASValue] -> IO [ASCell]
createListCells (Index sheet (a,b)) [] = return []
createListCells (Index sheet (a,b)) values = 
  let
    shift (ValueL v) r (a,b) = [(a+c,b+r) | c<-[0..length(v)-1] ]
    shift other r (a,b)  = [(a,b+r)]
    origLoc = Index sheet (a,b)
    vals = concat $ map lst values
    locs = map (Index sheet) (concat $ [(shift (values!!row) row (a,b)) | row <- [0..(length values)-1]])
    exprs = map (\(Index _ (x,y)) -> Reference origLoc (x-a,y-b)) locs
    cells = L.tail $ map (\(l,e,v) -> Cell l e v) (zip3 locs exprs vals)
  in do
    DB.dbUpdateLocationDepsBatch (zip (L.tail locs) (repeat [origLoc]))
    return cells

createExcelCells :: ASValue -> ASLocation -> [ASCell]
createExcelCells v l = case v of
  ExcelSheet locs exprs vals -> [Cell (Index (sheet l) (realLocs!!i)) (Expression (realExprs!!i) Python) (realVals!!i) | i<-[0..length realLocs-1]]
    where
      realLocs = unpackExcelLocs locs
      realExprs = unpackExcelExprs exprs
      realVals = unpackExcelVals vals
  otherwise -> []


---------------------- primitives -----------------------------------------------

evaluatePrimitive :: ASCell -> IO ASCell
evaluatePrimitive cell = DB.setCell cell >> return cell

insertCellImmediate :: ASCell -> IO ()
insertCellImmediate cell = 
  let
    val = parseValue (language $ cellExpression cell) ((\(ValueS str) -> str) $ cellValue cell)
    locs = decomposeLocs (cellLocation cell)
    cells' = map (\loc -> Cell loc (cellExpression cell) val) locs
  in do
    DB.setCells cells' 
    printTimed "DB primitive inserted"
    return ()
