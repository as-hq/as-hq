module AS.Dispatch where

import AS.Types
import Prelude 
import qualified AS.Eval    as R (evalExpression,evalExcel)
import qualified Data.Map   as M
import qualified AS.DAG     as D
import qualified AS.DB      as DB
import qualified Data.List  as L (head,last,tail,length) 
import AS.Parsing.Common
import AS.Parsing.Out
import AS.Parsing.In
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
  if ((language xp)==Excel)
    then do
      time <- (getCurrentTime >>= return . utctDayTime)

      newXp <- R.evalExcel xp

      time <- (getCurrentTime >>= return . utctDayTime)

      updateCell (loc, newXp) 
      time <- (getCurrentTime >>= return . utctDayTime)

      cells <- reevaluateCell (loc, newXp)
      return $ Just $ map (\(Cell l (Expression e Excel) v ) -> (Cell l xp v)) (fromJust cells)
    else updateCell (loc,xp) >> reevaluateCell (loc, xp) 
 
updateCell :: (ASLocation, ASExpression) -> IO ()
updateCell (loc, xp) = do
  let offsets = getOffsets loc
  let (deps,exprs) = getDependenciesAndExpressions loc xp offsets
  let locs = decomposeLocs loc

  time <- (getCurrentTime >>= return . utctDayTime)

  DB.dbUpdateLocationDepsBatch (zip locs deps)
  DB.setCells $ map (\(l,e,v)-> Cell l e v) (zip3 locs exprs (repeat (ValueNaN ()) ))  

  time <- (getCurrentTime >>= return . utctDayTime)
  return ()

reevaluateCell :: (ASLocation, ASExpression) -> IO (Maybe [ASCell])
reevaluateCell (loc, xp) = do
  descendants <- D.dbGetSetDescendants $ decomposeLocs loc
  time <- (getCurrentTime >>= return . utctDayTime)

  results <- evalCells descendants

  time <- (getCurrentTime >>= return . utctDayTime)

  DB.setCells $ fromJust results -- set cells here, not in eval cells

  time <- (getCurrentTime >>= return . utctDayTime)
  return results

evalCells :: [ASLocation] -> IO (Maybe [ASCell])
evalCells [] = return $ Just []
evalCells locs = do
  ancestors <- fmap reverse $ D.dbGetSetAncestors locs
  time <- (getCurrentTime >>= return . utctDayTime)

  cells <- DB.getCells ancestors
  locsCells <- DB.getCells locs

  time <- (getCurrentTime >>= return . utctDayTime)

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
  -- $(logInfo) $ "Parsing returns: " ++ (fromString $ show cv)
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
  DB.dbUpdateLocationDepsBatch (zip (L.tail locs) (repeat [origLoc]))
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


----------------------

evaluatePrimitive :: ASCell -> IO ASCell
evaluatePrimitive cell = DB.setCell cell >> return cell

insertCellImmediate :: ASCell -> IO ()
insertCellImmediate cell = do
  let val = parseValue (language $ cellExpression cell) ((\(ValueS str) -> str) $ cellValue cell)
  let locs = decomposeLocs (cellLocation cell)
  let cells' = map (\loc -> Cell loc (cellExpression cell) val) locs
  DB.setCells cells'
  return ()
