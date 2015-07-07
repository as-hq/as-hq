module AS.Dispatch where

import AS.Types
import Import
import Prelude ((!!)) 
import qualified AS.Eval    as R (evalExpression,evalExcel)
import qualified Data.Map   as M
import qualified AS.DAG     as D
import qualified AS.DB      as DB
import qualified Data.List  as L (head,last,tail,length) 
import AS.Parsing.Common
import AS.Parsing.Out
import AS.Parsing.In
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Control.Applicative

import Data.Time.Clock
import Data.Text as T (unpack,pack)



propagateCell :: ASLocation -> ASExpression -> Handler (Maybe [ASCell])
propagateCell loc xp = do
  if ((language xp)==Excel)
    then do
      time <- liftIO (getCurrentTime >>= return . utctDayTime)
      $logInfo $ "before eval excel " ++ (fromString $ show time)

      newXp <- R.evalExcel xp

      time <- liftIO (getCurrentTime >>= return . utctDayTime)
      $logInfo $ "after eval excel " ++ (fromString $ show time)

      updateCell (loc, newXp) 
      $(logInfo) $ "new excel xp: " ++ (fromString $ show newXp)
      time <- liftIO (getCurrentTime >>= return . utctDayTime)
      $logInfo $ "new cell xp " ++ (fromString $ show time)

      cells <- reevaluateCell (loc, newXp)
      return $ Just $ map (\(Cell l (Expression e Excel) v ) -> (Cell l xp v)) (fromJust cells)
    else updateCell (loc,xp) >> reevaluateCell (loc, xp) 

updateCell :: (ASLocation, ASExpression) -> Handler ()
updateCell (loc, xp) = do
  let offsets = getOffsets loc
  let (deps,exprs) = getDependenciesAndExpressions loc xp offsets
  let locs = decomposeLocs loc

  time <- liftIO (getCurrentTime >>= return . utctDayTime)
  $logInfo $ "starting db update " ++ (fromString $ show time)

  DB.dbUpdateLocationDepsBatch (zip locs deps)
  DB.setCells $ map (\(l,e,v)-> Cell l e v) (zip3 locs exprs (repeat (ValueNaN ()) ))  

  time <- liftIO (getCurrentTime >>= return . utctDayTime)
  $logInfo $ "done with db update " ++ (fromString $ show time)

  $(logInfo) $ "updated cell"

reevaluateCell :: (ASLocation, ASExpression) -> Handler (Maybe [ASCell])
reevaluateCell (loc, xp) = do
  descendants <- D.dbGetSetDescendants $ decomposeLocs loc
  time <- liftIO (getCurrentTime >>= return . utctDayTime)
  $logInfo $ "calculated descendants " ++ (fromString $ show time)

  results <- evalCells descendants

  time <- liftIO (getCurrentTime >>= return . utctDayTime)
  $logInfo $ "cell reevaluated " ++ (fromString $ show time)

  DB.setCells $ fromJust results -- set cells here, not in eval cells

  time <- liftIO (getCurrentTime >>= return . utctDayTime)
  $logInfo $ "done with set cells " ++ (fromString $ show time)
  return results

evalCells :: [ASLocation] -> Handler (Maybe [ASCell])
evalCells [] = return $ Just []
evalCells locs = do
  ancestors <- fmap reverse $ D.dbGetSetAncestors locs
  time <- liftIO (getCurrentTime >>= return . utctDayTime)
  $logInfo $ "ancestors computed " ++ (fromString $ show time)

  cells <- DB.getCells ancestors
  locsCells <- DB.getCells locs

  time <- liftIO (getCurrentTime >>= return . utctDayTime)
  $logInfo $ "got ancestors from db " ++ (fromString $ show time)

  $(logInfo) $ "got cells: " -- ++ (fromString $ show cells)
  if any isNothing cells -- needed to ensure correct order
    then return Nothing
    else do
      let filterCells = map (\(Just x) -> x) cells
          filterLocsCells = map (\(Just x) -> x) locsCells
      $(logInfo) $ "filtered cells: " --  ++ (fromString $ show filterCells)
      results <- evalChain (M.fromList $ map (\c -> (cellLocation c, cellValue c)) $ filterCells) filterLocsCells
      return $ Just results

evalChain :: M.Map ASLocation ASValue -> [ASCell] -> Handler [ASCell]
evalChain _ [] = return []
evalChain mp (c:cs) = do
  let xp  = cellExpression c
      loc = cellLocation c
  $(logInfo) $ "EVALPY EXPRESSION: " ++ (fromString $ (expression xp))
  cv <- R.evalExpression loc mp xp -- eval expression needs to know current sheet
  -- $(logInfo) $ "Parsing returns: " ++ (fromString $ show cv)
  otherCells <- additionalCells loc cv 
  let newMp = M.insert (cellLocation c) cv mp
  rest <- evalChain newMp cs
  $(logInfo) $ "rest: " -- ++ (fromString $ show rest)
  ret <- case cv of 
    ExcelSheet l e v -> return $ otherCells ++ rest -- don't include the cell itself for excel sheet loading
    otherwise -> return $ [Cell loc (cellExpression c) cv] ++ otherCells ++ rest 
  $(logInfo) $ "done cells: " -- ++ (fromString $ show ret)
  return ret

additionalCells :: ASLocation -> ASValue -> Handler [ASCell]
additionalCells loc cv = do
  listCells <- case loc of
    Index sheet (a, b) -> case cv of
      ValueL lstValues -> createListCells (Index sheet (a, b)) lstValues
      otherwise -> return [] 
    otherwise -> return []
  $(logInfo) $ "list cells: " -- ++ (fromString $ show listCells)
  excelCells <- case cv of 
    ExcelSheet l e v-> createExcelCells cv loc
    otherwise -> return []
  return $ listCells ++ excelCells

-- not currently handling [[[]]] type things
createListCells :: ASLocation -> [ASValue] -> Handler [ASCell]
createListCells (Index sheet (a,b)) [] = return []
createListCells (Index sheet (a,b)) values = do 
  let origLoc = Index sheet (a,b)
  let vals = concat $ map lst values
  $(logInfo) $ "Vals: " -- ++ (fromString $ show vals)
  let locs = map (Index sheet) (concat $ [(shift (values!!row) row (a,b)) | row <- [0..(length values)-1]])
  $(logInfo) $ "Locs: " -- ++ (fromString $ show locs)
  let exprs = map (\(Index _ (x,y)) -> Reference origLoc (x-a,y-b)) locs
  let cells = L.tail $ map (\(l,e,v) -> Cell l e v) (zip3 locs exprs vals)
  $(logInfo) $ "Cells: " -- ++ (fromString $ show cells)
  -- DB.dbUpdateLocationDepsBatch (zip (L.tail locs) (repeat [origLoc]))
  return cells
  where
    shift (ValueL v) r (a,b) = [(a+c,b+r) | c<-[0..length(v)-1] ]
    shift other r (a,b)  = [(a,b+r)]

-- TODO: batch more. There should be NO setcells, and only one dbUpdate deps
createListCells' :: ASLocation -> [ASValue] -> Handler [ASCell]
createListCells' (Index sheet (a, b)) [] = return []
createListCells' (Index sheet (a, b)) (x:xs) =
  case x of
    ValueL lst ->
      do
        cellTriples <- mapM process $ L.tail matchedFirstRow
        let cells = map (\(cell,_,_)->cell) cellTriples
        -- DB.setCells cells
        DB.dbUpdateLocationDepsBatch $ map (\(_, loc, origLoc) -> (loc, [origLoc])) cellTriples
        listCellTriplesUnflat <- mapM processList $ L.tail matched
        let listCellTriples = concat listCellTriplesUnflat
        let listCells = map(\(cell,_,_)->cell) listCellTriples
        -- DB.setCells listCells
        DB.dbUpdateLocationDepsBatch $ map (\(_, loc, origLoc) -> (loc, [origLoc])) listCellTriples
        return $ cells ++ listCells
      where
        numCols = length lst
        matchedFirstRow = zip [(a + i, b) | i <- [0..numCols-1]] lst
        processList ((col, row), ValueL val) = mapM process $ zip [(col + i, row) | i <- [0..numCols-1]] val
    otherwise -> do
      result <- mapM process $ L.tail matched
      -- $(logInfo) $ "created list cell on otherwise"
      return $ map (\(cell,_,_)->cell) result
  where
    process ((col, row), val) = do
      -- $(logInfo) $ "created list cell"
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
  otherwise -> return []


----------------------

evaluatePrimitive :: ASCell -> Handler ASCell
evaluatePrimitive cell = DB.setCell cell >> return cell