module AS.Dispatch where

import AS.Types
import Prelude 
import qualified AS.Eval    as R (evalExpression,evalExcel)
import qualified Data.Map   as M
import qualified AS.DAG     as DAG
import qualified AS.DB      as DB
import qualified Data.List  as L (head,last,tail,length,splitAt) 
import AS.Parsing.Common
import AS.Parsing.Out hiding (first)
import AS.Parsing.In
import Data.Maybe (fromJust, isNothing)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Control.Applicative

import Data.Time.Clock
import Data.Text as T (unpack,pack)
import AS.Util

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Eval Handler

handleEval :: ASPayload -> Client -> IO ASMessage
handleEval (PayloadC cell) client = do
  let vWindow = clientVW client
  (descendants, updatedCells) <- propagateCell (cellLocation cell, cellExpression cell)
  result <- case updatedCells of
              Right cells -> do 
                let desc = fromRight descendants
                let commit = ASCommit (T.unpack (clientName client)) desc cells getUpdateTime
                DB.pushCommit commit
                _ <- putStrLn $ show commit
                return $ Message Evaluate Success (PayloadCL cells)
              Left e -> do 
                failDesc <- case descendants of
                  Right c -> generateErrorMessage e
                  Left e' -> generateErrorMessage e'
                return $ Message Evaluate (Failure failDesc) (PayloadN ())
  return result

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Evaluation

propagateCell :: (ASLocation,ASExpression) -> IO (ASEitherCells,ASEitherCells)
propagateCell (loc,xp) = do
  printTime "just started propagate cell"
  if ((language xp)==Excel)
    then do
      newXp <- R.evalExcel xp
      reevaluateCell (loc, newXp) -- deal with expression renaming
      -- return $ Just $ map (\(Cell l (Expression e Excel) v ) -> (Cell l xp v)) (fromJust cells)
    else (reevaluateCell (loc, xp))


reevaluateCell :: (ASLocation, ASExpression) -> IO (ASEitherCells,ASEitherCells)
reevaluateCell (loc, xp) = do 
  -- | Update DAG
  let (deps,exprs) = getDependenciesAndExpressions loc xp (getOffsets loc)
  let locs = decomposeLocs loc
  let initCells = map (\(l,e,v)-> Cell l e v) (zip3 locs exprs (repeat (ValueNaN ()) ))  
  _ <- DB.updateDAG (zip deps locs)
  _ <- printTime "initially updated DAG in reevaluateCell"
  -- | Get descendants and ancestors (return error if locked)
  dag <- DB.getDAG
  let locs = decomposeLocs loc
  printTime "got all edges from DB in reevaluateCell"
  let descendantLocs = DAG.descendants locs dag
  printTime "got descendants from DB in reevaluateCell"
  putStrLn $ "descendant locations: " ++ (show descendantLocs)
  previousCells <- DB.getCells (descendantLocs)
  let ancestorLocs = DAG.immediateAncestors descendantLocs dag
  printTime "got immediate ancestor locations from DB relations"
  putStrLn $ "immediate ancestor locations " ++ (show ancestorLocs)
  _ <- DB.setCells initCells
  cells <- DB.getCells (descendantLocs ++ ancestorLocs)
  printTime "done with get cells"
  putStrLn $ "D and A cells: " ++ (show cells)
  let (descendants,ancestors) = L.splitAt (L.length descendantLocs) cells

  -- | Wrap up
  if (L.length ancestors /= L.length ancestorLocs)
    then 
      return (Left DBNothingException,Left DBNothingException) -- user placed a dependency on non-existent cell
    else do
      results <- evalChain (M.fromList (map (\c -> (cellLocation c, cellValue c)) ancestors)) descendants
      printTime "finished all eval, back in reevaluateCell"
      DB.setCells $ results 
      printTime "set actual cells in DB"
      return $ (Right previousCells,Right results)

evalChain :: M.Map ASLocation ASValue -> [ASCell] -> IO [ASCell]
evalChain _ [] = return []
evalChain mp (c:cs) = do
  printTime "Starting eval chain"
  putStrLn $ "MAP " ++ (show mp)
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


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Deal with primitives

evaluatePrimitive :: ASCell -> IO ASCell
evaluatePrimitive cell = DB.setCell cell >> return cell

insertCellImmediate :: ASCell -> IO ()
insertCellImmediate cell = do
  let val = parseValue (language $ cellExpression cell) ((\(ValueS str) -> str) $ cellValue cell)
  let locs = decomposeLocs (cellLocation cell)
  let cells' = map (\loc -> Cell loc (cellExpression cell) val) locs
  DB.setCells cells'
  return ()
