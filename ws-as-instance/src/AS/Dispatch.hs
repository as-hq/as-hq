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
import AS.Eval.Middleware as EM
import AS.Eval.Endware as EE

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Eval Handler

handleEval :: ASPayload -> ASUser -> IO ASMessage
handleEval (PayloadC cell) user = do
  cell' <- EM.evalMiddleware cell -- middleware for streaming, data integration, excel preevaluation, etc all happen here
  initCells <- getInitialCells cell'
  result <- reevaluateInitialCells initCells
  sendResult result user

sendResult :: (EitherCells, EitherCells) -> ASUser -> IO ASMessage
sendResult ((Right cells),(Right descendants)) user = do
  cells' <- mapM EE.evalEndware cells -- endware for producing tags post-eval (that either kickoff daemons or can be read by frontend), e.g. streaming or styling
  time <- getASTime
  let commit = ASCommit (userId user) descendants cells' time
  DB.pushCommit commit
  putStrLn $ show commit
  return $ Message (userId user) Evaluate Success (PayloadCL cells')
sendResult ((Left e),descendants) user = do 
  failDesc <- case descendants of
    Right c -> generateErrorMessage e
    Left e' -> generateErrorMessage e'
  return $ Message (userId user) Evaluate (Failure failDesc) (PayloadN ())


----------------------------------------------------------------------------------------------------------------------------------------------
-- | Evaluation

getInitialCells :: ASCell -> IO [ASCell]
getInitialCells (Cell loc xp NoValue ts) = do -- case that no middleware produced a value
  let (deps,exprs) = getDependenciesAndExpressions loc xp (getOffsets loc)
  let locs = decomposeLocs loc
  let initCells = map (\(l,e,v)-> Cell l e v ts) (zip3 locs exprs (repeat NoValue))  
  DB.updateDAG (zip deps locs)
  DB.setCells initCells
  printTimed "got initial cells"
  return initCells
getInitialCells (Cell loc xp val ts) = do 
  -- if the first cell has its value set, assume the same value and expression for all decomposed locs
  let locs = decomposeLocs loc
  -- preserve tags
  let initCells = map (\(l,e,v)-> Cell l e v ts) (zip3 locs (repeat xp) (repeat val))  
  DB.setCells initCells
  printTimed "got initial cells"
  return initCells

reevaluateInitialCells :: [ASCell] -> IO (EitherCells,EitherCells)
reevaluateInitialCells initCells = do 
  -- | Get descendants and ancestors (return error if locked)
  let locs = map cellLocation initCells
  dag <- DB.getDAG
  printTimed "got all edges from DB in reevaluateCell"
  let descendantLocs = DAG.descendants locs dag
  printTimed "got descendants from DB in reevaluateCell"
  putStrLn $ "descendant locations: " ++ (show descendantLocs)
  previousCells <- DB.getCells (descendantLocs)
  let ancestorLocs = DAG.immediateAncestors descendantLocs dag
  printTimed "got immediate ancestor locations from DB relations"
  putStrLn $ "immediate ancestor locations " ++ (show ancestorLocs)
  cells <- DB.getCells (descendantLocs ++ ancestorLocs)
  printTimed "done with get cells"
  putStrLn $ "D and A cells: " ++ (show cells)
  let (descendants,ancestors) = L.splitAt (L.length descendantLocs) cells

  -- | Wrap up
  if (L.length ancestors /= L.length ancestorLocs)
    then 
      return (Left DBNothingException,Left DBNothingException) -- user placed a dependency on non-existent cell
    else do
      results <- evalChain (M.fromList (map (\c -> (cellLocation c, cellValue c)) ancestors)) descendants
      printTimed "finished all eval, back in reevaluateCell"
      DB.setCells $ results 
      printTimed "set actual cells in DB"
      return $ (Right previousCells,Right results)

evalChain :: M.Map ASLocation ASValue -> [ASCell] -> IO [ASCell]
evalChain _ [] = return []
evalChain mp ((Cell loc xp NoValue ts):cs) = do  -- case: cell value not yet determined by middleware (streaming etc.)
  printTimed "Starting eval chain"
  putStrLn $ "MAP " ++ (show mp)
  cv <- R.evalExpression loc mp xp 
  otherCells <- additionalCells loc cv 
  let newMp = M.insert loc cv mp
  rest <- evalChain newMp cs
  ret <- case cv of 
    ExcelSheet l e v -> return $ otherCells ++ rest -- don't include the cell itself for excel sheet loading
    otherwise -> return $ [Cell loc xp cv ts] ++ otherCells ++ rest 
  return ret
evalChain mp ((Cell loc xp cv ts):cs) = do
  printTimed "Starting eval chain"
  putStrLn $ "MAP " ++ (show mp)
  otherCells <- additionalCells loc cv 
  let newMp = M.insert loc cv mp
  rest <- evalChain newMp cs
  ret <- case cv of 
    ExcelSheet l e v -> return $ otherCells ++ rest -- don't include the cell itself for excel sheet loading
    otherwise -> return $ [Cell loc xp cv ts] ++ otherCells ++ rest 
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
  let cells = L.tail $ map (\(l,e,v) -> Cell l e v []) (zip3 locs exprs vals)
  DB.updateDAG (zip (repeat [origLoc]) (L.tail locs))
  return cells
    where
      shift (ValueL v) r (a,b) = [(a+c,b+r) | c<-[0..length(v)-1] ]
      shift other r (a,b)  = [(a,b+r)]
    

createExcelCells :: ASValue -> ASLocation -> IO [ASCell]
createExcelCells v l = case v of
  ExcelSheet locs exprs vals -> do
    return [Cell (Index (sheet l) (realLocs!!i)) (Expression (realExprs!!i) Python ) (realVals!!i) [] | i<-[0..length realLocs-1]]
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
  let cells' = map (\loc -> Cell loc (cellExpression cell) val []) locs
  DB.setCells cells'
  return ()
