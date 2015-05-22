module AS.Dispatch where

import AS.Types
import Import
import Prelude ((!!)) --ADDED FOR RANGES
import qualified AS.Eval    as R (evalExpression)
import qualified Data.Map   as M
import qualified AS.DAG     as D
import qualified AS.DB      as DB
import qualified Data.List  as DL (head,last,tail) --ADDED FOR RANGES
import AS.Parsing.Common
import AS.Parsing.Out
import AS.Parsing.In
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Control.Applicative

propagateCell :: ASLocation -> ASExpression -> Handler [ASCell]
propagateCell loc xp = updateCell (loc, xp) >> reevaluateCell (loc, xp)


updateCell :: (ASLocation, ASExpression) -> Handler ()
updateCell (loc, xp) = 
  case loc of
    Index _ -> do
      DB.dbUpdateLocationDependencies (loc, deps) >> (DB.setCell $ Cell loc xp (ValueNaN ()))
      $(logInfo) $ "(1) updated Index cell"
      where 
        deps = normalizeRanges $ parseDependencies xp
    Range ((a,b),(c,d)) ->  --CHANGED FOR RANGES
      if (DL.head (expression xp) == '{') && (DL.last (expression xp) == '}') --check for array formula
        then do 
          let topLeftExpr = Expression (topLeft $ expression xp) (language xp)
          let deps = [fst (parseDependenciesRelative topLeftExpr rowOff colOff) | rowOff<-[0..c-a], colOff<-[0..d-b]]
          let exps = [snd (parseDependenciesRelative topLeftExpr rowOff colOff) | rowOff<-[0..c-a], colOff<-[0..d-b]]
          let locs = [Index (row,col) | row <-[a..c], col<-[b..d]]
          DB.dbUpdateLocationDepsBatch (zip locs deps) >> DB.setCells [Cell (locs!!i) (exps!!i) (ValueNaN ()) | i<-[0..(c-a+1)*(d-b+1)-1]] 
          -- $(logInfo) $ "DB ARRAY: "++ (fromString $ show exps) --DB expressions have $ signs
          $(logInfo) $ "(1) updated Range cell"
        else do 
          let deps = [fst (parseDependenciesRelative xp rowOff colOff) | rowOff<-[0..c-a], colOff<-[0..d-b]]
          let exps = [snd (parseDependenciesRelative xp rowOff colOff) | rowOff<-[0..c-a], colOff<-[0..d-b]]
          let locs = [Index (row,col) | row <-[a..c], col<-[b..d]]
          DB.dbUpdateLocationDepsBatch (zip locs deps) >> DB.setCells [Cell (locs!!i) (exps!!i) (ValueNaN ()) | i<-[0..(c-a+1)*(d-b+1)-1]] 
          -- $(logInfo) $ "DB DOLLARS: "++ (fromString $ show exps) --DB expressions have $ signs
          $(logInfo) $ "(1) updated Range cell"


reevaluateCell :: (ASLocation, ASExpression) -> Handler [ASCell]
reevaluateCell (loc, xp) = do
  descendants <- D.dbGetSetDescendants $ decomposeLocs loc --CHANGED FOR RANGES
  $(logInfo) $ "Descendants being calculated: " ++ (fromString $ show descendants)
  result <- evalCells descendants
  $(logInfo) $ "(2) cell reevaluated"
  return result


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
      ValueL lst -> do 
        $(logInfo) $ "Creating all list cells"
        result <- createListCells (Index (a, b)) lst
        $(logInfo) $ "Finished creating list cells"
        return result
      otherwise -> return [] 
    otherwise -> return []
  $(logInfo) $ "Parsing returns: " ++ (fromString $ show cv)
  let newMp = M.insert (cellLocation c) cv mp
  rest <- evalChain newMp cs
  return $ [Cell loc (cellExpression c) cv] ++ additionalCells ++ rest --the cell should contain $ signs in expression


createListCells :: ASLocation -> [ASValue] -> Handler [ASCell]
createListCells (Index (a, b)) [] = return []
createListCells (Index (a, b)) (x:xs) =
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
      let loc  = Index (col, row)
          cell = Cell loc (Reference origLoc (col - a, row - b)) val
      return (cell, loc, origLoc)
    matched  = zip [(a, b + i) | i <- [0..numRows-1]] values
    origLoc  = Index (a, b)
    numRows  = length values
    values   = x:xs


--TODO change dbGetSetAncestors to have no flattening
evalCells :: [ASLocation] -> Handler [ASCell]
evalCells [] = return []
evalCells locs = do
  ancestors <- fmap reverse $ D.dbGetSetAncestors locs
  $(logInfo) $ "ancestors computed: " ++ (fromString $ show ancestors)
  cells <- DB.getCells ancestors
  locsCells <- DB.getCells locs
  $(logInfo) $ "got cells: " ++ (fromString $ show cells)
  results <- evalChain (M.fromList $ map (\c -> (cellLocation c, cellValue c)) $ cells) locsCells
  DB.setCells results
  return results


cellValues :: [ASLocation] -> Handler (M.Map ASLocation ASValue)
cellValues locs = do
  cells <- DB.getCells locs
  return $ M.fromList $ map (\cell -> (cellLocation cell, cellValue cell)) cells



