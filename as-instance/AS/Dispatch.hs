module AS.Dispatch where

import AS.Types
import AS.Parsing
import Import
import qualified AS.Eval.Py as R (evalPy)
import qualified Data.Map as M
import qualified AS.DAG as D
import qualified AS.DB as DB
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Control.Applicative
 
evalCellSeq :: [ASCell] -> Handler [ASValue]
evalCellSeq = evalChain M.empty
  where
    evalChain :: M.Map ASLocation ASValue -> [ASCell] -> Handler [ASValue]
    evalChain _ [] = return []
    evalChain mp (c:cs) = do
      cv <- R.evalPy mp (cellExpression c) --TODO: handle c being range
      $(logInfo) $ (fromString $ show cv)
      let newMp = M.insert (cellLocation c) cv mp
      rest <- evalChain newMp cs
      return (cv:rest)

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
      let ZipList newCells = Cell <$>
                             ZipList (map cellLocation filterCells) <*>
                             ZipList (map cellExpression filterCells) <*>
                             ZipList results
      DB.setCells newCells
      return $ Just newCells

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

updateSubCell :: (ASLocation, ASExpression) -> Handler ()
updateSubCell (loc, xp) = 
  DB.dbUpdateLocationDependencies (loc, deps) >> (DB.setCell $ Cell loc xp (ValueNaN ()))
    where 
      deps = parseDependencies xp -- not normalizing since subcells should point to range, not everything in the range

createRangeSubCells :: (ASLocation, ASExpression) -> Handler ()
createRangeSubCells (loc, xp) =
  case loc of
    Range (a,b) -> do
      let eleLocs = decomposeLocs loc
      -- TODO: another updateCell here for the entire range
      -- but, already done in propagateCell
      mapM_ putElement eleLocs 
        where
          putElement x = updateSubCell (Index x, Expression $ (expression xp) ++ (idxRep $ Index x))
          idxRep x
            | fst a == fst b = "["++(show $ snd diff)++"]"
            | otherwise      = "["++ (show $ fst diff)++"]["++(show $ snd diff)++"]"
              where
                diff = rangeDiff (Index a) x
    otherwise -> return ()


reevaluateCell :: (ASLocation, ASExpression) -> Handler (Maybe [ASCell])
reevaluateCell (loc, xp) = do
  let rdeps = normalizeRanges $ parseDependencies xp
  --double counting loc in list of descendents, it seems
  --fix descendants because descendants is defined in DAG
  descendants' <- fmap concat $ D.dbGetSetDescendants $ [loc]
  mCellResults <- evalCells (rdeps ++ [loc] ++ descendants')
  case mCellResults of 
    Just cellResults -> do
      case loc of 
        Index a -> do
          let rangeDist = maxRangeDiff $ map cellLocation cellResults
          let rng = Range (a, (fst a + fst rangeDist, snd a + snd rangeDist))
          createRangeSubCells (rng, xp) 
          let rangeCell = Cell rng xp (ValueLD $ map (\(ValueD d) -> d) $ map cellValue cellResults)
          updateCell (rng, xp)
          -- TODO: get this range cell
          -- TODO: put the value we got in cellResults into this range cell
          -- TODO: return these results
        otherwise -> return ()
      return $ Just cellResults
    Nothing -> return Nothing
  -- here, check if evalCells actually returns a list, but our loc is only an index. if it is, then create megacell, set expression to xp,
    -- and call rangeCellCreate

propagateCell :: ASLocation -> ASExpression -> Handler (Maybe [ASCell])
propagateCell loc xp = do
  updateCell (loc, xp)
  createRangeSubCells (loc, xp)
  reevaluateCell (loc, xp)
{-- TODO
evalRepl :: String -> Handler (Maybe ASValue)
evalRepl xp = cellValues deps >>= ((flip R.evalPy) expr)
  where deps = parseDependencies expr
        expr = Expression xp
        --}
