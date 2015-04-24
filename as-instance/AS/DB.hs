module AS.DB where

import AS.Types
import AS.Parsing
import Import hiding (index)
import Prelude (read, show, (!!))

fromDBCell :: ASCellDB -> ASCell
fromDBCell (ASCellDB locationString expressionString valueString) =
	Cell (read locationString :: ASLocation) (read expressionString :: ASExpression) (read valueString :: ASValue)

toDBCell :: ASCell -> ASCellDB
toDBCell (Cell l e v) =
	ASCellDB (show l) (show e) (show v)

fromDBRelation :: RelationDB -> (ASLocation, ASLocation)
fromDBRelation (RelationDB firstEndpoint secondEndpoint) = (read firstEndpoint :: ASLocation, read secondEndpoint :: ASLocation)

toDBRelation :: (ASLocation, ASLocation) -> RelationDB
toDBRelation (x, y) = RelationDB (show x) (show y)

getCell :: ASLocation -> Handler (Maybe ASCell)
getCell loc = do
	cells <- runDB $ selectList [ASCellDBLocationString ==. show loc] []
	return $ case cells of
		[] -> Nothing
		((Entity cellId cell):cs) -> Just . fromDBCell $ cell

getCells :: [ASLocation] -> Handler [Maybe ASCell]
getCells locs = do
	cells <- runDB $ mapM (\loc -> selectList [ASCellDBLocationString ==. show loc] []) locs
	return $ map (\cellList -> case cellList of 
		[] -> Nothing
		((Entity cellId cell):cs) -> Just . fromDBCell $ cell) cells

setCell :: ASCell -> Handler ()
setCell cell = do
	let loc = cellLocation cell
	cells <- runDB $ selectList [ASCellDBLocationString ==. show loc] []
	case cells of 
		[] -> (runDB $ insert (toDBCell cell)) >> return ()
		((Entity cellDBId cellDB):cs) -> (runDB $ replace cellDBId (toDBCell cell)) >> return ()
	case loc of 
		(Range a) -> do
			let locs = zip (decomposeLocs loc) [0..]
			$(logInfo) $ (fromString $ show $ cellValue cell)
			let vals = case (cellValue cell) of
				(ValueNaN ()) -> repeat $ ValueNaN ()
				(ValueL b) -> b
			setCells [Cell (fst l) (Expression ((indexToExcel . index $ fst l) ++ "[" ++ show (snd l) ++ "]")) (vals !! (snd l)) | l<-locs]
		otherwise -> return ()

-- {--
-- -- TODO FIX
-- setCells :: [ASCell] -> Handler ()
-- setCells cells = setCell $ cells !! 0
-- 	 -- retrievedCells <- runDB $ mapM_ (\cell -> selectList [ASCellDBLocationString ==. show (cellLocation cell)] []) cells
-- 	 -- runDB $ mapM_ (\cellTuple -> case (fst cellTuple) of 
-- 	 -- 	[] -> (insert (toDBCell (snd cellTuple))) >> return ()
-- 	 -- 	((Entity cellId cell):cs) -> (replace cellId (toDBCell (snd cellTuple))) >> return ()) (zip retrievedCells cells)
-- --}

setCells :: [ASCell] -> Handler ()
setCells = mapM_ setCell

deleteCell :: ASLocation -> Handler ()
deleteCell loc = do
	cells <- runDB $ selectList [ASCellDBLocationString ==. show loc] []
	case cells of
		[] -> return ()
		((Entity cellDBId cellDB):cs) -> (runDB $ delete cellDBId) >> return ()

dbInsertSingleRelation :: (ASLocation, ASLocation) -> Handler ()
dbInsertSingleRelation rel = (runDB . insert . toDBRelation $ rel) >> return ()

dbInsertRelation :: [(ASLocation, ASLocation)] -> Handler ()
dbInsertRelation = mapM_ dbInsertSingleRelation

dbDeleteLocationDependencies :: ASLocation -> Handler ()
dbDeleteLocationDependencies loc = runDB $ deleteWhere [RelationDBFirstEndpoint ==. (show loc)]

--TODO: also delete dependencies with expressions that are references

dbUpdateLocationDependencies :: (ASLocation, [ASLocation]) -> Handler ()
dbUpdateLocationDependencies (loc, deps) =
  dbDeleteLocationDependencies loc >> dbInsertRelation (zip (repeat loc) deps)

dbInsertDependency :: (ASCell, ASCell) -> Handler ()
dbInsertDependency (x, y) = dbInsertSingleRelation (cellLocation x, cellLocation y)

dbGetDAG :: Handler [(ASLocation, ASLocation)]
dbGetDAG = do
  dag <- runDB $ selectList [] []
  let edges = [ foundEdge | (Entity foundEdgeId foundEdge) <- dag ]
  return $ map fromDBRelation edges

-- deleteDependency :: ASRelation -> Handler ()

-- getDependency :: ASCell -> ASCell -> ASRelation

-- putDAG :: [(ASCell, ASCell)] -> Handler ()
-- putDAG [] = Nothing
-- putDAG dag = do 
-- 	mapM_ (\edge -> runDB . insert . (show . cellLocation . fst $ edge, show . cellLocation . snd $ edge)) dag

-- getDAG :: Handler [(ASLocation, ASLocation)]
-- getDAG = do
-- 	dag <- runDB $ selectList [] []
-- 	let edges = [foundEdge | (Entity foundEdgeId foundEdge) <- dag]
-- 	return $ map (\edge -> (read . fst $ edge :: ASLocation, read . snd $ edge :: ASLocation)) edges

setFunc :: ASFunc -> Handler ()
setFunc func = do
	funcs <- runDB $ selectList [ASFuncAlias ==. aSFuncAlias func] []
	case funcs of 
		[] -> (runDB $ insert func) >> return ()
		((Entity foundFuncId foundFunc):funcs) -> (runDB $ replace foundFuncId func) >> return ()

deleteFunc :: String -> Handler ()
deleteFunc aliasStr = do
	funcs <- runDB $ selectList [ASFuncAlias ==. pack aliasStr] []
	case funcs of 
		[] -> return ()
		((Entity foundFuncId foundFunc):funcs) -> (runDB $ delete foundFuncId) >> return ()

getFuncs :: Handler [ASFunc]
getFuncs = do
	result <- runDB $ selectList [] []
	return [func | (Entity funcId func) <- result]
