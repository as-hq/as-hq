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

getCells :: [ASLocation] -> Handler [ASCell]
getCells locs = do
	cells <- runDB $ selectList [ASCellDBLocationString <-. map show locs] []
	return $ map (\(Entity cellId cell) -> fromDBCell cell) cells

setCell :: ASCell -> Handler ()
setCell cell = setCells [cell]

setCells :: [ASCell] -> Handler ()
setCells cells = do
	let rngCells = filter (\cell -> case (cellLocation cell) of 
								(Range a) -> True
								otherwise -> False) cells
	let locs = (map cellLocation cells) ++ concat (map (decomposeLocs . cellLocation) rngCells)
	runDB $ deleteWhere [ASCellDBLocationString <-. (map show locs)]
	insertCells cells
	setRangeCells rngCells

-- internal use only
setRangeCells :: [ASCell] -> Handler ()
setRangeCells cells = do
	let locs = map (\cell -> zip3 (repeat $ language . cellExpression $ cell) (decomposeLocs $ cellLocation cell) [0..]) cells

	let vals = map (\cell -> case (cellValue cell) of
						(ValueNaN ()) -> repeat $ ValueNaN ()
						(ValueL b) -> b) cells
	let subCells = map(\(locSet, val) ->
		[Cell loc (Expression ((indexToExcel . index $ loc) ++ "[" ++ (show idx) ++ "]") lang) (val !! idx) | (lang, loc, idx)<-locSet]
		) $ zip locs vals

	insertCells $ concat subCells

trd :: (a,b,c) -> c
trd (a,b,c) = c

-- internal use only
insertCells :: [ASCell] -> Handler ()
insertCells cells = mapM_ (\cell -> (runDB $ insert $ toDBCell cell) >> return ()) cells

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

-- ADDED FOR RANGES - should be optimized
dbUpdateLocationDepsBatch :: [(ASLocation,[ASLocation])] -> Handler ()
dbUpdateLocationDepsBatch = mapM_ dbUpdateLocationDependencies

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
		[] 										-> (runDB $ insert func) >> return ()
		((Entity foundFuncId foundFunc):funcs) 	-> (runDB $ (delete foundFuncId) >> (insert func)) >> return ()

deleteFunc :: String -> Handler ()
deleteFunc aliasStr = do
	funcs <- runDB $ selectList [ASFuncAlias ==. pack aliasStr] []
	case funcs of 
		[] -> return ()
		((Entity foundFuncId foundFunc):funcs) -> (runDB $ delete foundFuncId) >> return ()

getFuncsNaive :: Handler [ASFunc]
getFuncsNaive = do
	result <- runDB $ selectList [] []
	return [func | (Entity funcId func) <- result]


getFuncs :: ASLanguage -> Handler [ASFunc]
getFuncs lang = do
	result <- runDB $ selectList [ASFuncLang ==. (pack $ show lang)] []
	return [func | (Entity funcId func) <- result]