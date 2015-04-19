module AS.DB where

import AS.Types
import Import
import Prelude (read, show, (!!))

fromDBCell :: ASCellDB -> ASCell
fromDBCell (ASCellDB locationString expressionString valueString) =
	Cell (read locationString :: ASLocation) (read expressionString :: ASExpression) (read valueString :: ASValue)

toDBCell :: ASCell -> ASCellDB
toDBCell (Cell l e v) =
	ASCellDB (show l) (show e) (show v)

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
	cells <- runDB $ selectList [ASCellDBLocationString ==. show (cellLocation cell)] []
	case cells of
		[] -> (runDB $ insert (toDBCell cell)) >> return ()
		((Entity cellDBId cellDB):cs) -> (runDB $ replace cellDBId (toDBCell cell)) >> return () 

-- TODO FIX
setCells :: [ASCell] -> Handler ()
setCells cells = setCell $ cells !! 0
	 -- retrievedCells <- runDB $ mapM_ (\cell -> selectList [ASCellDBLocationString ==. show (cellLocation cell)] []) cells
	 -- runDB $ mapM_ (\cellTuple -> case (fst cellTuple) of 
	 -- 	[] -> (insert (toDBCell (snd cellTuple))) >> return ()
	 -- 	((Entity cellId cell):cs) -> (replace cellId (toDBCell (snd cellTuple))) >> return ()) (zip retrievedCells cells)


deleteCell :: ASLocation -> Handler ()
deleteCell loc = do
	cells <- runDB $ selectList [ASCellDBLocationString ==. show loc] []
	case cells of
		[] -> return ()
		((Entity cellDBId cellDB):cs) -> (runDB $ delete cellDBId) >> return ()

-- insertRelation :: (Relation a) -> IO ()

-- updateRelation :: (Relation a) -> IO ()

-- insertDependency :: (ASCell, ASCell) -> IO ()

-- deleteDependency :: ASRelation -> IO ()

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