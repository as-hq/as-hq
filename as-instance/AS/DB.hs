module AS.DB where

import AS.Types
import Foundation (runDB)

getCell :: ASLocation -> IO (Maybe ASCell)
getCell loc = do
	maybeCell <- runDB $ getBy $ ASCellLocation loc
	return $ case maybeCell of
		Nothing -> Nothing
		Just (Entity cellId cell) -> Just cell

getCells :: [ASLocation] -> IO [ASCell]
getCells locs = do
	cells <- runDB $ [getBy $ ASCellLocation loc | loc->locs]
	return [cell | (Entity cellId cell) <- cells]

setCell :: ASCell -> IO ()
setCell cell = do
	maybeCell <- runDB $ getBy $ ASCellLocation $ cellLoc cell
	return $ case maybeCell of
		Nothing -> runDB $ insert cell
		Just (Entity foundCellId foundCell) -> runDB $ replace foundCellId cell 

deleteCell :: ASCell -> IO ()
deleteCell cell = deleteCell $ cellLoc cell

deleteCell :: ASlocation -> IO ()
deleteCell loc = do
	maybeCell <- runDB $ getBy $ ASCellLocation loc
	case maybeCell of
		Nothing -> Nothing
		Just (Entity foundCellId foundCell) -> runDB $ delete foundCellId 

-- insertRelation :: (Relation a) -> IO ()

-- updateRelation :: (Relation a) -> IO ()

-- insertDependency :: (ASCell, ASCell) -> IO ()

-- deleteDependency :: ASRelation -> IO ()

-- getDependency :: ASCell -> ASCell -> ASRelation

putDAG :: IO [(ASCell, ASCell)] -> IO ()
putDAG [] = Nothing
putDAG dag = do
	mapM_ (\edge -> runDB (insert edge)) dag

getDAG :: IO [(ASCell, ASCell)]
getDAG = do
	dag <- runDB $ selectList [] []
	return dag

insertDependency :: (ASCell, ASCell) -> IO ()
insertDependency edge = runDB $ insert edge

deleteDependency :: (ASCell, ASCell) -> IO ()
deleteDependency edge = do
	maybeEdge <- runDB $ getBy $ ASDAGEdge edge
	case maybeEdge of 
		Nothing -> Nothing
		Just (Entity edgeId edge) -> runDB $ delete edgeId

-- getDependents :: ASCell -> IO [ASCell]
-- getDependents cell = do
-- 	foundCell <- getCell cell
-- 	runDB $ selectList 
