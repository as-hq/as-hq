module AS.DB where

import AS.Types
import Import
import Prelude (read, show)

fromDBCell :: ASCellDB -> ASCell
fromDBCell (ASCellDB locationString expressionString valueString) =
	Cell (read locationString :: ASLocation) (read expressionString :: ASExpression) (read valueString :: ASValue)

toDBCell :: ASCell -> ASCellDB
toDBCell (Cell l e v) =
	ASCellDB (show l) (show e) (show v)

getCell :: ASLocation -> Handler (Maybe ASCell)
getCell loc = do
	maybeCell <- runDB $ getBy $ aSCellDBLocationString . show $ loc
	return $ liftIO $ case maybeCell of
		Nothing -> Nothing
		Just (Entity cellId cell) -> Just . fromDBCell $ cell

getCells :: [ASLocation] -> Handler [ASCell]
getCells locs = do
	cells <- runDB $ [getBy $ aSCellDBLocationString . show $ loc | loc<-locs]
	return $ liftIO [fromDBCell cell | (Entity cellId cell) <- cells]

setCell :: ASCell -> Handler ()
setCell cell = do
	maybeCell <- getCell . cellLocation $ cell
	case maybeCell of
		Nothing -> runDB $ insert (toDBCell cell)
		Just (Entity foundCellDBId foundCellDB) -> runDB $ replace foundCellDBId (toDBCell cell) 

deleteCell :: ASLocation -> Handler ()
deleteCell loc = do
	maybeCell <- runDB $ getCell loc
	case maybeCell of
		Nothing -> Nothing
		Just (Entity foundCellDBId foundCellDB) -> runDB $ delete foundCellDBId 

-- insertRelation :: (Relation a) -> IO ()

-- updateRelation :: (Relation a) -> IO ()

-- insertDependency :: (ASCell, ASCell) -> IO ()

-- deleteDependency :: ASRelation -> IO ()

-- getDependency :: ASCell -> ASCell -> ASRelation

putDAG :: [(ASCell, ASCell)] -> Handler ()
putDAG [] = Nothing
putDAG dag = do 
	mapM_ (\edge -> runDB . insert . (show . cellLocation . fst $ edge, show . cellLocation . snd $ edge)) dag

getDAG :: Handler [(ASLocation, ASLocation)]
getDAG = do
	dag <- runDB $ selectList [] []
	let edges = [foundEdge | (Entity foundEdgeId foundEdge) <- dag]
	return $ map (\edge -> (read . fst $ edge :: ASLocation, read . snd $ edge :: ASLocation)) edges