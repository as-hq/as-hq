-- Used code snippets: including copyright just in case
-- Copyright   : (c) 2010,2012 Simon Meier
-- License     : GPL v3 (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
module AS.DAG ( 
    Relation
  , insertDAG
  , deleteDAG
  , updateDAG
  , ancestors
  , descendants 
  , getSetDescendants
  , getSetAncestors
  , dbGetSetDescendants
  , dbGetSetAncestors
  , inverse
  , image
  , reachableSet
  , cyclic
  , toposort

) where

import Import hiding (intersect)
import Control.Arrow
import Control.Applicative
import Control.Monad hiding (foldM)
import Control.Monad.Writer hiding (foldM)
import Control.Monad.RWS hiding (foldM)

import Data.List (intersect)
import qualified Data.Set as S  
import AS.Types
import AS.DB as DB

-- | A relation represented as a list of tuples.
type Relation a = [(a,a)]


-- {--
-- --db interaction functions
-- dbPutDAG :: Relation ASCell -> Handler ()
-- dbPutDAG = mapM_ (runDB . insert . (\edge -> (show . cellLocation . fst $ edge, show . cellLocation . snd $ edge)))
-- dbGetDAG :: Handler (Relation ASCell)
-- dbGetDAG = do
--   dag <- runDB $ selectList [] []
--   let edges = [ foundEdge | (Entity foundEdgeId foundEdge) <- dag ]
--   return $ map (\edge -> (read . fst $ edge :: ASLocation, read . snd $ edge :: ASLocation)) edges
-- dbInteract :: (Eq a) => (Relation a -> b -> Relation a) -> b -> Handler (Relation a)
-- dbInteract f x = do
--    currentDAG <- dbGetDAG
--    let finalDAG = f currentDAG x
--    dbPutDAG finalDAG
--    return finalDAG
-- dbInsertDAG :: (Eq a) => (a, [a]) -> Handler (Relation a)
-- dbInsertDAG = dbInteract insertDAG
-- dbDeleteDAG :: (Eq a) => a -> Handler (Relation a)
-- dbDeleteDAG = dbInteract deleteDAG
-- dbUpdateDAG :: (Eq a) => (a, [a]) -> Handler (Relation a)
-- dbUpdateDAG = dbInteract updateDAG
-- --}

--General notes: we use Data.Set in this implemention; needs ASLocation to derive (Ord) 
--Only represents edges (dependencies); does not hold the information that B1=2, for example
--Use the function decomposeLocs in ASTypes to decompose range dependencies to cell dependencies
  --e.g. A1=f(B1:B5) should be (A1,B1)...(A1,B5) in dag

-- Given ASLocation and list of ASLocations, modify DAG
-- edge from v to w means that v depends on w
-- doesn't delete anything; only adds new things
insertDAGNoCycles :: (Eq a, Ord a) => Relation a -> (a, [a]) -> Relation a
insertDAGNoCycles g (node, []) = g 
insertDAGNoCycles g (node, (adj0:rest)) 
  |elem (node,adj0) g || node==adj0 = insertDAGNoCycles g (node,rest) --don't add duplicate edges
  |otherwise = (node,adj0):(insertDAGNoCycles g (node,rest))

--same as above, but deals with circular references
insertDAG :: (Eq a, Ord a) => Relation a -> (a,[a]) -> Relation a
insertDAG g (node, lst)
  |cyclic h = error "Circular Reference Detected"
  |otherwise = h
  where h = insertDAGNoCycles g (node,lst)

-- Given ASLocation to delete (one direction), modify DAG
-- deleteDAG v will delete all edges of the form (v,w)
-- used as helper function for updateDAG
deleteDAG :: (Eq a) => Relation a -> a -> Relation a
deleteDAG g node = filter (\(a,b) -> node /= a) g

-- Given ASLocation and list of ASLocations, delete current ASLocation and add new edges
--upon adding x to the dag, we delete a    Possible fix:
-- ll edges of the form (x,y); x depends on new things now
--example originally A1=B1+C1, so we have (A,B) and (A,C)
-- if update to A1=C1+D1, we need to delete these and add (A,C) and (A,D)
-- if edge (y,x) exists, we keep it; in above example, if E1=A1 and A1 is changed, keep dependency
-- intermediate function; doesn't detect circular references
-- if you pass it (1,[2,2]) ; duplicate values, it will create the same edge twice. 
updateDAGNoCycles :: (Eq a, Ord a) => Relation a -> (a, [a]) -> Relation a
updateDAGNoCycles g (node, []) = deleteDAG g node
updateDAGNoCycles g (node, (adj0:rest)) 
  |node==adj0 = updateDAGNoCycles h (node,rest) --don't add (1,1)-type things
  |otherwise = (node,adj0):(updateDAGNoCycles h (node,rest))
  where h = deleteDAG g node

--same as above, but detects cycles on an update 
updateDAG :: (Eq a, Ord a) => Relation a -> (a,[a]) -> Relation a
updateDAG g (node, lst)
  |cyclic h = error "Circular Reference Detected"
  |otherwise = h 
  where h = updateDAGNoCycles g (node,lst)

-- Given ASLocation loc, returns list of ASLocations that loc depends on
-- includes loc as first cell in list
ancestors :: (Eq a, Ord a) => a -> Relation a -> [a]
--select things in ts (in order) that are in reach(node)
ancestors node graph = intersect ts reachList
  where
    ts = toposort graph 
    reachList = S.toList $ reachableSet [node] graph 

-- Given ASLocation loc, returns list of ASLocations that depend on loc
-- useful for determining what order to update if a given cell is updated
-- returns given loc as first item on list
descendants :: (Eq a, Ord a) => a -> Relation a -> [a]
descendants node graph = ancestors node (map (\(a,b)->(b,a)) graph) --reverse graph 

--given list of nodes, gives descendents (things that depend on that list) 
--things that depend on first node, followed by things that depend on 2nd node, etc. 
getSetDescendants :: (Eq a, Ord a) => [a] -> Relation a -> [[a]]
getSetDescendants locs graph = map (\x -> descendants x graph) locs 

dbGetSetDescendants :: [ASLocation] -> Handler [[ASLocation]]
dbGetSetDescendants locs = DB.dbGetDAG >>= (return . (getSetDescendants locs))

--given list of nodes, gives ancestors (things that that list depends on)
--things that first node depends on, followed by things second node depends on, etc. 
getSetAncestors :: (Eq a, Ord a) => [a] -> Relation a -> [[a]]
getSetAncestors locs graph = map (\x -> ancestors x graph) locs 

dbGetSetAncestors :: [ASLocation] -> Handler [[ASLocation]]
dbGetSetAncestors locs = DB.dbGetDAG >>= (return . (getSetAncestors locs))

-- | Produce a topological sorting of the given relation. If the relation is
-- cyclic, then the result is at least some permutation of all elements of
-- the given relation.
toposort :: Ord a => [(a, a)] -> [a]
toposort dag = 
    execWriter . foldM visit S.empty $ map fst dag ++ map snd dag
  where 
    visit visited x 
      | x `S.member` visited = return visited
      | otherwise            =
          foldM visit (S.insert x visited) preds <* tell (pure x)
      where
        preds = x `image` inverse dag


-- | Compute the set of nodes reachable from the given set of nodes.
reachableSet :: (Ord a) => [a] -> [(a,a)] -> S.Set a
reachableSet start dag = 
    foldl' visit S.empty start
  where 
    visit visited x 
      | x `S.member` visited = visited
      | otherwise            =
          foldl' visit (S.insert x visited) (x `image` dag)

-- | Is the relation cyclic.
cyclic :: Ord a => [(a,a)] -> Bool
cyclic rel = 
    maybe True (const False) $ foldM visitForest S.empty $ map fst rel
  where 
    visitForest visited x
      | x `S.member` visited = return visited
      | otherwise            = findLoop S.empty visited x

    findLoop parents visited x 
      | x `S.member` parents = mzero
      | x `S.member` visited = return visited
      | otherwise            = 
          S.insert x <$> foldM (findLoop parents') visited next
      where
        next     = [ e' | (e,e') <- rel, e == x ]
        parents' = S.insert x parents

-- | The image of an element under a 'Relation'.
image :: Eq a => a -> Relation a -> [a]
image x rel = [ y' | (x', y') <- rel, x == x' ]

-- | The inverse of a 'Relation'.
inverse :: Relation a -> Relation a
inverse rel = [ (y,x) | (x, y) <- rel ]
-- Given ASLocation loc, returns list of ASLocations that loc depends on
-- includes loc as first cell in list
-- ancestors :: (Eq a, Ord a) => a -> Relation a -> [a]
-- --select things in ts (in order) that are in reach(node)
-- ancestors node graph = intersect ts r
--   $(logInfo) $ "ancestors computed: " ++ (fromString $ show ancestors)eachList
--   where
--     ts = toposort graph 
--     reachList = S.toList $ reachableSet [node] graph 
