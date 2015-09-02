module AS.DAG where

import Control.Arrow
import Control.Applicative
import Control.Monad 
import Control.Monad.Writer hiding (foldM)
import Control.Monad.RWS hiding (foldM)

import Data.List (intersect, foldl')
import qualified Data.Set as S  
import AS.Types hiding (error)
import AS.DB as DB

-- | A relation represented as a list of tuples.
type Relation a = [(a,a)]

-- | The image of an element under a 'Relation'.
image :: Eq a => a -> Relation a -> [a]
image x rel = [ y' | (x', y') <- rel, x == x' ]

-- | Compute the set of nodes reachable from the given set of nodes.
reachableSet :: (Ord a) => [a] -> [(a,a)] -> S.Set a
reachableSet start dag = 
    foldl' visit S.empty start
  where 
    visit visited x 
      | x `S.member` visited = visited
      | otherwise            =
          foldl' visit (S.insert x visited) (x `image` dag)

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

-- | The inverse of a 'Relation'.
inverse :: Relation a -> Relation a
inverse rel = [ (y,x) | (x, y) <- rel ]

-- | Gets the descendant subgraph induced by locs (things to update next if locs change)
descendants :: (Eq a, Ord a) => [a] -> Relation a -> [a]
descendants locs graph = let result = intersect (toposort graph) (S.toList (reachableSet locs graph)) in 
  case result of
    [] -> locs
    (x:xs) -> result
      
-- | Gets the immediate ancestors of a set of locs (things needed to evaluate the locs)
immediateAncestors :: (Eq a, Ord a) => [a] -> Relation a -> [a]
immediateAncestors locs graph = concat $ map adjacency locs
  where
    adjacency v = [x | (x,y) <- graph, y == v]

-- | Handler/DB versions

getDescendants :: [ASLocation] -> IO [ASLocation]
getDescendants locs = DB.getDAG >>= (return . (descendants locs))

getImmediateAncestors :: [ASLocation] -> IO [ASLocation]
getImmediateAncestors locs = DB.getDAG >>= (return . (immediateAncestors locs)) 


