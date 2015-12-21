{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Updates where

import GHC.Generics
import Data.List as L

-- want to bake into the type system that key is an injective function??? 
class HasKey a where 
  key :: (Eq b) => a -> b

data Diff a = Diff { afterVals :: [a], beforeVals :: [a] } deriving (Show, Read, Eq, Generic)
data Update a b = Update { newVals :: [a], oldKeys :: [b] }

diffToUpdate :: (Eq b, HasKey a) => Diff a -> Update a b
diffToUpdate (Diff after before) = Update after (map key before)

-- Given the descriptorDiff, add a descriptor to the descriptorDiff. It it's already in beforeVals, remove it from that list and don't add it to 
-- afterVals. This maintains the invariant that the same rangeDescriptor is never in both the added and removed lists. 

-- ::ALEX::  
addValue :: (Eq a) => Diff a -> a -> Diff a
addValue diff d = if (inRemoved d) 
  then diff { beforeVals = L.delete d (beforeVals diff) } 
  else diff { afterVals = d:(afterVals diff) } 
    where inRemoved x = L.elem x (beforeVals diff)

removeValue :: (Eq a) => Diff a -> a -> Diff a
removeValue diff d = if (inAdded d)
  then diff { afterVals = L.delete d (afterVals diff) } 
  else diff { beforeVals = d:(beforeVals diff) } 
    where inAdded x = L.elem x (afterVals diff)

emptyDiff :: Diff a
emptyDiff = Diff [] []