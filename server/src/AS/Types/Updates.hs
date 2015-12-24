{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module AS.Types.Updates where

import GHC.Generics
import Data.List as L

class HasKey a where 
  type KeyType a :: * -- #expert how to ensure this is an Eq
  key :: a -> KeyType a

data Diff a = Diff { afterVals :: [a], beforeVals :: [a] } deriving (Show, Read, Eq, Generic)

flipDiff :: Diff a -> Diff a
flipDiff (Diff x y) = Diff y x

-- #expert would like to somehow ensure that a is an instance of HasKey and b is KeyType a. 
data Update a b = Update { newVals :: [a], oldKeys :: [b] } deriving (Show, Read, Eq, Generic)

diffToUpdate :: (HasKey a, Eq (KeyType a)) => Diff a -> Update a (KeyType a)
diffToUpdate (Diff after before) = Update after ((map key before) \\ (map key after))

-- Assumes beforeVals is a subset of the thing you're taking a diff of. 
addValue :: (Eq a) => Diff a -> a -> Diff a
addValue diff d = if (inRemoved d) 
  then diff { beforeVals = L.delete d (beforeVals diff) }
  else diff { afterVals = d:(afterVals diff) } 
    where inRemoved x = x `L.elem` (beforeVals diff)

removeValue :: (Eq a) => Diff a -> a -> Diff a
removeValue diff d = if (inAdded d)
  then diff { afterVals = L.delete d (afterVals diff) } 
  else diff { beforeVals = d:(beforeVals diff) } 
    where inAdded x = x `L.elem` (afterVals diff)

emptyDiff :: Diff a
emptyDiff = Diff [] []

emptyUpdate :: Update a b
emptyUpdate = Update [] []