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
addValue :: (HasKey a, Eq (KeyType a)) => Update a (KeyType a) -> a -> Update a (KeyType a)
addValue update d = if (inRemoved $ key d) 
  then update { oldKeys = L.delete (key d) (oldKeys update) }
  else update { newVals = d:(newVals update) } 
    where inRemoved x = x `L.elem` (oldKeys update)

removeKey :: (HasKey a, Eq (KeyType a)) => Update a (KeyType a) -> (KeyType a) -> Update a (KeyType a)
removeKey update d = if (inAdded d)
    then update { newVals = L.filter ((/=) d . key) (newVals update) } 
  else update { oldKeys = d:(oldKeys update) } 
    where inAdded x = x `L.elem` (map key $ newVals update)

emptyDiff :: Diff a
emptyDiff = Diff [] []

emptyUpdate :: Update a b
emptyUpdate = Update [] []

updateToDiff :: (HasKey a, Eq (KeyType a)) => Update a (KeyType a) -> ([KeyType a] -> IO [a]) -> IO (Diff a)
updateToDiff (Update nv ok) dbGetter = do 
  oldValues <- dbGetter ok
  overWrittenValues <- dbGetter $ map key nv
  return $ Diff { afterVals = nv, beforeVals = L.unionBy (\x y -> key x == key y) overWrittenValues oldValues }