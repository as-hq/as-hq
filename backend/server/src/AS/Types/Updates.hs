{-# LANGUAGE TypeFamilies, StandaloneDeriving #-}

module AS.Types.Updates where

import AS.Prelude
import Prelude()

import GHC.Generics
import Control.Applicative (liftA2)
import Data.List as L
import Data.SafeCopy

class HasKey a where 
  type KeyType a :: * -- #expert how to ensure this is an Eq
  key :: a -> KeyType a 

data Diff a = Diff { afterVals :: [a], beforeVals :: [a] } deriving (Show, Read, Eq, Generic)

flipDiff :: Diff a -> Diff a
flipDiff (Diff x y) = Diff y x

-- #needsrefactor make this a GADT to restrict b to be of type Keytype a
-- | To apply an update, we delete the values at oldKeys, and then add the newVals in. 
-- For example if oldKeys is (1,1), (1,2), and (1,3), and I have a new cell CELL (that's
-- a variable name) at (1,1), applying this update means deleting what's at (1,1), (1,2), and (1,3) 
-- and adding CELL to (1,1). This is used both on the backend to update the DB and on the frontend
-- to update the spreadsheet presented to the user. 
data Update a b = Update { newVals :: [a], oldKeys :: [b] } deriving (Show, Read, Eq, Data, Typeable, Generic)
-- You could write a constructor yourself (not sure if compiles) #anand
--mkUpdate :: (HasKey a, Eq (KeyType b)) => [a] -> [KeyType b] -> Update a (KeyType b)
--mkUpdate as bs = Update {newVals = as, oldKeys = bs}1

diffToUpdate :: (HasKey a, Eq (KeyType a)) => Diff a -> Update a (KeyType a)
diffToUpdate (Diff after before) = Update after ((map key before) \\ (map key after))

filterUpdateByKey :: (HasKey a, Eq (KeyType a)) => ((KeyType a) -> Bool) -> Update a (KeyType a) -> Update a (KeyType a)
filterUpdateByKey f (Update newVals oldKeys) = Update newVals' oldKeys' 
  where 
    newVals' = filter (f . key) newVals
    oldKeys' = filter f oldKeys

-- Assumes beforeVals is a subset of the thing you're taking a diff of. 
addValue :: (HasKey a, Eq (KeyType a)) => a -> Update a (KeyType a) -> Update a (KeyType a)
addValue d update = if (inRemoved $ key d) 
  then update { oldKeys = L.delete (key d) (oldKeys update) }
  else update { newVals = d:(newVals update) } 
    where inRemoved x = x `L.elem` (oldKeys update)

removeKey :: (HasKey a, Eq (KeyType a)) => (KeyType a) -> Update a (KeyType a) -> Update a (KeyType a)
removeKey d update = if (inAdded d)
    then update { newVals = L.filter ((/=) d . key) (newVals update) } 
  else update { oldKeys = d:(oldKeys update) } 
    where inAdded x = x `L.elem` (map key $ newVals update)

applyUpdate :: (HasKey a, Eq (KeyType a)) => Update a (KeyType a) -> [a] -> [a]
applyUpdate (Update nvs oks) as = L.unionBy (\x y -> key x == key y) nvs (filter (not . (flip elem) oks . key) as) 

emptyDiff :: Diff a
emptyDiff = Diff [] []

emptyUpdate :: Update a b
emptyUpdate = Update [] []

updateToDiff :: (HasKey a, Eq (KeyType a)) => Update a (KeyType a) -> ([KeyType a] -> IO [a]) -> IO (Diff a)
updateToDiff (Update nvs oks) dbGetter = do 
  oldValues <- dbGetter oks
  overWrittenValues <- dbGetter $ map key nvs
  return $ Diff { afterVals = nvs, beforeVals = L.unionBy (\x y -> key x == key y) overWrittenValues oldValues }

--------------------------------------------------------------------------------------------------------------
-- Instances

deriveSafeCopy 1 'base ''Update
deriveSafeCopy 1 'base ''Diff
