{-# LANGUAGE TypeFamilies, StandaloneDeriving #-}

module AS.Types.Updates where

import AS.Prelude

import GHC.Generics
import Control.Applicative (liftA2)
import Data.Aeson.Types
import Data.List as L
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.SafeCopy
import Control.Lens
import Control.Lens.TH

class HasKey a where 
  type KeyType a :: * -- #expert how to ensure this is an Eq
  key :: a -> KeyType a 

data Diff a = Diff { afterVals :: [a], beforeVals :: [a] } deriving (Show, Read, Eq, Generic)

flipDiff :: Diff a -> Diff a
flipDiff (Diff x y) = Diff y x

-- #needsrefactor make this a GADT to restrict b to be of type Keytype a
-- | To apply an update, we delete the values at oldKeys, and then add the newValsSet in. 
-- For example if oldKeys is (1,1), (1,2), and (1,3), and I have a new cell CELL (that's
-- a variable name) at (1,1), applying this update means deleting what's at (1,1), (1,2), and (1,3) 
-- and adding CELL to (1,1). This is used both on the backend to update the DB and on the frontend
-- to update the spreadsheet presented to the user. 

data Update a b = Update { _newValsSet :: Set a, _oldKeysSet :: Set b } deriving (Show, Read, Eq, Data, Typeable, Generic)
makeLenses ''Update


-- 
-- newVals :: Functor f => ([a] -> f [a]) -> (Update a b) -> f (Update a b)

newVals :: (Ord a, Ord b) => Lens' (Update a b) [a]
newVals f (Update n o) = fmap (\x -> Update (S.fromList x) o) (f (S.toList n))

oldKeys :: (Ord a, Ord b) => Lens' (Update a b) [b]
oldKeys f (Update n o) = fmap (\x -> Update n (S.fromList x)) (f (S.toList o))

updateFromLists :: (Ord a, Ord b) => [a] -> [b] -> Update a b
updateFromLists x y = Update (S.fromList x) (S.fromList y)

-- You could write a constructor yourself (not sure if compiles) #anand
--mkUpdate :: (HasKey a, Eq (KeyType b)) => [a] -> [KeyType b] -> Update a (KeyType b)
--mkUpdate as bs = Update {newValsSet = as, oldKeysSet = bs}1

diffToUpdate :: (HasKey a, Eq (KeyType a), Ord a, Ord (KeyType a)) => Diff a -> Update a (KeyType a)
diffToUpdate (Diff after before) = Update after' ((S.map key before') S.\\ (S.map key after'))
  where 
    after' = S.fromList after
    before' = S.fromList before

filterUpdateByKey :: (HasKey a, Eq (KeyType a), Ord a, Ord (KeyType a)) => ((KeyType a) -> Bool) -> Update a (KeyType a) -> Update a (KeyType a)
filterUpdateByKey f update =
  update & newValsSet %~ S.filter (f . key) & oldKeysSet %~ S.filter f

-- Assumes beforeVals is a subset of the thing you're taking a diff of. 
addValue :: (HasKey a, Eq (KeyType a), Ord a, Ord (KeyType a)) => a -> Update a (KeyType a) -> Update a (KeyType a)
addValue d update = if (inRemoved $ key d) 
  then update & oldKeysSet %~ S.delete (key d)
  -- TODO: tim. What the hell is d????
  else update & newValsSet .~ (S.insert d $ update^.newValsSet)
    where inRemoved x = x `S.member` (update^.oldKeysSet)

removeKey :: (HasKey a, Eq (KeyType a), Ord a, Ord (KeyType a)) => (KeyType a) -> Update a (KeyType a) -> Update a (KeyType a)
removeKey d update = if (inAdded d)
    then update & newValsSet %~ S.filter ((/=) d . key)
  else update & oldKeysSet .~ (S.insert d $ update^.oldKeysSet)
    where inAdded x = x `S.member` (S.map key $ update^.newValsSet)

applyUpdate :: (HasKey a, Eq (KeyType a), Ord a, Ord (KeyType a)) => Update a (KeyType a) -> [a] -> [a]
applyUpdate (Update nvs oks) as = S.toList as'''
  where 
    as'   = S.fromList as
    as''  = S.filter (not . (`S.member` oks) . key) as' -- remove the elements with keys in oks
    as''' = as'' `S.union` nvs                          -- add in the new values

emptyDiff :: Diff a
emptyDiff = Diff [] []

-- ::ALEX::
newValuesUpdate :: (HasKey a, Eq (KeyType a), Ord a, Ord (KeyType a)) => [a] -> Update a (KeyType a)
newValuesUpdate c = Update (S.fromList c) (S.empty)

emptyUpdate :: (HasKey a, Eq (KeyType a), Ord a, Ord (KeyType a)) => Update a (KeyType a)
emptyUpdate = newValuesUpdate []

updateToDiff :: (HasKey a, Eq (KeyType a), Ord a, Ord (KeyType a)) => Update a (KeyType a) -> ([KeyType a] -> IO [a]) -> IO (Diff a)
updateToDiff (Update nvs oks) dbGetter = do 
  let (nvs', oks') = (S.toList nvs, S.toList oks)
  oldValues <- dbGetter oks'
  overWrittenValues <- dbGetter $ map key nvs'
  return $ Diff { afterVals = nvs', beforeVals = L.unionBy (\x y -> key x == key y) overWrittenValues oldValues }

--------------------------------------------------------------------------------------------------------------
-- Instances

deriveSafeCopy 1 'base ''Diff

lensNameConverter :: String -> String
lensNameConverter = drop 1 . reverse . drop 3 . reverse

-- _newValsSet --> newVals
instance (ToJSON a, ToJSON b) => ToJSON (Update a b) where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = lensNameConverter}
instance (Ord a, Ord b, FromJSON a, FromJSON b) => FromJSON (Update a b) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = lensNameConverter}