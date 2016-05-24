{-# LANGUAGE TemplateHaskell #-}

module AS.Types.Mutate where

import AS.Prelude
import AS.Types.Locations

import AS.ASJSON
import Data.Aeson

import Data.SafeCopy


-- | MutateTypeNew and PairMutate are type constructors with parameters,
-- since this will make certain functions in Mutate (such as coreMutate)
-- much easier to write.
-- | The only types ever passed into this type constructor are Row and Col.
data MutateTypeNew a = Insert a | Delete a | Drag a a
  deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | PairMutate is only ever applied on Row and Col, but has parameters to make
-- functions like coordMutate (in Handlers/Mutate.hs) easier to write.
-- | See coordMutate to see what I mean in my unclear comment above.
-- | PairMutate is ONLY used by helper functions, and in anything exposed to
-- the user, Mutate should be used.
data Mutate = 
  ColMutate (MutateTypeNew Col) | RowMutate (MutateTypeNew Row)
  deriving (Show, Read, Eq, Data, Typeable, Generic)

type Mc = MutateTypeNew Col
type Mr = MutateTypeNew Row
deriveSafeCopy 1 'base ''MutateTypeNew
deriveSafeCopy 1 'base ''Mutate

-- #NeedsRefactor: timchu. Blatant hack to get ToJSON and fromJSON to work on Mutates
-- properly. This will be changed at the time I land this.

-- types should change, so this will not be necessary in the long run.
--instance ToJSON MutateTypeNew where
--  toJSON (ColMutate $ Insert a) = 
--    object [ "tag" .= ("InsertCol" :: String),
--             "insertColNum" .= a]
--  toJSON (RowMutate $ Insert a) = 
--    object [ "tag" .= ("InsertRow" :: String),
--             "insertRowNum" .= a]
--  toJSON (ColMutate $ Delete a) = 
--    object [ "tag" .= ("DeleteCol" :: String),
--             "deleteColNum" .= a]
--  toJSON (RowMutate $ Delete a) = 
--    object [ "tag" .= ("DeleteRow" :: String),
--             "deleteRowNum" .= a]
--  toJSON (ColMutate $ Drag a b) = 
--    object [ "tag" .= ("DragCol" :: String),
--             "oldColNum" .= a,
--             "newColNum" .= b]
--  toJSON (RowMutate $ Drag a b) = 
--    object [ "tag" .= ("DragRow" :: String),
--             "oldRowNum" .= a,
--             "newRowNum" .= b]

toMT :: Mutate -> MutateType
toMT (ColMutate (Insert (Col a)))       = InsertCol a
toMT (ColMutate (Delete (Col a)))       = DeleteCol a
toMT (ColMutate (Drag (Col a) (Col b))) = DragCol a b
toMT (RowMutate (Insert (Row a)))       = InsertRow a
toMT (RowMutate (Delete (Row a)))       = DeleteRow a
toMT (RowMutate (Drag (Row a) (Row b))) = DragRow a b

fromMT :: MutateType -> Mutate
fromMT (InsertCol a) = (ColMutate (Insert (Col a)))        
fromMT (DeleteCol a) = (ColMutate (Delete (Col a)))        
fromMT (DragCol a b) = (ColMutate (Drag (Col a) (Col b)))  
fromMT (InsertRow a) = (RowMutate (Insert (Row a)))        
fromMT (DeleteRow a) = (RowMutate (Delete (Row a)))        
fromMT (DragRow a b) = (RowMutate (Drag (Row a) (Row b)))  
-- Dud thing so that we can automatically get the right To and From JSON.
data MutateType = InsertCol { insertColNum :: Int } | InsertRow { insertRowNum :: Int } |
                  DeleteCol { deleteColNum :: Int } | DeleteRow { deleteRowNum :: Int } |
                  DragCol { oldColNum :: Int, newColNum :: Int } | DragRow { oldRowNum :: Int, newRowNum :: Int }
                  deriving (Show, Read, Eq, Data, Typeable, Generic)

-- Turns Mutates into JSONs that the frontend knows how to deal with.
instance ToJSON Mutate where
  toJSON = toJSON . toMT
instance FromJSON Mutate where
  parseJSON mt = do
    m <- parseJSON mt
    return $ fromMT m
asToFromJSON ''MutateType
