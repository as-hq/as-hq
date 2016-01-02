{-# LANGUAGE TypeFamilies, DeriveGeneric, TemplateHaskell #-}

module AS.Types.RangeDescriptor where 

import AS.ASJSON

import AS.Types.Locations
import AS.Types.Updates
import AS.Types.Values

import GHC.Generics

import qualified Data.Map as M

-- turning a spreadsheet range into dataframe etc...
-- only needed during at syntax and list decoupling
data RangeDescriptor = RangeDescriptor { descriptorKey :: RangeKey, expandingType :: ExpandingType, attrs :: JSON }
  deriving (Show, Read, Eq, Generic)


data ExpandingType = List | RList | RDataFrame | NPArray | NPMatrix | PDataFrame | PSeries deriving (Show, Read, Eq, Generic)

-- range keys are used to access range descriptors, which relay metadata about a range of cells
-- e.g. for embedded lists and objects
data RangeKey = RangeKey { keyIndex :: ASIndex
                         , keyDimensions :: Dimensions } 
                         deriving (Show, Read, Eq, Generic)

-- NORM: never expand this type. always modify and access it using beforeVals and afterVals.
type DescriptorDiff = Diff RangeDescriptor
type DescriptorUpdate = Update RangeDescriptor RangeKey

instance HasKey RangeDescriptor where 
  type KeyType RangeDescriptor = RangeKey
  key = descriptorKey

-- The JSON type stores metadata about the object. (For example, in time series,JS what the indices are)
-- #needsrefactor JSON should in principle be a typeclass; JSONValue's shouldn't only be allowed to be
-- ListValues or SimpleValues. 
type JSON = M.Map JSONKey JSONField 
type JSONKey = String 
data JSONField = JSONTree JSON | JSONLeaf JSONValue deriving (Show, Read, Eq, Generic)
data JSONValue = ListValue Collection | SimpleValue ASValue deriving (Show, Read, Eq, Generic)

asToJSON ''RangeDescriptor
asToJSON ''ExpandingType
asToJSON ''RangeKey
asToJSON ''DescriptorDiff
asToJSON ''DescriptorUpdate
asToJSON ''JSONField
asToJSON ''JSONValue