{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module AS.Types.BarProps where

import AS.ASJSON

import GHC.Generics
import Data.Aeson
import Data.Serialize (Serialize)
import qualified Data.Map as M

import qualified AS.Types.CellProps as CP

data BarProp = 
    Dimension Int -- width for columns, height for rows
  | FromCell CP.CellProp
  deriving (Show, Read, Eq, Generic)

data BarPropType = DimensionProp | FromCellProp CP.CellPropType deriving (Show, Read, Eq, Generic, Ord)

data ASBarProps = ASBarProps { underlyingProps :: M.Map BarPropType BarProp } deriving (Show, Read, Generic)

instance Eq ASBarProps where
  (==) (ASBarProps m1) (ASBarProps m2) = (m1 == m2)

setProp :: BarProp -> ASBarProps -> ASBarProps
setProp cp (ASBarProps m) = ASBarProps $ M.insert (propType cp) cp m

propType :: BarProp -> BarPropType
propType (Dimension _) = DimensionProp
propType (FromCell cp) = FromCellProp $ CP.propType cp

emptyProps :: ASBarProps
emptyProps = ASBarProps M.empty

-- Will need these eventually for eval. (e.g., bolding entire columns, and checking if a cell
-- is in a column with defualt properties.) Note that BarProps, unlike CellProps, don't care about
-- conditional formatting, since those are stored on a cell-by-cell basis. 
-- 
-- Could put BarProps and CellProps in a typeclass, but really doesn't seem worth it right now. 

-- getProp :: BarPropType -> ASBarProps -> Maybe BarProp
-- getProp pt (ASBarProps m) = M.lookup pt m

-- hasPropType :: BarPropType -> ASBarProps -> Bool
-- hasPropType pt p = isJust $ getProp pt p

-- removeProp :: BarPropType -> ASBarProps -> ASBarProps
-- removeProp pt (ASBarProps m) = ASBarProps $ M.delete pt m

-- Frontend should actually never pass BarProps to backend. 
asToFromJSON ''BarProp 
asToFromJSON ''BarPropType

instance ToJSON ASBarProps where
  toJSON (ASBarProps m) = toJSON $ M.elems m
instance Serialize ASBarProps