{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Bar where

import qualified AS.Types.CellProps as CP

import GHC.Generics
import Data.Aeson

import Data.Serialize (Serialize)
import Data.Aeson.Types (Parser)
import Data.Serialize (Serialize)
import Data.Maybe
import qualified Data.Map as M
import Control.Applicative
import Control.Monad (liftM, ap)

data BarType = ColumnType | RowType deriving (Show, Read, Eq, Generic)

data Bar = Bar {barType :: BarType, barIndex :: Int, barProps :: ASBarProps} deriving (Show, Read, Eq, Generic)

data BarPropType = DimensionProp | FromCellProp CP.CellPropType deriving (Show, Read, Eq, Generic, Ord)

data BarProp = 
    Dimension Int -- width for columns, height for rows
  | FromCell CP.CellProp
  deriving (Show, Read, Eq, Generic)

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
-- is in a column with defualt properties.)

-- getProp :: BarPropType -> ASBarProps -> Maybe BarProp
-- getProp pt (ASBarProps m) = M.lookup pt m

-- hasPropType :: BarPropType -> ASBarProps -> Bool
-- hasPropType pt p = isJust $ getProp pt p

-- removeProp :: BarPropType -> ASBarProps -> ASBarProps
-- removeProp pt (ASBarProps m) = ASBarProps $ M.delete pt m

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

-- Just to get this to compile. We're never actually gettting passed these from frontend. 
instance FromJSON ASBarProps where
  parseJSON v = return emptyProps
instance ToJSON ASBarProps where
  toJSON (ASBarProps m) = toJSON $ M.elems m

instance FromJSON BarProp
instance ToJSON BarProp

instance FromJSON BarType
instance ToJSON BarType

instance FromJSON Bar
instance ToJSON Bar

instance Serialize BarType
instance Serialize BarProp
instance Serialize Bar
instance Serialize ASBarProps
instance Serialize BarPropType
