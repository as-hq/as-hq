{-# LANGUAGE DeriveGeneric #-}

module AS.Types.RowColProps where

import qualified AS.Types.CellProps as CP

import GHC.Generics
import Data.Aeson

import Data.Serialize (Serialize)
import Data.Aeson.Types (Parser)
import Data.Maybe
import qualified Data.Map as M
import Control.Applicative
import Control.Monad (liftM, ap)

data RowColType = ColumnType | RowType deriving (Show, Read, Eq, Generic)

data RowCol = RowCol {rowColType :: RowColType, rowColIndex :: Int, rowColProps :: ASRowColProps} deriving (Show, Read, Eq, Generic)

data RowColPropType = DimensionProp | FromCellProp CP.CellPropType deriving (Show, Read, Eq, Generic, Ord)

data RowColProp = 
    Dimension Int -- width for columns, height for rows
  | FromCell CP.CellProp
  deriving (Show, Read, Eq, Generic)

data ASRowColProps = ASRowColProps { underlyingProps :: M.Map RowColPropType RowColProp } deriving (Show, Read, Generic)

instance Eq ASRowColProps where
  (==) (ASRowColProps m1) (ASRowColProps m2) = (m1 == m2)

setProp :: RowColProp -> ASRowColProps -> ASRowColProps
setProp cp (ASRowColProps m) = ASRowColProps $ M.insert (propType cp) cp m

propType :: RowColProp -> RowColPropType
propType (Dimension _) = DimensionProp
propType (FromCell cp) = FromCellProp $ CP.propType cp

emptyProps :: ASRowColProps
emptyProps = ASRowColProps M.empty

-- getProp :: RowColPropType -> ASRowColProps -> Maybe RowColProp
-- getProp pt (ASRowColProps m) = M.lookup pt m

-- hasProp :: RowColPropType -> ASRowColProps -> Bool
-- hasProp pt p = isJust $ getProp pt p

-- removeProp :: RowColPropType -> ASRowColProps -> ASRowColProps
-- removeProp pt (ASRowColProps m) = ASRowColProps $ M.delete pt m



-- Just to get this to compile. We're never actually gettting passed these from frontend. 
instance FromJSON ASRowColProps where
  parseJSON v = return emptyProps
instance ToJSON ASRowColProps where
  toJSON (ASRowColProps m) = toJSON $ M.elems m

instance FromJSON RowColProp
instance ToJSON RowColProp

instance FromJSON RowColType
instance ToJSON RowColType

instance FromJSON RowCol
instance ToJSON RowCol