{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Bar where

import AS.Types.Sheets
import AS.Types.BarProps

import GHC.Generics
import Data.Aeson

import Data.Aeson.Types (Parser)
import Data.Serialize (Serialize)
import Data.Maybe
import Control.Applicative
import Control.Monad (liftM, ap)

data BarType = ColumnType | RowType deriving (Show, Read, Eq, Generic)

-- Uniquely identifies a row or column in a sheet. 
data BarIndex = BarIndex { barSheetId :: ASSheetId, barType :: BarType, barNumber :: Int } deriving (Show, Read, Eq, Generic)

data Bar = Bar {barIndex :: BarIndex, barProps :: ASBarProps} deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

instance FromJSON BarType
instance ToJSON BarType

instance FromJSON BarIndex
instance ToJSON BarIndex

instance FromJSON Bar
instance ToJSON Bar

instance Serialize BarType
instance Serialize BarIndex
instance Serialize Bar