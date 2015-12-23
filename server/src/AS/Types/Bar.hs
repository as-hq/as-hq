-- Bar means row or column. 

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Bar where

import AS.Types.Sheets
import AS.Types.BarProps
import AS.Types.Updates

import GHC.Generics
import Data.Aeson

import Data.Aeson.Types (Parser)
import Data.Serialize (Serialize)
import Data.Maybe
import Control.Applicative
import Control.Monad (liftM, ap)

data BarType = ColumnType | RowType deriving (Show, Read, Eq, Generic)

-- NORM: never expand this type; always modify it using the records. (So we don't confuse 
-- before and after accidentally.)
type BarDiff = Diff Bar
type BarUpdate = Update Bar BarIndex

-- Uniquely identifies a row or column in a sheet. 
data BarIndex = BarIndex { barSheetId :: ASSheetId, barType :: BarType, barNumber :: Int } deriving (Show, Read, Eq, Generic)

data Bar = Bar {barIndex :: BarIndex, barProps :: ASBarProps} deriving (Show, Read, Eq, Generic)

instance HasKey Bar where
  type KeyType Bar = BarIndex
  key = barIndex

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

instance FromJSON BarType
instance ToJSON BarType

instance FromJSON BarDiff
instance ToJSON BarDiff

instance FromJSON BarUpdate
instance ToJSON BarUpdate

instance FromJSON BarIndex
instance ToJSON BarIndex

instance FromJSON Bar
instance ToJSON Bar

instance Serialize BarType
instance Serialize BarUpdate
instance Serialize BarDiff
instance Serialize BarIndex
instance Serialize Bar