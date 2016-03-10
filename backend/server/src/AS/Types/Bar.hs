-- Bar means row or column. 

{-# LANGUAGE TypeFamilies, DeriveGeneric, TemplateHaskell #-}

module AS.Types.Bar where

import AS.Prelude
import Prelude()

import AS.ASJSON

import AS.Types.Sheets
import AS.Types.BarProps
import AS.Types.Updates

import GHC.Generics
import Data.Aeson.Types (Parser)
import Data.Maybe
import Data.SafeCopy
import Control.Applicative
import Control.Monad (liftM, ap)

data BarType = ColumnType | RowType deriving (Show, Read, Eq, Data, Typeable, Generic, Ord)

-- NORM: never expand this type; always modify it using the records. (So we don't confuse 
-- before and after accidentally.)
type BarDiff = Diff Bar
type BarUpdate = Update Bar BarIndex

-- Uniquely identifies a row or column in a sheet. 
data BarIndex = BarIndex { barSheetId :: ASSheetId, barType :: BarType, barNumber :: Int } deriving (Show, Read, Eq, Data, Typeable, Generic, Ord)

data Bar = Bar {barIndex :: BarIndex, barProps :: ASBarProps} deriving (Show, Read, Eq, Generic)

instance Ord Bar where 
  (<=) b1 b2 = (barIndex b1) <= (barIndex b2)

instance HasKey Bar where
  type KeyType Bar = BarIndex
  key = barIndex

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

deriveSafeCopy 1 'base ''Bar
deriveSafeCopy 1 'base ''BarIndex
deriveSafeCopy 1 'base ''BarType

asToFromJSON ''BarType
asToFromJSON ''BarIndex
asToJSON ''BarDiff
asToJSON ''Bar
