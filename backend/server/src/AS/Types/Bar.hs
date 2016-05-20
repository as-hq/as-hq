-- Bar means row or column. 

{-# LANGUAGE TypeFamilies, DeriveGeneric, TemplateHaskell #-}

module AS.Types.Bar where

import AS.Prelude

import AS.ASJSON
import AS.Types.Sheets
import AS.Types.BarProps
import AS.Types.Updates
import AS.Types.Locations

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe
import Data.SafeCopy
import Control.Applicative

-- NORM: never expand this type; always modify it using the records. (So we don't confuse 
-- before and after accidentally.)
type BarDiff = Diff Bar
type BarUpdate = Update Bar BarIndex

-- Uniquely identifies a row or column in a sheet. 
data BarCoord = BarRow Row | BarCol Col deriving (Show, Read, Eq, Typeable, Data, Generic, Ord)
data BarIndex = BarIndex { barSheetId :: SheetID, barCoord :: BarCoord } deriving (Show, Read, Eq, Typeable, Data, Generic, Ord)

data Bar = Bar {barIndex :: BarIndex, barProps :: ASBarProps} deriving (Show, Read, Eq, Generic, Typeable, Data)

instance Ord Bar where 
  (<=) b1 b2 = (barIndex b1) <= (barIndex b2)

instance HasKey Bar where
  type KeyType Bar = BarIndex
  key = barIndex

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

deriveSafeCopy 1 'base ''Bar
deriveSafeCopy 1 'base ''BarCoord

basicObject :: String -> Value
basicObject s = object["tag" .= (s :: String)]

colType :: Value
colType = basicObject "ColumnType"

rowType :: Value
rowType = basicObject "RowType"

-- these can get replaced by asFromToJSON once frontend changes are implemented.
instance ToJSON BarIndex where
  toJSON (BarIndex sid bCoord) = object [ "barSheetId" .= sid,
                                         "barType"    .= case bCoord of
                                                       BarCol _ -> "ColumnType"
                                                       BarRow _ -> "RowType" :: String,
                                         "barNumber"   .= case bCoord of 
                                                        BarCol (Col i) -> i
                                                        BarRow (Row j) -> j]

barCoordFromTypeNum :: String -> Int -> BarCoord
barCoordFromTypeNum typ i =
  case typ of
     "ColumnType" ->  BarCol (Col i)
     "RowType" -> BarRow  (Row i)

instance FromJSON BarIndex  where
  parseJSON (Object v) = do
    sid <- v .: "barSheetId"
    typStr <- v .: "barType"
    num <- v .: "barNumber"
    return $ BarIndex sid $ barCoordFromTypeNum typStr num
  parseJSON _          = fail "couldn't parse BarIndex"

asToJSON ''BarDiff
asToJSON ''Bar

-- ************ MIGRATIONS *************
-- This is used to handle DB migrations when the types in the DB change.

data BarType0 = ColumnType | RowType deriving (Show, Read, Eq, Data, Typeable, Generic)
data BarIndex0 = BarIndex0 { barSheetId0 :: SheetID, barType0 :: BarType0, barNumber0 :: Int } deriving (Show, Read, Eq, Data, Typeable, Generic)
deriveSafeCopy 1 'base ''BarType0
deriveSafeCopy 1 'base ''BarIndex0
deriveSafeCopy 2 'extension ''BarIndex

instance Migrate BarIndex where
  type MigrateFrom BarIndex = BarIndex0
  migrate (BarIndex0 sid barType num) =
    case barType of
         ColumnType -> BarIndex sid (BarCol $ Col num)
         RowType -> BarIndex sid (BarRow $ Row num)
