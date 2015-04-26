{-# LANGUAGE DeriveGeneric #-}

module AS.Types where

import Import
import Database.Persist.TH
import Data.Aeson

-- NOTE: follow excel (col, row) ordering
data ASLocation = Index {index :: (Int, Int)} | Range {range :: ((Int, Int), (Int, Int))}
	deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON ASLocation
instance FromJSON ASLocation

data ASValue =
  ValueNaN () |
  ValueS String |
  ValueD Double |
  ValueL [ASValue] |
  StyledValue { style :: String, value :: ASValue } |
  DisplayValue { displayValue :: String, actualValue :: ASValue }
	deriving (Show, Read, Eq, Generic)

instance ToJSON ASValue
instance FromJSON ASValue

data ASExpression =
  Expression { expression :: String } |
  Reference { location :: ASLocation, referenceIndex :: (Int, Int) }
	deriving (Show, Read, Eq, Generic)

instance ToJSON ASExpression
instance FromJSON ASExpression

data ASCell = Cell {cellLocation :: ASLocation, 
					cellExpression :: ASExpression,
					cellValue :: ASValue}
	deriving (Show, Read, Eq, Generic)

instance ToJSON ASCell
instance FromJSON ASCell

-- convenience funcs

