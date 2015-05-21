{-# LANGUAGE DeriveGeneric #-}

module AS.Types where

import Import
import Database.Persist.TH
import Data.Aeson

-- NOTE: follow excel (col, row) ordering
-- why does ASLocation include a
data ASLocation = Index {index :: (Int, Int)} | Range {range :: ((Int, Int), (Int, Int))}
	deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON ASLocation
instance FromJSON ASLocation

data ASValue =
  ValueNaN () |
  ValueS String |
  ValueD Double |
  ValueB Bool |
  ValueL [ASValue] |
  ValueError { position :: (Int,Int), message :: String } | 
  ValueImage { imagePath :: String } |
  StockChart { stockPrices :: ASValue, stockName :: String } |
  ObjectValue { objectType :: String, jsonRepresentation :: String } |
  StyledValue { style :: String, value :: ASValue } |
  DisplayValue { displayValue :: String, actualValue :: ASValue }
	deriving (Show, Read, Eq, Generic)

instance ToJSON ASValue
instance FromJSON ASValue

data ASLanguage = R | Python | OCaml
  deriving (Show, Read, Eq, Generic)

instance ToJSON ASLanguage
instance FromJSON ASLanguage

data ASExpression =
  Expression { expression :: String, language :: ASLanguage } | --TODO: incorporate language into ASE references everywhere
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

