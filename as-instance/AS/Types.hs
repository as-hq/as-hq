{-# LANGUAGE DeriveGeneric #-}

module AS.Types where

import Import
import Database.Persist.TH
import Data.Aeson

-- NOTE: follow excel (col, row) ordering
data ASLocation = Index {sheet :: String, index :: (Int, Int)} | 
                  Range {sheet :: String, range :: ((Int, Int), (Int, Int))}
  deriving (Show, Read, Eq, Ord, Generic)

data ASValue =
  ValueNaN () |
  ValueS String |
  ValueD Double |
  ValueB Bool |
  ValueL [ASValue] |
  ExcelSheet { locs :: ASValue, exprs :: ASValue, vals :: ASValue} |
  Rickshaw {rickshawData :: ASValue} |
  ValueError { error :: String, err_type :: String, file :: String, position :: Int } | 
  ValueImage { imagePath :: String } |
  StockChart { stockPrices :: ASValue, stockName :: String } |
  ObjectValue { objectType :: String, jsonRepresentation :: String } |
  StyledValue { style :: String, value :: ASValue } |
  DisplayValue { displayValue :: String, actualValue :: ASValue }
  deriving (Show, Read, Eq, Generic)
  -- excel: locs=[[Int]], exprs = [String], loc = [ASValue]


lst :: ASValue -> [ASValue]
lst (ValueL l) = l
lst v = []

str :: ASValue -> String
str (ValueS s) = s
str v = []

dbl :: ASValue -> Double
dbl (ValueD d) = d
dbl v = 0


data ASLanguage = R | Python | OCaml | SQL deriving (Show, Read, Eq, Generic)

data ASExpression =
  Expression { expression :: String, language :: ASLanguage } | 
  Reference { location :: ASLocation, referenceIndex :: (Int, Int) }
  deriving (Show, Read, Eq, Generic)


data ASCell = Cell {cellLocation :: ASLocation, 
					cellExpression :: ASExpression,
					cellValue :: ASValue} deriving (Show, Read, Eq, Generic)

instance ToJSON ASLocation
instance FromJSON ASLocation
instance ToJSON ASValue
instance FromJSON ASValue
instance ToJSON ASLanguage
instance FromJSON ASLanguage
instance ToJSON ASExpression
instance FromJSON ASExpression
instance ToJSON ASCell
instance FromJSON ASCell


