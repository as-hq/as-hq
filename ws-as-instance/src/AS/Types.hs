{-# LANGUAGE DeriveGeneric #-}

module AS.Types where

import Prelude
import GHC.Generics
import Data.Aeson hiding (Success)

-- NOTE: follow excel (col, row) ordering
data ASLocation = Index {sheet :: String, index :: (Int, Int)} | 
                  Range {sheet :: String, range :: ((Int, Int), (Int, Int))}
  deriving (Show, Read, Eq, Ord, Generic)

data ASValue =
  ValueNaN () |
  ValueS String |
  ValueI Int |
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

data ASLanguage = R | Python | OCaml | CPP | Java | SQL | Excel deriving (Show, Read, Eq, Generic)

data ASExpression =
  Expression { expression :: String, language :: ASLanguage } | 
  Reference { location :: ASLocation, referenceIndex :: (Int, Int) }
  deriving (Show, Read, Eq, Generic)


data ASCell = Cell {cellLocation :: ASLocation, 
					cellExpression :: ASExpression,
					cellValue :: ASValue} deriving (Show, Read, Eq, Generic)

data ASMessage = Message {
  action :: ASAction,
  result :: ASResult,
  payload :: ASPayload
} deriving (Show, Read, Eq, Generic)

-- alphasheets "verbs"
data ASAction = 
  NoAction |
  Evaluate | 
  Get |
  Delete 
  deriving (Show, Read, Eq, Generic)

data ASResult = 
  Success | 
  Failure
  deriving (Show, Read, Eq, Generic)

data ASPayload = 
  PayloadN () |
  PayloadC ASCell | 
  PayloadCL [ASCell] | 
  PayloadL ASLocation | 
  PayloadLL [ASLocation]
  deriving (Show, Read, Eq, Generic)

------------------- convenience -------------------------------------------------

failureMessage :: ASMessage
failureMessage = Message NoAction Failure (PayloadN ())

successMessage :: ASMessage
successMessage = Message NoAction Success (PayloadN ())

lst :: ASValue -> [ASValue]
lst (ValueL l) = l
lst other = [other]

str :: ASValue -> String
str (ValueS s) = s

dbl :: ASValue -> Double
dbl (ValueD d) = d

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
instance ToJSON ASAction
instance FromJSON ASAction
instance ToJSON ASResult
instance FromJSON ASResult
instance ToJSON ASPayload
instance FromJSON ASPayload
instance ToJSON ASMessage
instance FromJSON ASMessage