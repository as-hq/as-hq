{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Cell
  ( module AS.Types.Cell
  , module AS.Types.Eval
  , module AS.Types.CellProps
  ) where

import AS.Types.Eval
import AS.Types.CellProps

import GHC.Generics
import Data.Aeson

import Data.Aeson.Types (Parser)
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)


data ASLanguage = R | Python | OCaml | CPP | Java | SQL | Excel deriving (Show, Read, Eq, Generic)

data ASExpression =
    Expression { expression :: String, language :: ASLanguage }
  | Coupled { cExpression :: String, cLanguage :: ASLanguage, cType :: ExpandingType, cRangeKey :: RangeKey }
  deriving (Show, Read, Eq, Generic)

xpString :: ASExpression -> String
xpString (Expression xp _) = xp
xpString (Coupled xp _ _ _) = xp

xpLanguage :: ASExpression -> ASLanguage
xpLanguage (Expression _ lang) = lang
xpLanguage (Coupled _ lang _ _) = lang


data ASCell = Cell {cellLocation :: ASIndex,
          cellExpression :: ASExpression,
          cellValue :: ASValue,
          cellProps :: ASCellProps} deriving (Show, Read, Eq, Generic)


-- turning a spreadsheet range into dataframe etc...
-- only needed during at syntax and list decoupling
data RangeDescriptor = RangeDescriptor { descriptorKey :: RangeKey, expandingType :: ExpandingType, attrs :: JSON }
  deriving (Show, Read, Eq, Generic)

-- range keys are used to access range descriptors, which relay metadata about a range of cells
-- e.g. for embedded lists and objects
type RangeKey = String

-- For internal use only. Represents a "cell" that takes up numerous cells (e.g., range(10)).
data FatCell = FatCell { expandedCells :: [ASCell], descriptor :: RangeDescriptor } deriving (Show, Read)
data CompositeCell = Single ASCell | Fat FatCell


instance ToJSON ASExpression where
  toJSON (Expression xp lang) = object ["expression" .= xp,
                                        "language" .= (show lang)]
  toJSON (Coupled xp lang dtype key) = object ["expression" .= xp,
                                               "language" .= (show lang),
                                               "expandingType" .= (show dtype),
                                               "rangeKey" .= key]
instance FromJSON ASExpression where
  parseJSON (Object v) = do
    dType <- (v .:? "expandingType") :: Parser (Maybe ExpandingType)
    case dType of 
      Just _ -> Coupled <$> v .: "expression"
                           <*> v .: "language"
                           <*> v .: "expandingType"
                           <*> v .: "rangeKey"
      Nothing -> Expression <$> v .: "expression" <*> v .: "language"

instance ToJSON ASCell
instance FromJSON ASCell

instance FromJSON RangeDescriptor
instance ToJSON RangeDescriptor

instance ToJSON ASLanguage
instance FromJSON ASLanguage