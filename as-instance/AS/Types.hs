module AS.Types where

import Import
import Database.Persist.TH
import Data.Aeson

-- NOTE: follow excel (col, row) ordering
data ASLocation = Index {index :: (Int, Int)} | Range {range :: ((Int, Int), (Int, Int))}
	deriving (Show, Read, Eq, Ord, Generic)
derivePersistField "ASLocation"

instance ToJSON ASLocation
instance FromJSON ASLocation

data ASValue = ValueS String | ValueD Double | ValueLD [Double] | ValueLS [String]
	deriving (Show, Read, Eq, Generic)
derivePersistField "ASValue"

instance ToJSON ASValue
instance FromJSON ASValue

data ASExpression = Expression {expression :: String}
	deriving (Show, Read, Eq, Generic)
derivePersistField "ASExpression"

instance ToJSON ASExpression
instance FromJSON ASExpression

data ASCell = Cell {cellLocation :: ASLocation, 
					cellExpression :: ASExpression,
					cellValue :: ASValue}
	deriving (Show, Read, Eq, Generic)

instance ToJSON ASCell
instance FromJSON ASCell

-- convenience funcs

