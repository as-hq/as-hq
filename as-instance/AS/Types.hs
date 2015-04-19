module AS.Types where

import Import
import Database.Persist.TH

-- NOTE: follow excel (col, row) ordering
data ASLocation = Index {index :: (Int, Int)} | Range {range :: ((Int, Int), (Int, Int))}
	deriving (Show, Read, Eq)
derivePersistField "ASLocation"

data ASValue = ValueS String | ValueD Double | ValueLD [Double] | ValueLS [String]
	deriving (Show, Read, Eq)
derivePersistField "ASValue"

data ASExpression = Expression {expression :: String}
	deriving (Show, Read, Eq)
derivePersistField "ASExpression"

data ASCell = Cell {cellLocation :: ASLocation, 
					cellExpression :: ASExpression,
					cellValue :: ASValue}
	deriving (Show, Read, Eq)

-- convenience funcs

