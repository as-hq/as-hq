data ASCellIndex = Index (Int, Int)
index :: ASCellIndex -> (Int, Int)
index (Index a) = a

data ASCellRange = Range (ASCellIndex, ASCellIndex)
range :: ASCellRange -> (ASCellIndex, ASCellIndex) 
range (Range a) = a

data ASLocation = IndexL ASCellIndex | RangeL ASCellRange

{--func :: ASLocation -> whatever
func (IndexL idx) = 
func (RangeL rng) = --} 

data ASValue = ValueS String | ValueD Double

data ASCell = Cell (ASLocation, ASExpression, ASValue)
cell :: ASCell -> (ASLocation, ASExpression, ASValue)
cell (Cell a) = a

{--cellLocation :: ASCell -> ASLocation
cellLocation (a, _, _) = a

cellExpression :: ASCell -> ASExpression
cellExpression (_, a, _) = a

cellValue :: ASCell -> ASValue
cellValue (_, _, a) = a--}

data ASExpression = Expression String
expression :: ASExpression -> String
expression (Expression a) = a