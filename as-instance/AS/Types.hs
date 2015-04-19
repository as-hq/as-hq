module AS.Types (
decomposeLocs) where

-- NOTE: follow excel (col, row) ordering
data ASLocation = Index (Int, Int) | Range ((Int, Int), (Int, Int))
index :: ASLocation -> (Int, Int)
index (Index a) = a
range :: ASLocation -> ((Int, Int), (Int, Int))
range (Range a) = a

data ASValue = ValueS String | ValueD Double

data ASCell = Cell {cellLoc :: ASLocation, 
					cellExpr :: ASExpression,
					cellVal :: ASValue} 
-- for convenience, access fields as "cellVal somecell" etc.

data ASExpression = Expression String
expression :: ASExpression -> String
expression (Expression a) = a


-- convenience funcs

--use this function to decompose list of into a list of cells
--e.g. [B1:B5]-> [B1,B2,B3,B4,B5]
decomposeLocs :: [ASLocation] -> [ASLocation]
decomposeLocs [] =[]
decomposeLocs ((Index x):rst) = (Index x):(decomposeLocs rst)
decomposeLocs ((Range x):rst) = (map toIndex [(m,n)|m<-[a..c], n<-[b..d]])++(decomposeLocs rst)
	where 
	(a,b)= index(fst(range x))
	(c,d)= index(snd(range x))

locationToExcel :: ASLocation -> String
locationToExcel loc = case loc of 
	(Index a) -> excelIndex a
	(Range (a,b)) -> excelIndex a ++ ":" ++ excelIndex b
	where
		excelIndex :: (Int, Int) -> String
		excelIndex (a,b) = (['A'..'Z'] !! (a-1)):(show b)

sameCol :: ASCell -> ASCell -> Bool
sameCol cell1 cell2 = (fst (index (cellLoc cell1))) == (fst (index (cellLoc cell2)))

sameRow :: ASCell -> ASCell -> Bool
sameRow cell1 cell2  = (snd (index (cellLoc cell1))) == (snd (index (cellLoc cell2)))
