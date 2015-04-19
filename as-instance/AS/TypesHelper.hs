module AS.TypesHelper where 

import AS.Types
import Import hiding (index)
import Prelude ((!!))
import Text.ParserCombinators.Parsec
import Data.Char

--use this function to decompose list of into a list of cells
--e.g. [B1:B5]-> [B1,B2,B3,B4,B5]
decomposeLocs :: [ASLocation] -> [ASLocation]
decomposeLocs [] =[]
decomposeLocs ((Index x):rst) = (Index x):(decomposeLocs rst)
decomposeLocs ((Range x):rst) = (map Index [(m,n)|m<-[a..c], n<-[b..d]])++(decomposeLocs rst)
	where 
	(a,b)= fst x 
	(c,d)= snd x

locationToExcel :: ASLocation -> String
locationToExcel loc = case loc of 
	(Index a) -> excelIndex a
	(Range (a,b)) -> excelIndex a ++ ":" ++ excelIndex b
	where
		excelIndex :: (Int, Int) -> String
		excelIndex (a,b) = (['A'..'Z'] !! (a-1)):(show b)

sameCol :: ASCell -> ASCell -> Bool
sameCol cell1 cell2 = (fst (index (cellLocation cell1))) == (fst (index (cellLocation cell2)))

sameRow :: ASCell -> ASCell -> Bool
sameRow cell1 cell2  = (snd (index (cellLocation cell1))) == (snd (index (cellLocation cell2)))

