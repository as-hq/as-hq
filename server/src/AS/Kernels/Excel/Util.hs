module AS.Kernels.Excel.Util where

import AS.Types.Core
import AS.Types.Excel
import qualified Data.Text as T
import AS.Parsing.Out (exRefToASRef)


import AS.Kernels.Excel.Compiler
import Data.Either
import Data.Maybe
import Data.List
import qualified Data.Vector as V

-------------------------------------------------------------------------------------------------------------
-- | Matrix methods

matrixToLsts :: EMatrix -> [[EValue]]
matrixToLsts (EMatrix c r v) = (V.toList firstRow):(matrixToLsts (EMatrix c r rest))
	where 
		(firstRow,rest) = V.splitAt c v

matrixIndex :: (Int,Int) -> EMatrix -> EValue
matrixIndex (c,r) (EMatrix numCols numRows v) = (V.!) v (r*numCols+c)

-- ignoring ValueL
asValueToEntity :: ASValue -> Maybe EEntity
asValueToEntity v = case (toEValue v) of 
	Nothing -> Nothing
	Just v -> Just $  EntityVal v

	{-
	asValueToEntity v@(ValueL rows)
	| (allPrimitive rows) = Just $ EntityMatrix $ EMatrix (length rows) 1 (V.fromList $ map (fromJust . toEValue) rows)
	| (allRowsPrimitive rows) = Just $ EntityMatrix $ EMatrix (valLength (head rows)) (length rows) (V.concat $ map (\row -> V.fromList (map (fromJust . toEValue) (flattenValue row))) rows)
	| otherwise = Nothing -- Not a 2D array -}

-- | Returns the dimensions of a matrix in col,row format
matrixDim :: EMatrix -> (Int,Int) 
matrixDim (EMatrix c r _) = (c,r)

-- | If a matrix is a 1xn or nx1, return that vector, otherwise return Nothing
to1D :: EMatrix -> Maybe (V.Vector EValue)
to1D (EMatrix c r v) 
	| or [c==1,r==1] = Just v
	| otherwise = Nothing

-------------------------------------------------------------------------------------------------------------
-- | Conversions

resToVal :: EResult -> ThrowsError EValue
resToVal (Right (EntityVal v)) = Right v
resToVal (Left e) = Left e
resToVal other = Left $ Default "slgs"

stringResult :: String -> EResult
stringResult = Right . EntityVal . EValueS

locToResult :: ASReference -> EResult
locToResult = Right . EntityRef . ERef

doubleToResult :: Double -> EResult
doubleToResult = valToResult . EValueNum . EValueD

intToResult :: Int ->  EResult
intToResult = valToResult . EValueNum . EValueI

-- add error conversion
toASValue :: EValue -> ASValue 
toASValue (EValueS s) = ValueS s
toASValue (EValueNum (EValueD d)) = ValueD d
toASValue (EValueNum (EValueI i)) = ValueI i
toASValue (EValueB b) = ValueB b
toASValue (EBlank)    = NoValue

-- | Maps some ASValues to EValue's
toEValue :: ASValue -> Maybe EValue
toEValue (ValueS s) = Just $ EValueS s
toEValue (ValueB b) = Just $ EValueB b
toEValue (ValueD d) = Just $ EValueNum $ EValueD d
toEValue (ValueI i) = Just $ EValueNum $ EValueI i
toEValue (NoValue) = Just EBlank
toEValue v = Nothing

flattenValue :: ASValue -> [ASValue]
flattenValue (ValueL l) = l
flattenValue x = [x]

toASLoc :: ASSheetId -> ExRef -> ASReference
toASLoc s e = exRefToASRef s e

valToResult :: EValue -> EResult
valToResult = Right . EntityVal
-------------------------------------------------------------------------------------------------------------

extractRefs :: [EEntity] -> [ERef]
extractRefs [] = []
extractRefs ((EntityRef e):es) = e:(extractRefs es)

-------------------------------------------------------------------------------------------------------------
-- | AS Value methods

allPrimitive :: [ASValue] -> Bool
allPrimitive [] = True
allPrimitive ((ValueL l):vs) = False
allPrimitive v = True

allRowsPrimitive :: [ASValue] -> Bool 
allRowsPrimitive [] = True
allRowsPrimitive ((ValueL lst):vs) = and [(allPrimitive lst),allRowsPrimitive vs]
allRowsPrimitive _ = False

valLength :: ASValue -> Int
valLength (ValueL lst) = length lst
valLength x = 1

-------------------------------------------------------------------------------------------------------------
-- | AS Reference utility methods

shName ::  ASReference ->  ASSheetId
shName (IndexRef (Index s _)) = s
shName (RangeRef (Range s _)) = s

topLeftLoc :: ASReference -> (Int,Int)
topLeftLoc (IndexRef (Index _ x)) = x
topLeftLoc (RangeRef (Range _ (a,_))) = a

getHeight :: ASReference -> Int
getHeight (IndexRef (Index _ _)) = 1
getHeight (RangeRef (Range _ ((_,b),(_,d)))) = d-b+1

getWidth :: ASReference -> Int
getWidth (IndexRef (Index _ _)) = 1
getWidth (RangeRef (Range _ ((a,_),(c,_)))) = c-a+1

isRange :: ASReference -> Bool
isRange (IndexRef _) = False
isRange (RangeRef _) = True

dimension :: ASReference -> (Int,Int)
dimension (IndexRef (Index _ _)) = (1,1)
dimension (RangeRef (Range _ ((a,b),(c,d)))) = (c-a+1,d-b+1)

-- | TODO: finish
scalarizeLoc :: ASReference -> ASReference -> Maybe ASReference
scalarizeLoc cl l = Just l

-- | TODO: finish
-- | Find the rectangular intersection of two locations
locIntersect :: ASReference -> ASReference -> Maybe ASReference
locIntersect a b = Just b

-- | TODO: finish
-- | If the second loc doesn't have the same dim as the first, fix it
-- | Used for sumif-type functions to "extend" the second range
matchDimension :: ERef -> ERef -> ERef
matchDimension l1 l2 = l2

-------------------------------------------------------------------------------------------------------------
-- | General utilities

-- | Make sure that the list of lists is rectangular
aligned :: [[a]] -> Bool
aligned lst = allTheSame lenRows
	where 
		lenRows = map length lst

-- | [[1,2],[3,4]] -> 1 if not empty
topLeftLst :: [[a]] -> Maybe a
topLeftLst b 
	| (length b == 0) = Nothing
	| (length (head b) == 0) = Nothing
	| otherwise = Just $ (head . head) b

max' :: Ord a => a -> a -> a
max' j k = if j > k
    then j
    else k

min' :: Ord a => a -> a -> a
min' j k = if j < k
    then j
    else k

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

-- | All the elements in the list are either 1 or equal to some common value [1,3,3,1,1,3]
allTheSameOrOne :: (Eq a, Num a) => [a] -> Bool
allTheSameOrOne xs = allTheSame notOnes
	where
		(ones,notOnes) = partition (==1) xs

isBasic :: EEntity -> Bool
isBasic (EntityMatrix m) = False
isBasic e = True

---------------------------------------------------------------------------------------------------
-- | Parsing utilities

-- | TODO: need parsing on string to cast to int, float, etc
leafEval :: String -> Either EError EEntity
leafEval s = Right $ EntityVal $ EValueS s
--EValueNum $ EValueI (read s :: Int)

---------------------------------------------------------------------------------------------------
-- | Error Handling

errorVal :: EValue -> Bool 
errorVal (EValueE _) = True
errorVal _ = False

-- | If any element of the matrix is an error, return the first such instance; else return the matrix
matrixError :: EMatrix -> ThrowsError EMatrix
matrixError m@(EMatrix _ _ v) = do 
	let errs = V.filter (errorVal) v
	if (V.null errs)
		then Right m
		else do 
			let (EValueE s) = V.head errs
			Left $ Default s

getError :: [EError] -> EError
getError = head

compressErrors :: [ThrowsError a] -> ThrowsError [a]
compressErrors x 
	| (any isLeft x) = Left $ getError (lefts x)
	| otherwise      = Right $ rights x










