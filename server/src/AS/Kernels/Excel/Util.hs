module AS.Kernels.Excel.Util where

import AS.Types.Core
import AS.Types.Excel
import qualified Data.Text as T

import AS.Kernels.Excel.Compiler
import Data.Either
import Data.Maybe
import Data.List
import qualified Data.Vector as V

import AS.DB.API

import System.IO.Unsafe

-------------------------------------------------------------------------------------------------------------
-- | Matrix methods

-- | Converts a EMatrix to a 2D row major matrix of EValue's
matrixTo2DList :: EMatrix -> [[EValue]]
matrixTo2DList (EMatrix c 1 v) = [V.toList v]
matrixTo2DList (EMatrix c r v) = (V.toList firstRow):otherRows
  where
    (firstRow,rest) = V.splitAt c v
    otherRows = matrixTo2DList (EMatrix c (r-1) rest)

-- | Extracts an element of a matrix
matrixIndex :: (Int,Int) -> EMatrix -> EValue
matrixIndex (c,r) (EMatrix numCols numRows v) = (V.!) v (r*numCols+c)

-- | Cast ASValue (from RefValMap) to an Excel entity. Ignoring ValueL for now; cannot be in the map.  
asValueToEntity :: ASValue -> Maybe EEntity
asValueToEntity v = case (toEValue v) of 
  Nothing -> Nothing
  Just v -> Just $  EntityVal v

-- | Returns the dimensions of a matrix in col,row format
matrixDim :: EMatrix -> (Int,Int) 
matrixDim (EMatrix c r _) = (c,r)

-- | If a matrix is a 1xn or nx1, return that vector, otherwise return Nothing
to1D :: EMatrix -> Maybe (V.Vector EValue)
to1D (EMatrix c r v) 
  | or [c==1,r==1] = Just v
  | otherwise = Nothing

-------------------------------------------------------------------------------------------------------------
-- | Conversions to/from EResult

-- | Convert a result to a value. Used when mapping array formulas; eval gives a bunch of results
-- that need to be casted back to a EMatrix
resToVal :: EResult -> ThrowsError EValue
resToVal (Right (EntityVal v)) = Right v
resToVal (Left e) = Left e
resToVal other = Left $ ArrayFormulaUnMappable

valToResult :: EValue -> EResult
valToResult = Right . EntityVal

stringResult :: String -> EResult
stringResult = Right . EntityVal . EValueS

locToResult :: ASReference -> EResult
locToResult = Right . EntityRef . ERef

doubleToResult :: Double -> EResult
doubleToResult = valToResult . EValueNum . EValueD

intToResult :: Int ->  EResult
intToResult = valToResult . EValueNum . EValueI

-------------------------------------------------------------------------------------------------------------
-- | Other conversion functions

toASValue :: EValue -> ASValue 
toASValue (EValueS s) = ValueS s
toASValue (EValueNum (EValueD d)) = ValueD d
toASValue (EValueNum (EValueI i)) = ValueI i
toASValue (EValueB b) = ValueB b
toASValue (EBlank)    = NoValue

toEValue :: ASValue -> Maybe EValue
toEValue (ValueS s) = Just $ EValueS s
toEValue (ValueB b) = Just $ EValueB b
toEValue (ValueD d) = Just $ EValueNum $ EValueD d
toEValue (ValueI i) = Just $ EValueNum $ EValueI i
toEValue (NoValue) = Just EBlank
toEValue v = Nothing

dealWithBlank :: Maybe ASCell -> ASValue 
dealWithBlank (Just c) = cellValue c
dealWithBlank Nothing = NoValue

-- | The use of unsafePerformIO is temporary. Eventually a lot of this code may move into IO because of 
-- things like rand.
dbLookup :: ASIndex -> ASValue
dbLookup = dealWithBlank . unsafePerformIO . getCell

dbLookupBulk :: [ASIndex] -> [ASValue]
dbLookupBulk = (map dealWithBlank) . unsafePerformIO . getCells 

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

-- | Given current reference and another reference, only return the relevant part of the second reference
-- | Returns Nothing if the second reference is 2D, or if no intersection possible
-- | Leave ASIndex's alone (nothing to scalarize)
-- | Ex. ABS(A1:A3) while on B2 -> ABS(A2)
scalarizeLoc :: ASReference -> ASReference -> Maybe ASReference
scalarizeLoc (IndexRef i) j@(IndexRef _) = Just j
scalarizeLoc (IndexRef (Index sh1 (e,f))) r@(RangeRef (Range sh2 ((a,b),(c,d)))) 
  | sh1 /= sh2 = Nothing
  | h /= 1 && w/= 1 = Nothing
  | h == 1 = if e>=a && e<=c 
    then Just $ IndexRef $ Index sh1 (e,b) -- intersect column;  b==d
    else Nothing
  | w == 1 = if f>=b && f<=d
    then Just $ IndexRef $ Index sh1 (a,f) -- intersect row;  a==c
    else Nothing
    where
      h = d-b+1
      w = c-a+1

-- | TODO: finish
-- | Find the rectangular intersection of two locations (space operator in Excel)
locIntersect :: ASReference -> ASReference -> Maybe ASReference
locIntersect a b = Just b

-- | If the second loc doesn't have the same dim as the first, fix it
-- | Used for sumif-type functions to "extend" the second range
matchDimension :: ERef -> ERef -> ERef
matchDimension (ERef r1) (ERef r2) = if (a,b) == (c,d)
  then ERef $ IndexRef $ Index sh (a,b)
  else ERef $ RangeRef $ Range sh ((a,b),(c,d))
  where
    (a,b) = topLeftLoc r2
    (c,d) = (a + getWidth r1 -1, b + getHeight r1 -1)
    sh = shName r2

extractRefs :: [EEntity] -> [ERef]
extractRefs [] = []
extractRefs ((EntityRef e):es) = e:(extractRefs es)

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

-- | Check if all elements of a list are the same
allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

-- | All the elements in the list are either 1 or equal to some common value [1,3,3,1,1,3]
allTheSameOrOne :: (Eq a, Num a) => [a] -> Bool
allTheSameOrOne xs = allTheSame notOnes
  where
    (ones,notOnes) = partition (==1) xs

-- | Equivalent to "is not a matrix"
isBasic :: EEntity -> Bool
isBasic (EntityMatrix m) = False
isBasic e = True

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









