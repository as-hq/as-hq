module AS.Kernels.Excel.Util where

import AS.Prelude

import AS.Types.Cell
import AS.Types.Errors
import AS.Types.Excel
import AS.Types.Formats
import AS.Types.Shift

import AS.Kernels.Excel.Compiler
import AS.Util
import AS.DB.API

import qualified Data.Text as T
import Data.Either
import Data.Maybe hiding (fromJust)
import Data.List hiding (head, tail)
import qualified Data.Vector as V

import Control.Lens
import Control.Monad.Trans.Either

import System.IO.Unsafe
import Database.Redis (Connection)

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
matrixIndex :: Coord -> EMatrix -> EValue
matrixIndex coord (EMatrix numCols numRows v) = (V.!) v (r*numCols+c)
  where r = coord^.row^.int
        c = coord^.col^.int

-- | Cast ASValue (from CellMap) to an Excel entity. 
asValueToEntity :: Formatted ASValue -> Maybe EEntity
asValueToEntity v = EntityVal <$> toEValue v

-- | Returns the dimensions of a matrix in col,row format
matrixDim :: EMatrix -> (Int,Int)
matrixDim (EMatrix c r _) = (c,r)

matrixTranspose :: EMatrix -> EMatrix
matrixTranspose m@(EMatrix c r v) = EMatrix c r $ V.fromList $ concat $ transpose $ matrixTo2DList m

-- | If a matrix is a 1xn or nx1, return that vector, otherwise return Nothing
to1D :: EMatrix -> Maybe (V.Vector EValue)
to1D (EMatrix c r v)
  | or [c==1,r==1] = Just v
  | otherwise = Nothing

-------------------------------------------------------------------------------------------------------------
-- | Conversions to/from EResult

-- | Convert a result to a value. Used when mapping array formulas; eval gives a bunch of results
-- that need to be casted back to a EMatrix

valToResult :: EValue -> EResult
valToResult = Right . EntityVal

entityToVal :: EEntity -> ThrowsError EValue
entityToVal (EntityVal v) = Right v
entityToVal other = Left $ ArrayFormulaUnMappable

valToEitherT :: EValue -> EitherT EError IO EEntity
valToEitherT = return . EntityVal

stringResult :: String -> EResult
stringResult = Right . EntityVal . EValueS

locToResult :: ASReference -> EResult
locToResult = Right . EntityRef . ERef

locToEitherT :: ASReference -> EitherT EError IO EEntity
locToEitherT = right . EntityRef . ERef

doubleToResult :: Double -> EResult
doubleToResult = valToResult . EValueNum . return . EValueD

intToResult :: Int ->  EResult
intToResult = valToResult . EValueNum . return . EValueI . fromIntegral

-------------------------------------------------------------------------------------------------------------
-- | Other conversion functions

eValToASValue :: EValue -> Formatted ASValue
eValToASValue (EValueS s) = return $ ValueS s
eValToASValue (EValueNum (Formatted (EValueD d) f)) = Formatted (ValueD d) f
eValToASValue (EValueNum (Formatted (EValueI i) f)) = Formatted (ValueI i) f
eValToASValue (EValueB b) = return $ ValueB b
eValToASValue (EBlank)    = return NoValue
eValToASValue (EMissing)  = return NoValue
eValToASValue (EValueE s) = return $ ValueError s ""

toEValue :: Formatted ASValue -> Maybe EValue
toEValue (Formatted (ValueS s) _) = Just $ EValueS s
toEValue (Formatted (ValueB b) _) = Just $ EValueB b
toEValue (Formatted (ValueD d) f) = Just $ EValueNum $ Formatted (EValueD d) f
toEValue (Formatted (ValueI i) f) = Just $ EValueNum $ Formatted (EValueI i) f
toEValue (Formatted (NoValue)  _) = Just EBlank
toEValue v = Nothing

cellToFormattedVal :: Maybe ASCell -> Formatted ASValue
cellToFormattedVal (Just c) = Formatted cv f
  where 
    cv = c^.cellValue
    f = getCellFormat c
cellToFormattedVal Nothing = return NoValue

-- | The use of unsafePerformIO is temporary. Eventually a lot of this code may move into IO because of
-- things like rand.
dbLookup :: Connection -> ASIndex -> Formatted ASValue
dbLookup conn = cellToFormattedVal . unsafePerformIO . (getCell conn)

dbLookupBulk :: Connection -> [ASIndex] -> [Formatted ASValue]
dbLookupBulk conn = (map cellToFormattedVal) . unsafePerformIO . (getCells conn)

-------------------------------------------------------------------------------------------------------------
-- | AS Reference utility methods

topLeftLoc :: ASReference -> Coord
topLeftLoc (IndexRef (Index _ x)) = x
topLeftLoc (RangeRef (Range _ (a,_))) = a

dimension :: ASReference -> (Int,Int)
dimension r = ((getFiniteWidth r)^.int, (getFiniteHeight r)^.int)

-- | Given current reference and another reference, only return the relevant part of the second reference
-- | Returns Nothing if the second reference is 2D, or if no intersection possible
-- | Leave ASIndex's alone (nothing to scalarize)
-- | Ex. ABS(A1:A3) while on B2 -> ABS(A2)
scalarizeLoc :: ASReference -> ASReference -> Maybe ASReference
scalarizeLoc (IndexRef i) j@(IndexRef _) = Just j
scalarizeLoc (IndexRef (Index sh1 indexCoord)) r@(RangeRef (Range sh2 (rangeCoord1, rangeCoord2)))
  | sh1 /= sh2 = Nothing
  | h /= 1 && w/= 1 = Nothing
  | h == 1 = if e>=a && e<=c
    then Just $ IndexRef $ Index sh1 (makeCoord e b) -- intersect column;  b==d
    else Nothing
  | w == 1 = if f>=b && f<=d
    then Just $ IndexRef $ Index sh1 (makeCoord a f) -- intersect row;  a==c
    else Nothing
    where
      a = rangeCoord1^.col
      b = rangeCoord1^.row
      c = rangeCoord2^.finiteCol
      d = rangeCoord2^.finiteRow
      e = indexCoord^.col
      f = indexCoord^.row
      h = d-b+1
      w = c-a+1

-- | TODO: finish
-- | Find the rectangular intersection of two locations (space operator in Excel)
locIntersect :: ASReference -> ASReference -> Maybe ASReference
locIntersect a b = Just b

-- | If the second loc doesn't have the same dim as the first, fix it
-- | Used for sumif-type functions to "extend" the second range
matchDimension :: ERef -> ERef -> ERef
matchDimension (ERef r1) (ERef r2) = if topLeft == botRight
  then ERef $ IndexRef $ Index sh topLeft
  else ERef $ RangeRef $ Range sh (topLeft, toExtendedCoord $ botRight)
  where
    topLeft = topLeftLoc r2
    o = Offset (getFiniteWidth r1 - Col 1) (getFiniteHeight r1 - Row 1)
    botRight = shiftByOffset o topLeft
    sh = refSheetId r2

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
  | (length ($head b) == 0) = Nothing
  | otherwise = Just $ ($head . $head) b

-- | Check if all elements of a list are the same
allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== $head xs) ($tail xs)

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
getError = $head

compressErrors :: [ThrowsError a] -> ThrowsError [a]
compressErrors x
  | (any isLeft x) = Left $ getError (lefts x)
  | otherwise      = Right $ rights x










