{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module AS.Types.Locations
  ( module AS.Types.Locations
  , module AS.Types.Migrations.Locations
  , module AS.Types.Sheets
  , module AS.Types.Infinites
  ) where

import AS.Prelude hiding (isInfinite)

import AS.Types.Migrations.Locations
import AS.Types.Infinites

import AS.Types.Sheets

import Data.Aeson
import Data.Hashable
import Data.SafeCopy
import Data.Maybe (fromJust)

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)

-- We want O(n log n) unions for collections of locations and datatypes that depend on them
-- (e.g. ASCell), so we're making them instances of Ord. 
newtype GeneralCol a = Col {_generalColInt :: a} deriving (Show, Read, Num, Eq, Ord, Generic, Data, Typeable)
newtype GeneralRow a = Row {_generalRowInt :: a} deriving (Show, Read, Num, Eq, Ord, Generic, Data, Typeable)
makeFields ''GeneralCol
makeFields ''GeneralRow
-- :t (Col (10 :: Int)) = Col
-- :t (Row (10 :: Int)) = Row
type Col = GeneralCol Int
type Row = GeneralRow Int

instance Enum Col where
  toEnum a         = Col a
  fromEnum (Col a) = a
instance Enum Row where
  toEnum a         = Row a
  fromEnum (Row a) = a

type Coord = (Col, Row)

-- function for creating coordinates.
makeCoord :: Col -> Row -> (Col, Row)
makeCoord = (,)
col :: Lens' Coord Col
col = _1
row :: Lens' Coord Row
row = _2

-- Extended Coord type is used as the bottom right of an ASRange. THIs is since
-- some ranges can be infinite. E.g. A1:A.
type ExtendedCoord = (Infinite Col, Infinite Row)
makeExtendedCoord :: Infinite Col -> Infinite Row -> (Infinite Col, Infinite Row)
makeExtendedCoord = (,)
extendedCol :: Lens' ExtendedCoord (Infinite Col)
extendedCol = _1
extendedRow :: Lens' ExtendedCoord (Infinite Row)
extendedRow = _2

-- Create a lens for ExtendedCoord, that lets you directly access the col and row.
-- finiteness of the makeCoord MUST be checked. fromInfinite can potentially error.
finiteCol :: Lens' ExtendedCoord Col
finiteCol = lens (\eCoord -> fromInfinite $ eCoord^.extendedCol) (\eCoord a -> eCoord & extendedCol .~ Finite a)
finiteRow :: Lens' ExtendedCoord Row
finiteRow = lens (\eCoord -> fromInfinite $ eCoord^.extendedRow) (\eCoord a -> eCoord & extendedRow .~ Finite a)


-- Checks whether an extended makeCoord has finite col and row.
isCoord :: ExtendedCoord -> Bool
isCoord extendedCoord =
  isFinite (extendedCoord^.extendedCol) && isFinite (extendedCoord^.extendedRow)

-- If makeCoord is entire finite, get number back. Otherwise, error.
fromExtendedCoord :: ExtendedCoord -> Coord
fromExtendedCoord extendedCoord =
  let col = fromInfinite $ extendedCoord^.extendedCol
      row = fromInfinite $ extendedCoord^.extendedRow
  in
  makeCoord col row

toExtendedCoord :: Coord -> ExtendedCoord
toExtendedCoord coord = makeExtendedCoord (Finite $ coord^.col) (Finite $ coord^.row)


data Dimensions = Dimensions {width :: Col, height :: Row} deriving (Show, Read, Eq, Ord, Generic)
-- a and b are always Row and Col respectively.
data GeneralOffset a b = Offset { dCol :: a, dRow :: b }
type Offset = GeneralOffset Col Row
-- NORM: All ExtendedRects should be oriented.
type ExtendedRect = (Coord, ExtendedCoord)

--------------------------------------------------------------------------------------------------------------------------------------------
-- Locations

-- Template AlphaSheets operators. 
-- example: !{A1} 
--          ^ sampling operator causes a run-time reevaluation
data ASTemplateExpr = SampleExpr { samples :: Int, sampledIndex :: ASIndex}
  deriving (Show, Read, Eq, Ord, Generic, Data)

data ASIndex = Index { _locSheetId :: SheetID, _index :: Coord } 
  deriving (Show, Read, Eq, Generic, Data, Typeable, Ord)
data ASPointer = Pointer { pointerIndex :: ASIndex } 
  deriving (Show, Read, Eq, Ord, Generic, Data, Typeable)
-- NORM: All Ranges should be oriented. This is something that a lot of
-- code relies on. This is guaranteed when ranges are constructed via functions
-- 'range' and 'colRange' defined below in this file.
data ASRange = Range {rangeSheetId :: SheetID, rect :: ExtendedRect }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data ASReference = 
    IndexRef ASIndex 
  | RangeRef ASRange 
  | PointerRef ASPointer 
  | TemplateRef ASTemplateExpr
  | OutOfBounds
  deriving (Show, Read, Eq, Ord, Generic, Data)

makeLenses ''ASIndex

refSheetId :: ASReference -> SheetID
refSheetId (IndexRef i)   = i^.locSheetId
refSheetId (RangeRef r)   = rangeSheetId r
refSheetId (PointerRef p) = (view locSheetId) . pointerIndex $ p
refSheetId (TemplateRef t) = case t of 
  SampleExpr _ idx -> idx^.locSheetId

-- Creates a range from two coordinates. Result guaranteed to be finite.
-- Requires that coords are oriented.
makeFiniteRange :: SheetID -> Coord -> Coord -> ASRange
makeFiniteRange sid coord1 coord2 = Range sid $ makeFiniteRect coord1 coord2

-- Creates an oriented rectangle from two coordinates.
makeFiniteRect :: Coord -> Coord -> ExtendedRect
makeFiniteRect coord1 coord2 =  (coord1', coord2')
  where 
    coord1' =
      makeCoord (min (coord1^.col) $ coord2^.col) (min (coord1^.row) $ coord2^.row)
    coord2' =
      makeExtendedCoord
      (max (Finite $ coord1^.col) $ Finite $ coord2^.col) (max (Finite $ coord1^.row) $ Finite $ coord2^.row) 

-- Creates a range from two coordinates. Result guaranteed to be a column range.
-- Automatically orients the colRange properly.
makeColRange :: SheetID -> Coord -> Col -> ASRange
makeColRange sid coord1 col2 = Range sid $ makeColRect coord1 col2

makeColRect :: Coord -> Col -> ExtendedRect
makeColRect coord1 column = 
  let col1 = min (coord1^.col) column
      col2 = max (coord1^.col) column
  in 
  (coord1 & (col .~ col1), makeExtendedCoord (Finite col2) Infinite)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

instance ToJSON ASIndex where
  toJSON (Index sid coord) = object ["tag"     .= ("index" :: String),
                                     "sheetId" .= sid,
                                     "index"   .= object ["row" .= (coord^.row^.int),
                                                          "col" .= (coord^.col^.int)]]

instance FromJSON ASIndex where
  parseJSON (Object v) = do
    loc <- v .: "index"
    sid <- v .: "sheetId"
    idx <- (,) <$> (Col <$> loc .: "col") <*> (Row <$> loc .: "row")
    return $ Index sid idx
  parseJSON _          = fail "client message JSON attributes missing"

instance ToJSON ASPointer where
  toJSON (Pointer (Index sid coord)) = object ["tag"     .= ("index" :: String),
                                               "sheetId" .= sid,
                                               "index"   .= object ["row" .= (coord^.row^.int),
                                                                    "col" .= (coord^.col^.int) ]]

instance FromJSON ASPointer where
  parseJSON (Object v) = do
    loc <- v .: "index"
    sid <- v .: "sheetId"
    idx <- (,) <$> (Col <$> loc .: "col") <*> (Row <$> loc .: "row")
    return $ Pointer (Index sid idx)
  parseJSON _          = fail "client message JSON attributes missing"

instance ToJSON Coord where
  toJSON coord = object [ "row" .= (coord^.row^.int), "col" .= (coord^.col^.int) ]

instance FromJSON Coord where
  parseJSON (Object v) = 
    (,) <$> (Col <$> v .: "col") <*> (Row <$> v .: "row")
  parseJSON _          = fail "client message JSON attributes missing"

instance ToJSON ExtendedCoord where
  toJSON extendedCoord =
    case (extendedCoord^.extendedCol^?_Finite, extendedCoord^.extendedRow^?_Finite) of
         (Just i, Just j) -> object [ "row" .= j, "col" .= i]
         (Nothing, Just j) -> object [ "row" .= j]
         (Just i, Nothing) -> object [ "col" .= i]
         (Nothing, Nothing) -> object []

instance ToJSON ASRange  where
  toJSON (Range sid (coord, extendedCoord)) = object ["tag" .= ("range" :: String),
                                                      "sheetId" .= sid,
                                                      "range" .= object [
                                                      "tl" .= coord,
                                                      "br" .= extendedCoord]]

instance FromJSON ASRange where
  parseJSON (Object v) = do
    rng <- v .: "range"
    (tl, br) <- (,) <$> rng .: "tl" <*> rng .: "br"
    tl' <- (,) <$> (Col <$> tl .: "col") <*> (Row <$> tl .: "row")
    br' <- (,) <$> (Finite <$> (Col <$> br .: "col")) <*> (Finite <$> (Row <$> br .: "row"))
    sid <- v .: "sheetId"
    return $ Range sid (tl', br')
  parseJSON _          = fail "client message JSON attributes missing"

instance ToJSON ASReference where
  toJSON (IndexRef idx) = toJSON idx
  toJSON (PointerRef p) = toJSON p
  toJSON (RangeRef rng) = toJSON rng

--These ToJSON instances for Col and Row are a patch to make the new col and row
--types work with front end.
instance ToJSON Col where
  toJSON (Col i) = toJSON i
instance ToJSON Row where
  toJSON (Row i) = toJSON i
instance FromJSON Col where
  parseJSON (Object v) = do
    i <- parseJSON (Object v)
    return $ Col i
instance FromJSON Row where
  parseJSON (Object v) = do
    i <- parseJSON (Object v)
    return $ Row i


instance ToJSON Dimensions
instance FromJSON Dimensions

-- deep strict eval instances for R 

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers


-- This returns an error if an infinite range is passed in.
getFiniteHeight :: ASReference -> Row
getFiniteHeight (IndexRef _) = 1
getFiniteHeight (PointerRef _) = 1
getFiniteHeight (RangeRef (Range _ (coord1,extendedCoord2))) =
  let row1 = coord1^.row
      row2 = fromInfinite $ extendedCoord2^.extendedRow
  in
  row2 - row1 + Row 1

-- TODO: refactor getWidth to return a maybeInt, or an Either Int.
-- This returns an error if an infinite range is passed in.
getFiniteWidth :: ASReference -> Col
getFiniteWidth (IndexRef _) = 1
getFiniteWidth (PointerRef _) = 1
getFiniteWidth (RangeRef (Range _ (coord1,extendedCoord2))) =
  let col1 = coord1^.col
      col2 = fromInfinite $ extendedCoord2^.extendedCol
  in
  col2 - col1 + Col 1

isRange :: ASReference -> Bool
isRange (IndexRef _) = False
isRange (PointerRef _) = False
isRange (RangeRef _) = True

-- tail recursive for speed
containsRange :: [ASReference] -> Bool
containsRange [] = False
containsRange (ref:refs) = case ref of
  (RangeRef _) -> True
  _ -> containsRange refs

-- TODO:#ErrorSource. This requires that ranges are tl < br.
rangeContainsRect :: ASRange -> ExtendedRect -> Bool
rangeContainsRect (Range _ (topLeft1, extendedBottomRight1)) (topLeft2, extendedBottomRight2) = tl && br
  where
    tl = (topLeft2^.col >= topLeft1^.col) && (topLeft2^.row >= topLeft1^.row)
    br = (extendedBottomRight2^.extendedCol <= extendedBottomRight1^.extendedCol) && (extendedBottomRight2^.extendedRow <= extendedBottomRight1^.extendedRow)

-- #needsrefactor we should use a better type than ASReference for this... since this really only makes sense
-- when the ASReference passed in is an index or a range
refsToIndices :: [ASReference] -> [ASIndex]
refsToIndices = concatMap refToIndices
  where
    refToIndices :: ASReference -> [ASIndex]
    refToIndices (IndexRef i) = [i]
    refToIndices (RangeRef r) = finiteRangeToIndices r

-- | Range at ((1,1), (2,2)) --> [(1,1), (2, 1), (1, 2), (2, 2)]
-- Produces a flattened row-major array of locations.
finiteRangeToIndices :: ASRange -> [ASIndex]
finiteRangeToIndices (Range sheet (ul, extendedLr)) = [Index sheet (makeCoord x y) | y <- [starty..endy], x <- [startx..endx] ]
  where
    lr = fromExtendedCoord extendedLr
    startx = min (view col ul) (view col lr)
    endx = max (view col ul) (view col lr)
    starty = min (view row ul) (view row lr)
    endy = max (view row ul) (view row lr)

-- Helper methods for determining whether ranges contain an index, whether
-- ranges overlap, ....

inInterval :: (Ord a) => (a, Infinite a) -> a -> Bool
inInterval (a, extendedB) c = a <= c && Finite c <= extendedB

rectContainsCoord :: ExtendedRect -> Coord -> Bool
rectContainsCoord (topLeft, bottomRight) coord = and [
  inInterval (topLeft^.col, bottomRight^.extendedCol) (coord^.col),
  inInterval (topLeft^.row, bottomRight^.extendedRow) (coord^.row)
  ]

rangeContainsIndex :: ASRange -> ASIndex -> Bool
rangeContainsIndex (Range sid1 rect) (Index sid2 coord) =
  sid1 == sid2 && rectContainsCoord rect coord

rectContainsRect :: ExtendedRect -> ExtendedRect -> Bool
rectContainsRect (tl1, br1) (tl2, br2) =
  intervalContainsInterval (tl1^.col, br1^.extendedCol) (tl2^.col, br2^.extendedCol) &&
    intervalContainsInterval (tl1^.row, br1^.extendedRow) (tl2^.row, br2^.extendedRow)
  where
    intervalContainsInterval (a, extendedB) (c, extendedD) =
      a <= c && extendedD <= extendedB

rangeContainsRange :: ASRange -> ASRange -> Bool
rangeContainsRange (Range sid1 rect1) (Range sid2 rect2) =
  sid1 == sid2 && rectContainsRect rect1 rect2
    where

rangeContainsRef :: ASRange -> ASReference -> Bool
rangeContainsRef r ref = case ref of
  IndexRef i  -> rangeContainsIndex r i
  PointerRef p -> rangeContainsIndex r (pointerIndex p)
  RangeRef r' -> rangeContainsRange r r'
  TemplateRef (SampleExpr _ i) -> rangeContainsIndex r i
  OutOfBounds -> False

-- Gets the indices in a finite two dimensional range.
-- Row major means that range(2) goes to [[0],[1]]
-- If an infinite range is passed in, Haskell will throw an error.
finiteRangeToIndicesRowMajor2D :: ASRange -> [[ASIndex]]
finiteRangeToIndicesRowMajor2D (Range sheet (ul, extendedLr)) = map (\y -> [Index sheet (makeCoord x y) | x <- [startx..endx]]) [starty..endy]
  where
    lr = fromExtendedCoord extendedLr
    startx = min (view col ul) (view col lr)
    endx = max (view col ul) (view col lr)
    starty = min (view row ul) (view row lr)
    endy = max (view row ul) (view row lr)

getTopLeft :: ASRange -> ASIndex
getTopLeft (Range sh (tl,_)) = Index sh tl

-- Haskell throws an error if the ASRange is not finite.
getFiniteRangeDims :: ASRange -> Dimensions
getFiniteRangeDims (Range _ (tl, extendedBr)) =
  Dimensions { width  = (Col 1) + abs (r - l)
             , height = (Row 1) + abs (b - t) }
  where
    br = fromExtendedCoord $ extendedBr
    t = tl^.row
    l = tl^.col
    b = br^.row
    r = br^.col

-- | The offset needed to translate the top left corner of the first range to get the 
-- top left corner of the second. 
getRangeOffset :: ASRange -> ASRange -> Offset
getRangeOffset r1 r2 = getIndicesOffset (getTopLeft r1) (getTopLeft r2)

getIndicesOffset :: ASIndex -> ASIndex -> Offset
getIndicesOffset (Index _ coord1) (Index _ coord2) =
  Offset { dCol = col2 - col1, dRow = row2 - row1 }
  where
    col1 = coord1^.col
    row1 = coord1^.row
    col2 = coord2^.col
    row2 = coord2^.row

pointerAtIndex :: ASIndex -> ASPointer
pointerAtIndex = Pointer

-- Helper methods for determining whether an ASRange is a range, a colRange, a rowRange.
-- This is important, since functions such as list interpolation, show2, and read2, do
-- very different things on colRanges and regular ranges.
isColRange :: ASRange -> Bool
isColRange (Range _ (_, extendedCoord)) = isInfinite $ extendedCoord^.extendedRow

isFiniteRange :: ASRange -> Bool
isFiniteRange (Range _ (_, extendedCoord)) =
  (isFinite $ extendedCoord^.extendedCol) && (isFinite $ extendedCoord^.extendedRow)

-- Requires that a <= extendedB, c <= extendedD
intervalIntersect :: (Ord a) => (a, Infinite a) -> (a, Infinite a) -> Bool
intervalIntersect (a, extendedB) (c, extendedD) = 
  inInterval (a, extendedB) c || inInterval (c, extendedD) a

instance NFData Col                 where rnf = genericRnf
instance NFData Row                 where rnf = genericRnf
instance (NFData a) => NFData (Infinite a)
instance NFData ASIndex             where rnf = genericRnf
instance NFData ASPointer           where rnf = genericRnf
instance NFData ASRange             where rnf = genericRnf
instance NFData ASTemplateExpr      where rnf = genericRnf
instance NFData ASReference         where rnf = genericRnf

instance Hashable Col
instance Hashable Row
instance Hashable (Infinite Col)
instance Hashable (Infinite Row)
instance Hashable ASIndex

deriveSafeCopy 1 'base ''Infinite
deriveSafeCopy 1 'base ''GeneralCol
deriveSafeCopy 1 'base ''GeneralRow

-- ************ MIGRATIONS *************
-- This is used to handle DB migrations when the types in the DB change.

-- Changed makeCoord from Coord Int Int to makeCoord Col Row. 1/27/16. Timchu.
-- Changed Rect to ExtendedRect, ASRanges change accordingly.

data Coord0 = Coord0 { _coordCol0 :: Int, _coordRow0 :: Int }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)
data ASIndex0 = Index0 { _locSheetId0 :: SheetID, _index0 :: Coord0 } 
  deriving (Show, Read, Eq, Generic, Data, Typeable, Ord)
deriveSafeCopy 1 'base ''Coord0
deriveSafeCopy 1 'base ''ASIndex0
deriveSafeCopy 2 'extension ''ASIndex
instance Migrate ASIndex where
  type MigrateFrom ASIndex = ASIndex0
  migrate (Index0 sid (Coord0 c r)) = Index sid $ makeCoord (Col c) $ Row r

data ASRange0 = Range0 { _asRangeSid :: SheetID, _range :: (Coord0, Coord0) } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)
deriveSafeCopy 1 'base ''ASRange0
deriveSafeCopy 2 'extension ''ASRange
instance Migrate ASRange where
  type MigrateFrom ASRange = ASRange0
  migrate (Range0 sid (Coord0 c1 r1, Coord0 c2 r2)) =
    Range sid (makeCoord (Col c1) (Row r1), toExtendedCoord $ makeCoord (Col c2) (Row r2))

data Dimensions0 = Dimensions0 {_dimensionsWidth0 :: Int, _dimensionsHeight0 :: Int} deriving (Show, Read, Eq, Ord, Generic)
deriveSafeCopy 1 'base ''Dimensions0
deriveSafeCopy 2 'extension ''Dimensions
instance Migrate Dimensions where
  type MigrateFrom Dimensions = Dimensions0
  migrate (Dimensions0 a b) = Dimensions (Col a) (Row b)
