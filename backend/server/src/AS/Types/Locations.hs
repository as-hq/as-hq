{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module AS.Types.Locations
  ( module AS.Types.Locations
  , module AS.Types.Migrations.Locations
  , module AS.Types.Sheets
  ) where

import AS.Types.Migrations.Locations

import AS.Types.Sheets

import GHC.Generics
import Data.Aeson
import Data.Hashable
import Data.SafeCopy

import Control.Lens hiding ((.=))
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Control.Lens hiding ((.=))
import Control.Lens.TH

type Col = Int
type Row = Int 

-- We want O(n log n) unions for collections of locations and datatypes that depend on them
-- (e.g. ASCell), so we're making them Ord. 
data Coord = Coord { _coordCol :: Col, _coordRow :: Row } deriving (Show, Read, Eq, Ord, Generic)
makeFields ''Coord
data InfiniteRowCoord = InfiniteRowCoord { _infiniteRowCoordCol :: Int } deriving (Show, Read, Eq, Ord, Generic)
makeFields ''InfiniteRowCoord
data Dimensions = Dimensions {width :: Int, height :: Int} deriving (Show, Read, Eq, Ord, Generic)
data Offset = Offset { dCol :: Int, dRow :: Int }
type Rect = (Coord, Coord)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Locations

data ASIndex = Index { _locSheetId :: ASSheetId, _index :: Coord } 
  deriving (Show, Read, Eq, Generic, Ord)
data ASPointer = Pointer { pointerIndex :: ASIndex } 
  deriving (Show, Read, Eq, Ord, Generic)
data ASRange = Range {rangeSheetId :: ASSheetId, range :: Rect }
  deriving (Show, Read, Eq, Ord, Generic)
data ASColRange = ColRange {colRangeSheetId :: ASSheetId, colRange :: (Coord, InfiniteRowCoord) }
  deriving (Show, Read, Eq, Ord, Generic)
data ASReference = IndexRef ASIndex | ColRangeRef ASColRange | RangeRef ASRange | PointerRef ASPointer | OutOfBounds
  deriving (Show, Read, Eq, Ord, Generic)

makeLenses ''ASIndex

refSheetId :: ASReference -> ASSheetId
refSheetId (IndexRef i) = i^.locSheetId
refSheetId (RangeRef r) = rangeSheetId r
refSheetId (ColRangeRef r) = colRangeSheetId r
refSheetId (PointerRef p) = (view locSheetId) . pointerIndex $ p

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

instance ToJSON InfiniteRowCoord
instance FromJSON InfiniteRowCoord

instance ToJSON ASIndex where
  toJSON (Index sid coord) = object ["tag"     .= ("index" :: String),
                                     "sheetId" .= sid,
                                     "index"   .= object ["row" .= (coord^.row),
                                                          "col" .= (coord^.col)]]

instance FromJSON ASIndex where
  parseJSON (Object v) = do
    loc <- v .: "index"
    sid <- v .: "sheetId"
    idx <- Coord <$> loc .: "col" <*> loc .: "row"
    return $ Index sid idx
  parseJSON _          = fail "client message JSON attributes missing"

instance ToJSON ASPointer where
  toJSON (Pointer (Index sid coord)) = object ["tag"     .= ("index" :: String),
                                               "sheetId" .= sid,
                                               "index"   .= object ["row" .= (coord^.row),
                                                                    "col" .= (coord^.col) ]]

instance FromJSON ASPointer where
  parseJSON (Object v) = do
    loc <- v .: "index"
    sid <- v .: "sheetId"
    idx <- Coord <$> loc .: "col" <*> loc .: "row"
    return $ Pointer (Index sid idx)
  parseJSON _          = fail "client message JSON attributes missing"

instance ToJSON ASRange  where
  toJSON (Range sid (coord1, coord2)) = object ["tag" .= ("range" :: String),
                                                "sheetId" .= sid,
                                                "range" .= object [ 
                                                   "tl" .= object [ "row"  .= (coord1^.row), 
                                                                    "col"  .= (coord1^.col)],
                                                   "br" .= object [ "row"  .= (coord2^.row), 
                                                                    "col"  .= (coord2^.col)]]]
 
instance FromJSON ASRange where
  parseJSON (Object v) = do
    rng <- v .: "range" 
    (tl, br) <- (,) <$> rng .: "tl" <*> rng .: "br"
    tl' <- Coord <$> tl .: "col" <*> tl .: "row"
    br' <- Coord <$> br .: "col" <*> br .: "row"
    sid <- v .: "sheetId"
    return $ Range sid (tl', br')
  parseJSON _          = fail "client message JSON attributes missing"

instance ToJSON ASReference where
  toJSON (IndexRef idx) = toJSON idx
  toJSON (PointerRef p) = toJSON p
  toJSON (RangeRef rng) = toJSON rng
  toJSON (ColRangeRef colrng) = toJSON colrng

instance FromJSON ASReference

--TODO: timchu: not sure if r .= object ["col" .=r2 ] is right. Maybe no list brackets?
                            --Note: r stands for right, not row.
instance ToJSON ASColRange where
  toJSON (ColRange sid (coord,column)) = object["tag" .= ("colRange" :: String),
                                                "sheetId" .= sid,
                                                "colRange" .= object[
                                                "tl" .= object ["row" .= (coord^.row),
                                                                "col" .= (coord^.col)],
                                                "r"  .= object ["col" .= (column^.col)]]]

-- TODO: timchu, check that this actually works.
instance FromJSON ASColRange where
  parseJSON (Object v) = do
    colrng <- v .: "colRange"
    (tl, r) <- (,) <$>  colrng .: "tl" <*> colrng .: "r"
    tl' <- Coord <$> tl .: "col" <*> tl .: "row"
    r' <- r .: "col"
    sid <- v .: "sheetId"
    return $ ColRange sid (tl', r')
  parseJSON _ = fail "client message JSON attributes missing"

instance ToJSON Dimensions
instance FromJSON Dimensions

-- deep strict eval instances for R 

instance NFData Coord               where rnf = genericRnf
instance NFData InfiniteRowCoord    where rnf = genericRnf
instance NFData ASIndex             where rnf = genericRnf
instance NFData ASPointer           where rnf = genericRnf
instance NFData ASRange             where rnf = genericRnf
instance NFData ASColRange          where rnf = genericRnf
instance NFData ASReference         where rnf = genericRnf

instance Hashable Coord
instance Hashable InfiniteRowCoord
instance Hashable ASIndex

deriveSafeCopy 1 'base ''Coord
deriveSafeCopy 1 'base ''InfiniteRowCoord
deriveSafeCopy 1 'base ''ASRange
deriveSafeCopy 1 'base ''ASIndex
deriveSafeCopy 1 'base ''Dimensions

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers


-- TODO: timchu, refactor getHeight to return a maybeInt.
getHeight :: ASReference -> Int
getHeight (IndexRef _) = 1
getHeight (PointerRef _) = 1
getHeight (RangeRef (Range _ (coord1,coord2))) = coord2^.row - coord1^.row + 1

-- TODO: refactor getWidth to return a maybeInt, or an Either Int.
getWidth :: ASReference -> Int
getWidth (IndexRef _) = 1
getWidth (PointerRef _) = 1
getWidth (RangeRef (Range _ (coord1,coord2))) = coord2^.col - coord2^.col + 1
getWidth (ColRangeRef (ColRange _ (coord1, col2))) = coord1^.col - col2^.col + 1

isRange :: ASReference -> Bool
isRange (IndexRef _) = False
isRange (PointerRef _) = False
isRange (RangeRef _) = True
isRange (ColRangeRef _ ) = False

-- tail recursive for speed
containsRange :: [ASReference] -> Bool
containsRange [] = False
containsRange (ref:refs) = case ref of
  (RangeRef _) -> True
  _ -> containsRange refs

-- TODO:#ErrorSource. This requires that ranges are tl < br.
rangeContainsRect :: ASRange -> Rect -> Bool
rangeContainsRect (Range _ (topLeft1, bottomRight1)) (topLeft2, bottomRight2) = tl && br
  where
    tl = (topLeft2^.col >= topLeft1^.col) && (topLeft2^.row >= topLeft1^.row)
    br = (bottomRight2^.col <= bottomRight1^.col) && (bottomRight2^.row <= bottomRight1^.row)

-- #needsrefactor we should use a better type than ASReference for this... since this really only makes sense
-- when the ASReference passed in is an index or a range
refsToIndices :: [ASReference] -> [ASIndex]
refsToIndices = concatMap refToIndices
  where 
    refToIndices :: ASReference -> [ASIndex]
    refToIndices (IndexRef i) = [i] 
    refToIndices (RangeRef r) = rangeToIndices r

-- | Range at ((1,1), (2,2)) --> [(1,1), (2, 1), (1, 2), (2, 2)]
-- Produces a flattened row-major array of locations.
rangeToIndices :: ASRange -> [ASIndex]
rangeToIndices (Range sheet (ul, lr)) = [Index sheet (Coord x y) | y <- [starty..endy], x <- [startx..endx] ]
  where
    startx = min (view col ul) (view col lr)
    endx = max (view col ul) (view col lr)
    starty = min (view row ul) (view row lr)
    endy = max (view row ul) (view row lr)

rangeContainsIndex :: ASRange -> ASIndex -> Bool
rangeContainsIndex (Range sid1 (topLeft, bottomRight)) (Index sid2 coord) = and [
  sid1 == sid2, col' >= col1, col' <= col2, row' >= row1, row' <= row2 ]
    where 
      col1  = topLeft^.col
      row1  = topLeft^.row
      col2  = bottomRight^.col
      row2  = bottomRight^.row
      col'  = coord^.col
      row'  = coord^.row

rangeContainsRange :: ASRange -> ASRange -> Bool
rangeContainsRange (Range sid1 (topLeft1,  bottomRight1)) (Range sid2 (topLeft2, bottomRight2)) = and [
  sid1 == sid2, col1 <= col1', col2 >= col2', row1 <= row1', row2 >= row2']
  where
    col1  = topLeft1 ^.col
    col2  = bottomRight1 ^.col
    col1' = topLeft2^.col
    col2' = bottomRight2^.col
    row1  = topLeft1 ^.row
    row2  = bottomRight1 ^.row
    row1' = topLeft2^.row
    row2' = bottomRight2^.row

-- Probably needs case for columns
rangeContainsRef :: ASRange -> ASReference -> Bool
rangeContainsRef r ref = case ref of
  IndexRef i  -> rangeContainsIndex r i
  PointerRef p -> rangeContainsIndex r (pointerIndex p)
  RangeRef r' -> rangeContainsRange r r'
  -- TODO: timchu, should never contain columns?
  ColRangeRef r' -> False
  OutOfBounds -> False

-- #Question, timchu, 12/29/15. Can we use this in exRefToASRef, for consistency?
orientRange :: ASRange -> ASRange
orientRange (Range sid (tl, br)) = Range sid (tl',br')
  where
    tl' = Coord (min (view col tl) (view col br)) (min (view row tl) (view row br))
    br' = Coord (max (view col tl) (view col br)) (max (view row tl) (view row br))

rangeToIndicesRowMajor :: ASRange -> [ASIndex]
rangeToIndicesRowMajor (Range sheet (ul, lr)) = [Index sheet (Coord x y) | y <- [starty..endy],x <- [startx..endx] ]
  where
    startx = min (view col ul) (view col lr)
    endx   = max (view col ul) (view col lr)
    starty = min (view row ul) (view row lr)
    endy   = max (view row ul) (view row lr)

rangeToIndicesRowMajor2D :: ASRange -> [[ASIndex]]
rangeToIndicesRowMajor2D (Range sheet (ul, lr)) = map (\y -> [Index sheet (Coord x y) | x <- [startx..endx]]) [starty..endy]
  where
    startx = min (view col ul) (view col lr)
    endx = max (view col ul) (view col lr)
    starty = min (view row ul) (view row lr)
    endy = max (view row ul) (view row lr)

shiftCoord :: Offset -> Coord -> Maybe Coord
shiftCoord o coord = 
  if (view col coord)+(dCol o) >= 1 && (view row coord) +(dRow o) >= 1 
     then Just $ shiftCoordIgnoreOutOfBounds o coord
     else Nothing

-- TODO: timchu, 1/3/15. shiftCoordIgnoreOutOfBounds should not be used outside this file!
shiftCoordIgnoreOutOfBounds :: Offset -> Coord -> Coord
shiftCoordIgnoreOutOfBounds o = (col %~ (+ (dCol o))) . (row %~ (+ (dRow o)))

shiftInfiniteRowCoord :: Offset -> InfiniteRowCoord -> Maybe InfiniteRowCoord
shiftInfiniteRowCoord o (InfiniteRowCoord col) =
  if col+(dCol o) >= 1
     then Just $ InfiniteRowCoord (col + (dCol o))
     else Nothing

shiftRef :: Offset -> ASReference -> Maybe ASReference
shiftRef o (IndexRef ind) = IndexRef <$> (shiftInd o ind)
shiftRef o (PointerRef (Pointer ind)) =
  PointerRef <$> Pointer <$> (shiftInd o ind)
shiftRef o (RangeRef (Range sh (coord1, coord2))) =
  RangeRef <$> (Range sh) <$> ((,) <$> shiftCoord o coord1 <*> shiftCoord o coord2)
shiftRef o (ColRangeRef (ColRange sh (coord1, col2))) =
  ColRangeRef <$> (ColRange sh) <$> ((,) <$> shiftCoord o coord1 <*> shiftInfiniteRowCoord o col2)

shiftInd :: Offset -> ASIndex -> Maybe ASIndex
shiftInd o (Index sh coord) = (Index sh) <$> shiftCoord o coord

getTopLeft :: ASRange -> ASIndex
getTopLeft (Range sh (tl,_)) = Index sh tl

getRangeDims :: ASRange -> Dimensions
getRangeDims (Range _ (coord1, coord2)) =
  Dimensions { width  = 1 + abs (col2 - col1)
             , height = 1 + abs (row2 - row1) }
  where
    col1 = coord1^.col
    row1 = coord1^.row
    col2 = coord2^.col
    row2 = coord2^.row

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
