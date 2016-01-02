{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module AS.Types.Locations
  ( module AS.Types.Locations
  , module AS.Types.Sheets
  ) where

import AS.Types.Sheets
import AS.Types.Common

import GHC.Generics
import Data.Aeson
import Data.Hashable

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)

import Data.Serialize (Serialize)

type Col = Int
type Row = Int
type Coord = (Col, Row)
data Dimensions = Dimensions {width :: Int, height :: Int} deriving (Show, Read, Eq, Generic)
data Offset = Offset { dX :: Int, dY :: Int }
type Rect = (Coord, Coord)

col :: Coord -> Col
col = fst

row :: Coord -> Row
row = snd

----------------------------------------------------------------------------------------------------------------------------------------------
-- Locations

data ASIndex = Index { locSheetId :: ASSheetId, index :: Coord } 
  deriving (Show, Read, Eq, Generic, Ord)
data ASPointer = Pointer { pointerIndex :: ASIndex } 
  deriving (Show, Read, Eq, Generic, Ord)
data ASRange = Range {rangeSheetId :: ASSheetId, range :: (Coord, Coord)} -- ALWAYS (Col, Row)
  deriving (Show, Read, Eq, Generic, Ord)
data ASColRange = ColRange {colRangeSheetId :: ASSheetId, colRange :: (Coord, Col)} -- A4:B = ((1,4),2)
  deriving (Show, Read, Eq, Generic, Ord)
data ASReference = IndexRef ASIndex | ColRangeRef ASColRange | RangeRef ASRange | PointerRef ASPointer | OutOfBounds
  deriving (Show, Read, Eq, Generic, Ord)

refSheetId :: ASReference -> ASSheetId
refSheetId (IndexRef   i) = locSheetId     i
refSheetId (RangeRef   r) = rangeSheetId   r
refSheetId (ColRangeRef   r) = colRangeSheetId   r
refSheetId (PointerRef p) = locSheetId . pointerIndex $ p

instance ToJSON ASIndex where
  toJSON (Index sid (c,r)) = object ["tag"     .= ("index" :: String),
                                     "sheetId" .= sid,
                                     "index"   .= object ["row" .= r, 
                                                          "col" .= c]]
instance FromJSON ASIndex where
  parseJSON (Object v) = do
    loc <- v .: "index"
    sid <- v .: "sheetId"
    idx <- (,) <$> loc .: "col" <*> loc .: "row"
    return $ Index sid idx
  parseJSON _          = fail "client message JSON attributes missing"
instance Serialize ASIndex

instance ToJSON ASPointer where
  toJSON (Pointer (Index sid (c,r))) = object ["tag"     .= ("index" :: String),
                                       "sheetId" .= sid,
                                       "index"   .= object ["row" .= r, 
                                                            "col" .= c]]
instance FromJSON ASPointer where
  parseJSON (Object v) = do
    loc <- v .: "index"
    sid <- v .: "sheetId"
    idx <- (,) <$> loc .: "col" <*> loc .: "row"
    return $ Pointer (Index sid idx)
  parseJSON _          = fail "client message JSON attributes missing"
instance Serialize ASPointer

instance ToJSON ASRange where
  toJSON (Range sid ((c,r),(c2,r2))) = object ["tag" .= ("range" :: String),
                                               "sheetId" .= sid,
                                               "range" .= object [ 
                                                  "tl" .= object [ "row"  .= r, 
                                                                   "col"  .= c],
                                                  "br" .= object [ "row"  .= r2, 
                                                                   "col"  .= c2]]]
instance FromJSON ASRange where
  parseJSON (Object v) = do
    rng <- v .: "range" 
    (tl, br) <- (,) <$> rng .: "tl" <*> rng .: "br"
    tl' <- (,) <$> tl .: "col" <*> tl .: "row"
    br' <- (,) <$> br .: "col" <*> br .: "row"
    sid <- v .: "sheetId"
    return $ Range sid (tl', br')
  parseJSON _          = fail "client message JSON attributes missing"
instance Serialize ASRange 

instance ToJSON ASReference where
  toJSON (IndexRef idx) = toJSON idx
  toJSON (PointerRef p) = toJSON p
  toJSON (RangeRef rng) = toJSON rng
  toJSON (ColRangeRef colrng) = toJSON colrng
instance FromJSON ASReference
instance Serialize ASReference

--TODO: timchu: not sure if r .= object ["col" .=r2 ] is right. Maybe no list brackets?
                            --Note: r stands for right, not row.
instance ToJSON ASColRange where
  toJSON (ColRange sid ((c,r),c2)) = object["tag" .= ("colRange" :: String),
                                            "sheetId" .= sid,
                                            "colRange" .= object[
                                              "tl" .= object ["row" .=r,
                                                              "col" .= c],
                                              "r"  .= object ["col" .= c2]]]
instance Serialize ASColRange 
-- TODO: timchu, check that this actually works.
instance FromJSON ASColRange where
  parseJSON (Object v) = do
    colrng <- v .: "colRange"
    (tl, r) <- (,) <$>  colrng .: "tl" <*> colrng .: "r"
    tl' <- (,) <$> tl .: "col" <*> tl .: "row"
    r' <- r .: "col"
    sid <- v .: "sheetId"
    return $ ColRange sid (tl', r')
  parseJSON _ = fail "client message JSON attributes missing"

instance ToJSON Dimensions
instance FromJSON Dimensions
instance Serialize Dimensions

-- deep strict eval instances for R 
instance NFData ASIndex             where rnf = genericRnf
instance NFData ASPointer           where rnf = genericRnf
instance NFData ASRange             where rnf = genericRnf
instance NFData ASColRange          where rnf = genericRnf
instance NFData ASReference         where rnf = genericRnf

instance Hashable ASIndex

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers


-- TODO: timchu, refactor getHeight to return a maybeInt.
getHeight :: ASReference -> Int
getHeight (IndexRef _) = 1
getHeight (PointerRef _) = 1
getHeight (RangeRef (Range _ ((_,b),(_,d)))) = d-b+1

-- TODO: refactor getWidth to return a maybeInt, or an Either Int.
getWidth :: ASReference -> Int
getWidth (IndexRef _) = 1
getWidth (PointerRef _) = 1
getWidth (RangeRef (Range _ ((a,_),(c,_)))) = c-a+1
getWidth (ColRangeRef (ColRange _ ((a,_),c))) = c-a+1

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

rangeContainsRect :: ASRange -> Rect -> Bool
rangeContainsRect (Range _ ((x,y),(x2,y2))) ((x',y'),(x2',y2')) = tl && br
  where
    tl = (x' >= x) && (y' >= y)
    br = (x2' <= x2) && (y2' <= y2)

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
rangeToIndices (Range sheet (ul, lr)) = [Index sheet (x,y) | y <- [starty..endy], x <- [startx..endx] ]
  where
    startx = min (col ul) (col lr)
    endx = max (col ul) (col lr)
    starty = min (row ul) (row lr)
    endy = max (row ul) (row lr)

rangeContainsIndex :: ASRange -> ASIndex -> Bool
rangeContainsIndex (Range sid1 ((x1,y1),(x2,y2))) idx = and [
  sid1 == sid2, x >= x1, x <= x2, y >= y1, y <= y2 ]
    where
      (x,y,sid2) = case idx of
        Index sid2 (x,y) -> (x,y,sid2)

rangeContainsRange :: ASRange -> ASRange -> Bool
rangeContainsRange (Range sid1 ((x1, y1), (x2, y2))) (Range sid2 ((x1', y1'), (x2', y2'))) = and [
  sid1 == sid2, x1 <= x1', x2 >= x2', y1 <= y1', y2 >= y2']

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
    tl' = (min (col tl) (col br), min (row tl) (row br))
    br' = (max (col tl) (col br), max (row tl) (row br))

rangeToIndicesRowMajor :: ASRange -> [ASIndex]
rangeToIndicesRowMajor (Range sheet (ul, lr)) = [Index sheet (x,y) | y <- [starty..endy],x <- [startx..endx] ]
  where
    startx = min (col ul) (col lr)
    endx   = max (col ul) (col lr)
    starty = min (row ul) (row lr)
    endy   = max (row ul) (row lr)

rangeToIndicesRowMajor2D :: ASRange -> [[ASIndex]]
rangeToIndicesRowMajor2D (Range sheet (ul, lr)) = map (\y -> [Index sheet (x,y) | x <- [startx..endx]]) [starty..endy]
  where
    startx = min (col ul) (col lr)
    endx = max (col ul) (col lr)
    starty = min (row ul) (row lr)
    endy = max (row ul) (row lr)

shiftLoc :: Offset -> ASReference -> ASReference
shiftLoc o (IndexRef (Index sh (x,y))) = IndexRef $ Index sh (x+(dX o), y+(dY o))
shiftLoc o (PointerRef (Pointer (Index sh (x,y)))) = PointerRef $ Pointer $ Index sh (x + (dX o), y + (dY o))
shiftLoc o (RangeRef (Range sh ((x,y),(x2,y2)))) = RangeRef $ Range sh ((x+(dX o), y+(dY o)), (x2+(dX o), y2+(dY o)))
shiftLoc o (ColRangeRef (ColRange sh ((x,y),x2))) = ColRangeRef $ ColRange sh ((x+(dX o), y+(dY o)), x2+(dX o))

shiftInd :: Offset -> ASIndex -> Maybe ASIndex
shiftInd o (Index sh (x,y)) = if x+(dX o) >= 1 && y+(dY o) >= 1 
  then Just $ Index sh (x+(dX o), y+(dY o))
  else Nothing

getTopLeft :: ASRange -> ASIndex
getTopLeft (Range sh (tl,_)) = Index sh tl

getRangeDims :: ASRange -> Dimensions
getRangeDims (Range _ ((x1, y1), (x2, y2))) = Dimensions { width = 1 + abs (x2 - x1)
                                                         , height = 1 + abs (y2 - y1) }

-- | The offset needed to translate the top left corner of the first range to get the 
-- top left corner of the second. 
getRangeOffset :: ASRange -> ASRange -> Offset
getRangeOffset r1 r2 = getIndicesOffset (getTopLeft r1) (getTopLeft r2)

getIndicesOffset :: ASIndex -> ASIndex -> Offset
getIndicesOffset (Index _ (x, y)) (Index _ (x', y')) = Offset { dX = x'-x, dY = y'-y }

pointerAtIndex :: ASIndex -> ASPointer
pointerAtIndex = Pointer
