{-# LANGUAGE DeriveGeneric #-}

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

data ASIndex = Index {locSheetId :: ASSheetId, index :: Coord} 
  deriving (Show, Read, Eq, Generic, Ord)
data ASPointer = Pointer {pointerSheetId :: ASSheetId, pointerIndex :: Coord} 
  deriving (Show, Read, Eq, Generic, Ord)

-- ALWAYS (Col, Row)
data ASRange = Range {rangeSheetId :: ASSheetId, range :: (Coord, Coord)} 
  deriving (Show, Read, Eq, Generic, Ord)
data ASReference = IndexRef ASIndex | RangeRef ASRange | PointerRef ASPointer | OutOfBounds 
  deriving (Show, Read, Eq, Generic, Ord)

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

instance ToJSON ASPointer where
  toJSON (Pointer sid (c,r)) = object ["tag"     .= ("index" :: String),
                                     "sheetId" .= sid,
                                     "index"   .= object ["row" .= r, 
                                                          "col" .= c]]
instance FromJSON ASPointer where
  parseJSON (Object v) = do
    loc <- v .: "index"
    sid <- v .: "sheetId"
    idx <- (,) <$> loc .: "col" <*> loc .: "row"
    return $ Pointer sid idx
  parseJSON _          = fail "client message JSON attributes missing"


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

instance ToJSON ASReference where
  toJSON (IndexRef idx) = toJSON idx
  toJSON (PointerRef p) = toJSON p
  toJSON (RangeRef rng) = toJSON rng
instance FromJSON ASReference

instance ToJSON Dimensions
instance FromJSON Dimensions

-- deep strict eval instances for R 
instance NFData ASIndex             where rnf = genericRnf
instance NFData ASPointer           where rnf = genericRnf
instance NFData ASRange             where rnf = genericRnf
instance NFData ASReference         where rnf = genericRnf

instance Serialize Dimensions
instance Serialize ASIndex 
instance Serialize ASRange
instance Serialize ASPointer
instance Serialize ASReference

instance Hashable ASIndex

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers


getHeight :: ASReference -> Int
getHeight (IndexRef _) = 1
getHeight (PointerRef _) = 1
getHeight (RangeRef (Range _ ((_,b),(_,d)))) = d-b+1

getWidth :: ASReference -> Int
getWidth (IndexRef _) = 1
getWidth (PointerRef _) = 1
getWidth (RangeRef (Range _ ((a,_),(c,_)))) = c-a+1

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

rangeContainsRect :: ASRange -> Rect -> Bool
rangeContainsRect (Range _ ((x,y),(x2,y2))) ((x',y'),(x2',y2')) = tl && br
  where
    tl = (x' >= x) && (y' >= y)
    br = (x2' <= x2) && (y2' <= y2)

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
  PointerRef p -> rangeContainsIndex r (pointerToIndex p)
  RangeRef r' -> rangeContainsRange r r'
  OutOfBounds -> False 

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
shiftLoc o (PointerRef (Pointer sh (x,y))) = PointerRef $ Pointer sh (x+(dX o), y+(dY o))
shiftLoc o (RangeRef (Range sh ((x,y),(x2,y2)))) = RangeRef $ Range sh ((x+(dX o), y+(dY o)), (x2+(dX o), y2+(dY o)))

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

pointerToIndex :: ASPointer -> ASIndex
pointerToIndex (Pointer s c) = Index s c

indexToPointer :: ASIndex -> ASPointer
indexToPointer (Index s c) = Pointer s c
