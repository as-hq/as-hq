{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Locations
  ( module AS.Types.Locations
  , module AS.Types.Sheets
  ) where

import AS.Types.Sheets

import GHC.Generics
import Data.Aeson

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)


type Col = Int
type Row = Int
type Coord = (Col, Row)
type Dimensions = (Int, Int)
type Offset = (Int, Int)
type Rect = (Coord, Coord)

col :: Coord -> Col
col = fst

row :: Coord -> Row
row = snd

----------------------------------------------------------------------------------------------------------------------------------------------
-- Locations

data ASIndex = 
    Index {locSheetId :: ASSheetId, index :: Coord} 
  | Pointer {pointerSheetId :: ASSheetId, pointerIndex :: Coord} 
  deriving (Show, Read, Eq, Generic, Ord)
data ASRange = Range {rangeSheetId :: ASSheetId, range :: (Coord, Coord)} deriving (Show, Read, Eq, Generic, Ord)
data ASReference = IndexRef ASIndex | RangeRef ASRange | OutOfBounds deriving (Show, Read, Eq, Generic, Ord)

data ASWindow = Window {windowSheetId :: ASSheetId, topLeft :: Coord, bottomRight :: Coord} deriving (Show,Read,Eq,Generic)

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
  toJSON (RangeRef rng) = toJSON rng
instance FromJSON ASReference

instance ToJSON ASWindow where
  toJSON (Window sid (c,r) (c2, r2)) = object ["tag" .= ("window" :: String),
                                               "sheetId" .= sid,
                                               "range" .= object [ 
                                                  "tl" .= object [ "row"  .= r, 
                                                                   "col"  .= c],
                                                  "br" .= object [ "row"  .= r2, 
                                                                   "col"  .= c2]]]
instance FromJSON ASWindow where
  parseJSON (Object v) = do
    rng <- v .: "window" 
    (tl, br) <- (,) <$> rng .: "tl" <*> rng .: "br"
    tl' <- (,) <$> tl .: "col" <*> tl .: "row"
    br' <- (,) <$> br .: "col" <*> br .: "row"
    sid <- v .: "sheetId"
    return $ Window sid tl' br'
  parseJSON _          = fail "client message JSON attributes missing"

-- memory region exposure instances for R value unboxing
instance NFData ASIndex             where rnf = genericRnf
instance NFData ASRange             where rnf = genericRnf
instance NFData ASReference         where rnf = genericRnf