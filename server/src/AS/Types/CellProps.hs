{-# LANGUAGE DeriveGeneric #-}

module AS.Types.CellProps where

import AS.Types.User
import AS.Types.Common

import GHC.Generics
import Data.Aeson

import Data.Serialize (Serialize)
import Data.Aeson.Types (Parser)
import Data.Maybe
import qualified Data.Map as M
import Control.Applicative
import Control.Monad (liftM, ap)

data CellPropType =
    TextColorProp
  | FillColorProp
  | VAlignProp
  | HAlignProp
  | FontSizeProp
  | FontNameProp
  | ValueFormatProp
  | ImageDataProp
  | StreamInfoProp
  | ReadOnlyProp
  | URLProp
  | BoldProp | ItalicProp | UnderlineProp
  | VolatileProp
  | TrackingProp
  deriving (Show, Read, Eq, Generic, Ord)

data CellProp =
    TextColor Color
  | FillColor Color
  | VAlign VAlignType
  | HAlign HAlignType
  | FontSize Int
  | FontName String
  | ValueFormat {formatType :: FormatType}
  | StreamInfo Stream
  | ImageData {imageWidth :: Int, imageHeight :: Int, imageOffsetX :: Int, imageOffsetY :: Int}
  | ReadOnly [ASUserId]
  | URL { urlLink :: String }
  | Bold | Italic | Underline
  | Volatile
  | Tracking
  deriving (Show, Read, Eq, Generic)

type Color = String
data HAlignType = LeftAlign | HCenterAlign | RightAlign deriving (Show, Read, Eq, Generic)
data VAlignType = TopAlign | VCenterAlign | BottomAlign deriving (Show, Read, Eq, Generic)

data ASCellProps = ASCellProps { underlyingProps :: M.Map CellPropType CellProp , condFormatProps :: M.Map CellPropType CellProp } deriving (Show, Read, Generic)
instance Eq ASCellProps where
  (==) (ASCellProps m1 cm1) (ASCellProps m2 cm2) = (m1 == m2) && (cm1 == cm2)

getProp :: CellPropType -> ASCellProps -> Maybe CellProp
getProp pt (ASCellProps m cm) = listToMaybe $ catMaybes $ [M.lookup pt cm, M.lookup pt m]

hasPropType :: CellPropType -> ASCellProps -> Bool
hasPropType pt p = isJust $ getProp pt p

hasProp :: CellProp -> ASCellProps -> Bool
hasProp p ps = (getProp (propType p) ps) == Just p

propType :: CellProp -> CellPropType
propType (TextColor _) = TextColorProp
propType (FillColor _) = FillColorProp
propType (VAlign _) = VAlignProp
propType (HAlign _) = HAlignProp
propType (FontSize _) = FontSizeProp
propType (FontName _) = FontNameProp
propType (ValueFormat _) = ValueFormatProp
propType (StreamInfo _) = StreamInfoProp
propType (ImageData _ _ _ _) = ImageDataProp
propType (ReadOnly _) = ReadOnlyProp
propType (URL _) = URLProp
propType Bold = BoldProp
propType Italic = ItalicProp
propType Underline = UnderlineProp
propType Volatile = VolatileProp
propType Tracking = TrackingProp

setProp :: CellProp -> ASCellProps -> ASCellProps
setProp cp (ASCellProps m cm) = ASCellProps (M.insert (propType cp) cp m) cm

setCondFormatProp :: CellProp -> ASCellProps -> ASCellProps
setCondFormatProp cp (ASCellProps m cm) = ASCellProps m (M.insert (propType cp) cp cm)

clearCondFormatProps :: ASCellProps -> ASCellProps
clearCondFormatProps (ASCellProps m _) = ASCellProps m M.empty

removeProp :: CellPropType -> ASCellProps -> ASCellProps
removeProp pt (ASCellProps m cm) = ASCellProps (M.delete pt m) cm

emptyProps :: ASCellProps
emptyProps = ASCellProps M.empty M.empty

----------------------------------------------------------------------------------------------------------------------------------------------
-- Formats

data FormatType = NoFormat | Money | Percentage | Date deriving (Show, Read, Eq, Generic)
data Formatted a = Formatted { orig :: a, format :: Maybe FormatType }

instance Functor Formatted where
  fmap = liftM

instance Applicative Formatted where
  pure  = return
  (<*>) = ap

-- Always retain the format of the first argument, unless there was none
instance Monad Formatted where
  return x                   = Formatted x Nothing
  Formatted x Nothing >>= f  = f x
  Formatted x y >>= f        = (f x) { format = y }

instance (Eq a) => Eq (Formatted a) where
  (==) (Formatted x _) (Formatted y _)  = x==y

----------------------------------------------------------------------------------------------------------------------------------------------
-- Streaming

-- Stream sources
data Bloomberg = Bloomberg {url :: String, bmbKey :: String} deriving (Show, Read, Eq, Generic)
data StreamSource = StreamB Bloomberg | NoSource deriving (Show, Read, Eq, Generic)
data Stream = Stream {streamSource :: StreamSource, streamFreq :: Int} deriving (Show, Read, Eq, Generic)
-- A stream just needs a source and a frequency

-- Kind of a hack. The frontend only ever sends backend empty props -- it should be refactored to not
-- send back a cell at all, and ASCellProps should not actually be FromJSON.
instance FromJSON ASCellProps where
  parseJSON v = return emptyProps

instance FromJSON CellProp
instance ToJSON CellProp

instance ToJSON ASCellProps where
  toJSON (ASCellProps m cm) = toJSON $ M.elems $ M.union cm m

instance FromJSON VAlignType
instance ToJSON VAlignType

instance FromJSON HAlignType
instance ToJSON HAlignType

instance ToJSON FormatType
instance FromJSON FormatType

instance ToJSON Bloomberg
instance FromJSON Bloomberg

instance ToJSON Stream
instance FromJSON Stream

instance ToJSON StreamSource
instance FromJSON StreamSource

instance Serialize ASCellProps
instance Serialize CellPropType
instance Serialize CellProp
instance Serialize VAlignType
instance Serialize HAlignType
instance Serialize Stream
instance Serialize StreamSource
instance Serialize Bloomberg
instance Serialize FormatType
