{-# LANGUAGE DeriveGeneric #-}

module AS.Types.CellProps
  ( CellPropType(..), 
    CellProp(..),
    Color, 
    VAlignType(..), 
    HAlignType(..),
    ASCellProps,
    propType,
    getProp, 
    hasPropType, 
    hasProp, 
    isEmpty, 
    setProp, 
    setCondFormatProp, 
    clearCondFormatProps, 
    removeProp, 
    emptyProps,
    FormatType(..), 
    Formatted(..), 
    Bloomberg(..), 
    StreamSource(..), 
    Stream(..)
  ) where

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

-- #expert is there a better way to structure these? 

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


data ASCellProps = ASCellProps { underlyingProps :: M.Map CellPropType CellProp , condFormatProps :: M.Map CellPropType CellProp } deriving (Show, Read, Generic)
instance Eq ASCellProps where
  (==) (ASCellProps m1 cm1) (ASCellProps m2 cm2) = (m1 == m2) && (cm1 == cm2)

-- props from conditional formatting take precedence over the default underlying properties
getProp :: CellPropType -> ASCellProps -> Maybe CellProp
getProp pt (ASCellProps m cm) = listToMaybe $ catMaybes $ [M.lookup pt cm, M.lookup pt m]

hasPropType :: CellPropType -> ASCellProps -> Bool
hasPropType pt p = isJust $ getProp pt p

hasProp :: CellProp -> ASCellProps -> Bool
hasProp p ps = (getProp (propType p) ps) == Just p

isEmpty :: ASCellProps -> Bool
isEmpty = liftA2 (&&) (null . underlyingProps) (null . condFormatProps)

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

-- Frontend should actually *really* pass CellProps to backend. It currently does because frontend sends backend
-- a cell to do eval, which has an ASCellProps field. It is always blank though and always should be blank. Currently
-- defining this as a hack just to get it to compile -- the correcdt solution is to make this an error and have 
-- frontend stop sending ASCell's. 
instance FromJSON ASCellProps where
  parseJSON v = return emptyProps
instance ToJSON ASCellProps where
  toJSON (ASCellProps m cm) = toJSON $ M.elems $ M.union cm m
instance Serialize ASCellProps

instance FromJSON CellProp
instance ToJSON CellProp
instance Serialize CellProp

instance FromJSON VAlignType
instance ToJSON VAlignType
instance Serialize VAlignType

instance FromJSON HAlignType
instance ToJSON HAlignType
instance Serialize HAlignType

instance ToJSON FormatType
instance FromJSON FormatType

instance ToJSON Bloomberg
instance FromJSON Bloomberg
instance Serialize Bloomberg

instance ToJSON Stream
instance FromJSON Stream
instance Serialize Stream

instance ToJSON StreamSource
instance FromJSON StreamSource
instance Serialize StreamSource

instance Serialize CellPropType
instance Serialize FormatType
