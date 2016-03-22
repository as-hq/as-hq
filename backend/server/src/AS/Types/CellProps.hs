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
    setProps,
    setCondFormatProp,
    setCondFormatProps, 
    clearCondFormatProps, 
    removeProp, 
    emptyProps,
    upsertProp,
    filterProps,
    Bloomberg(..), 
    StreamSource(..), 
    Stream(..)
  ) where

import AS.Prelude

import AS.Types.User
import AS.Types.Formats
import AS.ASJSON

import GHC.Generics
import Data.Aeson
import Data.SafeCopy

import Control.Applicative (liftA2)

import Data.Aeson.Types (Parser)
import Data.Maybe
import Data.List (foldl')
import qualified Data.Map as M

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
  deriving (Show, Read, Eq, Generic, Ord, Data)

data CellProp =
    TextColor Color
  | FillColor Color
  | VAlign VAlignType
  | HAlign HAlignType
  | FontSize Int
  | FontName String
  | ValueFormat {valFormat :: Format}
  | StreamInfo Stream
  | ImageData {imageWidth :: Int, imageHeight :: Int, dragOffsetLeft :: Int, dragOffsetTop :: Int}
  | ReadOnly [ASUserId]
  | URL { urlLink :: String }
  | Bold | Italic | Underline
  | Volatile
  | Tracking
  deriving (Show, Read, Eq, Data, Typeable, Generic)

type Color = String -- represents hex color
data HAlignType = LeftAlign | HCenterAlign | RightAlign deriving (Show, Read, Eq, Data, Typeable, Generic)
data VAlignType = TopAlign | VCenterAlign | BottomAlign deriving (Show, Read, Eq, Data, Typeable, Generic)

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

-- #needsrefactor pointless pointfree
setProps :: [CellProp] -> ASCellProps -> ASCellProps
setProps = foldl' (.) id . map setProp

setCondFormatProp :: CellProp -> ASCellProps -> ASCellProps
setCondFormatProp cp (ASCellProps m cm) = ASCellProps m (M.insert (propType cp) cp cm)

-- #needsrefactor pointless pointfree
setCondFormatProps :: [CellProp] -> ASCellProps -> ASCellProps
setCondFormatProps = foldl' (.) id . map setCondFormatProp

clearCondFormatProps :: ASCellProps -> ASCellProps
clearCondFormatProps (ASCellProps m _) = ASCellProps m M.empty

removeProp :: CellPropType -> ASCellProps -> ASCellProps
removeProp pt (ASCellProps m cm) = ASCellProps (M.delete pt m) cm

emptyProps :: ASCellProps
emptyProps = ASCellProps M.empty M.empty

-- Given a default prop, and a prop transform, insert the default prop if its corresponding propType doesn't exist. 
-- Otherwise, map the prop transform over the ASCellProps. 
upsertProp :: CellProp -> (CellProp -> CellProp) -> (ASCellProps -> ASCellProps)
upsertProp defaultProp transformProp cp@(ASCellProps m cm) = if (hasPropType (propType defaultProp) cp)
  then ASCellProps (M.map transformProp m) cm
  else setProp defaultProp cp

-- In the delete handlers, you may want to removeFormats from some cells and not others, this is a helper function for that. 
-- It filters over one of the maps in ASCellProps
filterProps :: (CellProp -> Bool) -> ASCellProps -> ASCellProps
filterProps f (ASCellProps up cp) = ASCellProps (M.filter f up) cp

----------------------------------------------------------------------------------------------------------------------------------------------
-- Streaming

-- Stream sources
data Bloomberg = Bloomberg {url :: String, bmbKey :: String} deriving (Show, Read, Eq, Data, Typeable, Generic)
data StreamSource = StreamB Bloomberg | NoSource deriving (Show, Read, Eq, Data, Typeable, Generic)
data Stream = Stream {streamSource :: StreamSource, streamFreq :: Int} deriving (Show, Read, Eq, Data, Typeable, Generic)
-- A stream just needs a source and a frequency

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

deriveSafeCopy 1 'base ''CellPropType
deriveSafeCopy 1 'base ''CellProp
deriveSafeCopy 1 'base ''VAlignType
deriveSafeCopy 1 'base ''Stream
deriveSafeCopy 1 'base ''StreamSource
deriveSafeCopy 1 'base ''HAlignType
deriveSafeCopy 1 'base ''Bloomberg
deriveSafeCopy 1 'base ''ASCellProps


-- Frontend should actually *really* pass CellProps to backend. It currently does because frontend sends backend
-- a cell to do eval, which has an ASCellProps field. It is always blank though and always should be blank. Currently
-- defining this as a hack just to get it to compile -- the correcdt solution is to make this an error and have 
-- frontend stop sending ASCell's. 
instance ToJSON ASCellProps where
  toJSON (ASCellProps m cm) = toJSON $ M.elems $ M.union cm m


asToFromJSON ''CellProp
asToFromJSON ''CellPropType
asToFromJSON ''VAlignType
asToFromJSON ''HAlignType
asToFromJSON ''Bloomberg
asToFromJSON ''Stream
asToFromJSON ''StreamSource
