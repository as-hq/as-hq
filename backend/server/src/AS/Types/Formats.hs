module AS.Types.Formats
  ( Format(..),
    FormatType(..), 
    Formatted(..), 
    formatType, numDecimals, 
    orig, format,
    shiftDecPrecision
  ) where

import AS.Prelude
import Prelude()

import AS.Types.Values
import AS.ASJSON

import GHC.Generics
import Data.Aeson
import Data.SafeCopy
import Data.Maybe
import Data.List.Split (splitOn)

import Control.Applicative
import Control.Monad (liftM, ap)
import Control.Lens
import Control.Lens.Prism

----------------------------------------------------------------------------------------------------------------------------------------------
-- Formats

data FormatType = NoFormat | Money | Percentage | Date deriving (Show, Read, Eq, Data, Typeable, Generic)

-- A format must have a FormatType, and possibly keeps track of num decimal offset (if applicable, not so for Date)
-- An offset of +1 means that there's one more decimal place; 1.22 -> 1.220
-- note: I'd prefer to not expose this, but pattern-matching this just makes life *so* much easier
data Format = Format { _formatType :: FormatType, _numDecimals :: Maybe Int } deriving (Show, Read, Data, Typeable, Eq, Generic)

-- The Formatted monad possibly gives formatting to an original value
-- note: I'd prefer to not expose this, but pattern-matching this just makes life *so* much easier
data Formatted a = Formatted { _orig :: a, _format :: Maybe Format }

instance (Eq a) => Eq (Formatted a) where
  (==) (Formatted x _) (Formatted y _)  = x == y

makeLenses ''Format
makeLenses ''Formatted

instance Functor Formatted where
  fmap = liftM

instance Applicative Formatted where
  pure  = return
  (<*>) = ap

-- Always retain the format of the first argument, unless there was none
instance Monad Formatted where
  return x                   = Formatted x Nothing
  Formatted x Nothing >>= f = f x
  Formatted x (Just y) >>= f = f x & format .~ Just y

shiftDecPrecision :: Int -> Formatted ASValue -> Formatted ASValue 
shiftDecPrecision shft fv = 
  case fv^.orig of 
    ValueI i -> initFormat fv & format._Just.numDecimals %~ (Just . boundedShift shft . fromMaybe 0)
    ValueD d -> initFormat fv & format._Just.numDecimals %~ (Just . boundedShift shft . fromMaybe (decimalPlaces d))
    _        -> fv
  where
    initFormat    = format %~ Just . fromMaybe (Format NoFormat Nothing)
    boundedShift  = ((max 0 . min 20) .) . (+)
    decimalPlaces = min 10 . length . $head . splitOn "e" . $last . splitOn "." . show
    -- ^ number of digits after the decimal. Assumes all doubles have have a decimal when called with
    -- show. For doubles shown in scientific notation, this counts the number of digits shown before the e.

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

deriveSafeCopy 1 'base ''FormatType
deriveSafeCopy 1 'base ''Format

asLensedToFromJSON ''FormatType
asLensedToFromJSON ''Format