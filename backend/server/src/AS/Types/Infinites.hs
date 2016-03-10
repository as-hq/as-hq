{-# LANGUAGE DeriveGeneric, TemplateHaskell, DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AS.Types.Infinites where

import GHC.Generics

import AS.Prelude hiding (isInfinite)
import Prelude hiding (isInfinite)
import Control.Lens hiding ((.=))

-- | Creates an Infinite data type from any existing data type.
-- Used to create Infinite Col and Infinite Row data types, which are used as
-- coordinates for the bottom right corner of an ASRange: ASRanges can be infinite
-- in either the Column or Row direction, or both.

data Infinite a = Infinite | Finite a deriving (Show, Read, Eq, Generic, Data, Typeable, Functor)

-- Infinite numbers are always larger than finite ones.
-- This is useful because  min and max are often taken for Infinite numbers.
instance (Ord a) => Ord (Infinite a) where
  Infinite >= Finite _ = True
  Finite _ >= Infinite = False
  Finite i >= Finite j = i >= j
  Infinite <= Finite _ = False
  Finite _ <= Infinite = True
  Finite i <= Finite j = i <= j

-- Adding infinite numbers works the way you expect.
instance (Num a) => Num (Infinite a) where
  (+) Infinite (Finite _) = Infinite
  (+) (Finite i) (Finite j) = Finite (i+j)
  (-) Infinite (Finite _)   = Infinite
  (-) (Finite _) Infinite = Infinite -- This should never be called, and is not correct.
  (-) (Finite i) (Finite j) = Finite (i-j)
  (*) Infinite (Finite _)   = Infinite
  (*) (Finite _) Infinite = Infinite
  abs (Finite i) = Finite (abs i)
  abs Infinite = Infinite
  signum (Finite i) = Finite (signum i)
  signum Infinite = Finite 1
  fromInteger a = Finite (fromInteger a)

-- | Helper methods for checking whether a value is infinite, converting between
-- infinite and finite values.
isInfinite :: Infinite a -> Bool
isInfinite infiniteA =
  case infiniteA of
       Infinite -> True
       _ -> False

isFinite :: Infinite a -> Bool
isFinite = not . isInfinite

-- If Finite, get the number back. Otherwise, error.
fromInfinite :: Infinite a -> a
fromInfinite (Finite a) = a

toInfinite :: a -> Infinite a
toInfinite a = Finite a

-- Prism used to look into Infinite a.
-- Returns Nothing if the argument is Infinite, Just a if the argument is F a.
_Finite :: Prism' (Infinite a) a
_Finite = prism' Finite (\infiniteA -> case infiniteA of Finite i -> Just i; Infinite -> Nothing)
