{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

-- | This module's primary purpose is to create a type family to facilitate
-- with any kind of shifting functions, such as shifting an ASIndex by some
-- row and col offset.
-- | All Functions that do shifting on types in Types/Locations ,
-- functions such as shiftByOffset, should be located here.
module AS.Types.Shift where

import AS.Types.DataModification
import AS.Types.Locations
import AS.Prelude 

import Control.Monad ((>=>))

-- shift without bounds checking. ONLY USED ONCE in Mutate, in a contrived way.
-- #RoomForImprovement: deprecate.
shiftF :: (Data a, Num a, Data b) => a -> b -> b
shiftF a = deepGMapT (+ a)

-- shifts with bounds checking.
shift' :: (Data a, Num a, Ord a) => a -> a -> Maybe a
shift' shiftBy thingToShift
  | shiftBy + thingToShift > 0 = Just $ shiftBy + thingToShift
  | otherwise = Nothing

-- Base definition of shifting any ADT with a Col in it, by the Col.
-- WILL FAIL SILENTLY IF YOU SHIFT A ROW BY A SHIFTCOL. You should  never do that.
--  NORM: You should not shift a col by a col or a col by a row.
--  This function should be applied directly on indices and ranges and refs.
shiftByCol :: Col -> (Data a => a -> Maybe a)
shiftByCol = deepGMapM  . shift'

shiftByRow :: Row -> (Data a => a -> Maybe a)
shiftByRow = deepGMapM  . shift'

-- This is generally applied to Coords, ExtendedCoords, Ranges, Indices, ...
-- and not to Cols or Rows.
shiftSafe :: (Data a) => Offset -> a -> Maybe a
shiftSafe o = shiftByCol (dCol o) >=> shiftByRow (dRow o)

shiftRef :: Offset -> ASReference -> Maybe ASReference
shiftRef = shiftSafe
