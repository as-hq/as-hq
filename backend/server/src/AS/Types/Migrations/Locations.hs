{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module AS.Types.Migrations.Locations where

import GHC.Generics
import Data.SafeCopy

-- 1/22/16 migration of Coord.

-- assuming the relevant new types are (can leave these commented):
--newtype Col = Col { _colInt :: Int } deriving (Show, Read, Eq, Ord, Generic, Num)
--newtype Row = Row { _rowInt :: Int } deriving (Show, Read, Eq, Ord, Generic, Num)
-- data Coord = Coord { _coordCol :: Col, _coordRow :: Row } deriving (Show, Read, Eq, Ord, Generic)

-- the migration will be (uncomment this when migration time):
--data Coord0 = Coord0 { _coordCol0 :: Int, _coordRow0 :: Int } deriving (Show, Read, Eq, Ord, Generic)
--deriveSafeCopy 1 'base ''Coord0
--deriveSafeCopy 2 'extension ''Coord
--instance Migrate Coord where
--  type MigrateFrom Coord = Coord0
--  migrate (Coord0 c r) = Coord (Col c) (Row r)

