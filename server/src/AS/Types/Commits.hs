{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Commits where

import AS.Types.Cell

import Prelude
import GHC.Generics
import Data.Aeson hiding (Success)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Version Control

data ASTime = Time {day :: String, hour :: Int, minute :: Int, sec :: Int} deriving (Show, Read, Eq, Generic)

type ASRelation = (ASIndex, [ASIndex]) -- for representing ancestry relationships

data ASCommit = Commit {before :: [ASCell],
                        after :: [ASCell],
                        beforeDescriptors :: [RangeDescriptor],
                        afterDescriptors :: [RangeDescriptor],
                        time :: ASTime}
                        deriving (Show, Read, Generic)

-- Should refactor to not be an ordered pair
type CommitSource = (ASSheetId, ASUserId)

instance FromJSON ASTime
instance ToJSON ASTime
instance FromJSON ASCommit
instance ToJSON ASCommit