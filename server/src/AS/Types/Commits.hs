{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Commits where

import AS.Types.Cell
import AS.Types.Eval

import Prelude
import GHC.Generics
import Data.Aeson hiding (Success)
import Data.List

----------------------------------------------------------------------------------------------------------------------------------------------
-- Version Control

data ASTime = Time {day :: String, hour :: Int, minute :: Int, sec :: Int} deriving (Show, Read, Eq, Generic)

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

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers

mergeCommits :: ASCommit -> ASCommit -> ASCommit
mergeCommits (Commit b a bd ad _) (Commit b' a' bd' ad' t) = Commit b'' a'' bd'' ad'' t
  where
    b'' = mergeCells b' b
    a'' = mergeCells a' a
    bd'' = unionBy hasSameKey bd' bd
    ad'' = unionBy hasSameKey ad' ad
    hasSameKey d1 d2 = (descriptorKey d1) == (descriptorKey d2)

getASTime :: IO ASTime
getASTime = return $ Time "hi" 1 2 3