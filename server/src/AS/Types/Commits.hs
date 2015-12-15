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

-- NORM: never expand this type; always modify it using the records.
data CellDiff = CellDiff { beforeCells :: [ASCell]
                         , afterCells :: [ASCell] } 
                         deriving (Show, Read, Generic)

data ASCommit = Commit { cellDiff :: CellDiff
                       , commitDescriptorDiff :: DescriptorDiff
                       , time :: ASTime }
                       deriving (Show, Read, Generic)

-- Should refactor to not be an ordered pair
type CommitSource = (ASSheetId, ASUserId)

instance FromJSON ASTime
instance ToJSON ASTime
instance FromJSON ASCommit
instance ToJSON ASCommit
instance FromJSON CellDiff
instance ToJSON CellDiff


----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers

mergeCommits :: ASCommit -> ASCommit -> ASCommit
mergeCommits (Commit cdiff' ddiff' t) (Commit cdiff ddiff _) = Commit cdiff'' ddiff'' t
  where
    cdiff'' = CellDiff { beforeCells = mergeCells (beforeCells cdiff') (beforeCells cdiff)
                       , afterCells = mergeCells (afterCells cdiff') (afterCells cdiff) }
    ddiff'' = DescriptorDiff { addedDescriptors = unionBy hasSameKey (addedDescriptors ddiff') (addedDescriptors ddiff)
                             , removedDescriptors = unionBy hasSameKey (removedDescriptors ddiff') (removedDescriptors ddiff) }
    hasSameKey d1 d2 = (descriptorKey d1) == (descriptorKey d2)

getASTime :: IO ASTime
getASTime = return $ Time "hi" 1 2 3

generateCommitFromCells :: [ASCell] -> IO ASCommit
generateCommitFromCells cells = do 
  time <- getASTime
  let cdiff = CellDiff { beforeCells = [], afterCells = cells }
      ddiff = DescriptorDiff { addedDescriptors = [], removedDescriptors = [] }
  return $ Commit cdiff ddiff time