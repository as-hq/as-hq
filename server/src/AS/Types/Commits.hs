{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Commits where

import AS.Types.Cell
import AS.Types.CondFormat
import AS.Types.Updates
import AS.Types.Bar
import AS.Types.RangeDescriptor

import GHC.Generics
import Data.Aeson hiding (Success)
import Data.List
import qualified Data.Text as T

import Data.Serialize (Serialize)


----------------------------------------------------------------------------------------------------------------------------------------------
-- Version Control

data ASTime = Time {day :: String, hour :: Int, minute :: Int, sec :: Int} deriving (Show, Read, Eq, Generic)

data ASCommit = Commit { cellDiff :: CellDiff
                       , barDiff :: BarDiff
                       , rangeDescriptorDiff :: DescriptorDiff
                       , condFormatRulesDiff :: CondFormatRuleDiff
                       , time :: ASTime }
                       deriving (Show, Read, Generic)

data CommitSource = CommitSource { srcSheetId :: ASSheetId, srcUserId :: ASUserId }

type UpdateTransform = SheetUpdate -> SheetUpdate

flipCommit :: ASCommit -> ASCommit
flipCommit (Commit cd bd rdd cfrd time) = Commit cd' bd' rdd' cfrd' time 
  where 
    cd'   = flipDiff cd 
    bd'   = flipDiff bd
    rdd'  = flipDiff rdd 
    cfrd' = flipDiff cfrd 

-- Represents a set of collections of a sheet. 
data SheetUpdate = SheetUpdate { cellUpdates :: CellUpdate
                               , barUpdates :: BarUpdate
                               , descriptorUpdates :: DescriptorUpdate 
                               , condFormatRulesUpdates :: CondFormatRuleUpdate
                               }
                               deriving (Eq, Show, Read, Generic)

sheetUpdateFromCommit :: ASCommit -> SheetUpdate
sheetUpdateFromCommit (Commit cd bd rdd cfrd _) = SheetUpdate cu bu rdu cfru
  where 
    cu   = diffToUpdate cd 
    bu   = diffToUpdate bd 
    rdu  = diffToUpdate rdd
    cfru = diffToUpdate cfrd


instance FromJSON ASTime
instance ToJSON ASTime

instance FromJSON ASCommit
instance ToJSON ASCommit

instance FromJSON SheetUpdate
instance ToJSON SheetUpdate

instance Serialize ASTime
instance Serialize ASCommit
instance Serialize SheetUpdate

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers

-- Timchu, 12/14/15. Commented out this existing function. This does not appear to be used anywhere.
-- mergeCommits :: ASCommit -> ASCommit -> ASCommit
-- mergeCommits (Commit cdiff' ddiff' t) (Commit cdiff ddiff _) = Commit cdiff'' ddiff'' t
--   where
--     cdiff'' = CellDiff { beforeVals = mergeCells (beforeVals cdiff') (beforeVals cdiff)
--                        , afterVals = mergeCells (afterVals cdiff') (afterVals cdiff) }
--     ddiff'' = DescriptorDiff { afterVals = unionBy hasSameKey (afterVals ddiff') (afterVals ddiff)
--                              , beforeVals = unionBy hasSameKey (beforeVals ddiff') (beforeVals ddiff) }
--     hasSameKey d1 d2 = (descriptorKey d1) == (descriptorKey d2)

getASTime :: IO ASTime
getASTime = return $ Time "hi" 1 2 3

emptyTime = Time "hi" 1 2 3

emptyCommitWithTime :: ASTime -> ASCommit
emptyCommitWithTime = Commit emptyDiff emptyDiff emptyDiff emptyDiff

generateCommitFromCells :: [ASCell] -> IO ASCommit
generateCommitFromCells cells = do 
  time <- getASTime
  let cdiff = Diff { beforeVals = [], afterVals = cells }
  return $ Commit cdiff emptyDiff emptyDiff emptyDiff time

