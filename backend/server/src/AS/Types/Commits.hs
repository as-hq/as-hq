{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module AS.Types.Commits where

import AS.ASJSON

import AS.Types.Cell
import AS.Types.CondFormat
import AS.Types.Updates
import AS.Types.Bar
import AS.Types.RangeDescriptor
import AS.Types.User

import GHC.Generics

import Data.List
import qualified Data.Text as T
import Data.SafeCopy
import Control.Lens
import Control.Lens.TH


----------------------------------------------------------------------------------------------------------------------------------------------
-- Version Control

data ASTime = Time {day :: String, hour :: Int, minute :: Int, sec :: Int} deriving (Show, Read, Eq, Generic)

-- Note: this does not actually include ALL the information stored in a sheet (you can 
-- find this in ExportData in Types/DB.hs). This only records the information that should
-- conceptually get undone if the user presses Ctrl+Z in the spreadsheet. 
-- 
-- An example of information this does NOT include is the content in the evaluation header. 
-- Updating the contents of the eval header and undoing in the spreadsheet should not unset
-- the contents of the eval header. 
data ASCommit = Commit { cellDiff :: CellDiff
                       , barDiff :: BarDiff
                       , rangeDescriptorDiff :: DescriptorDiff
                       , condFormatRulesDiff :: CondFormatRuleDiff
                       , time :: ASTime }
                       deriving (Show, Read, Generic)

data CommitSource = CommitSource { srcSheetId :: ASSheetId, srcUserId :: ASUserId } deriving (Show, Generic)

type UpdateTransform = SheetUpdate -> SheetUpdate

flipCommit :: ASCommit -> ASCommit
flipCommit (Commit cd bd rdd cfrd time) = Commit cd' bd' rdd' cfrd' time 
  where 
    cd'   = flipDiff cd 
    bd'   = flipDiff bd
    rdd'  = flipDiff rdd 
    cfrd' = flipDiff cfrd 

-- Represents a set of collections of a sheet. 
data SheetUpdate = SheetUpdate { _cellUpdates :: CellUpdate
                               , _barUpdates :: BarUpdate
                               , _descriptorUpdates :: DescriptorUpdate 
                               , _condFormatRuleUpdate :: CondFormatRuleUpdate
                               }
                               deriving (Eq, Show, Read, Generic)
makeLenses ''SheetUpdate

emptySheetUpdate :: SheetUpdate
emptySheetUpdate = SheetUpdate emptyUpdate emptyUpdate emptyUpdate emptyUpdate

-- #RoomForImprovement: some data.data magic to have the args be [ValType CellUpdate]
-- instead of [ASCell]?
makeSheetUpdateWithNoOldKeys :: [ASCell] -> [Bar] -> [RangeDescriptor] -> [CondFormatRule] -> SheetUpdate
makeSheetUpdateWithNoOldKeys cells bars ds cfs = SheetUpdate (Update cells []) (Update bars []) (Update ds []) (Update cfs [])

sheetUpdateFromCommit :: ASCommit -> SheetUpdate
sheetUpdateFromCommit (Commit cd bd rdd cfrd _) = SheetUpdate cu bu rdu cfru
  where 
    cu   = diffToUpdate cd 
    bu   = diffToUpdate bd 
    rdu  = diffToUpdate rdd
    cfru = diffToUpdate cfrd

sheetUpdateFromCells :: [ASCell] -> SheetUpdate
sheetUpdateFromCells cs = emptySheetUpdate & cellUpdates.newVals .~ cs

addCellsToUpdate :: [ASCell] -> SheetUpdate -> SheetUpdate
addCellsToUpdate cs = cellUpdates.newVals %~ (++ cs)

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

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

asToJSON ''ASTime
asLensedToJSON ''SheetUpdate

deriveSafeCopy 1 'base ''ASCommit
deriveSafeCopy 1 'base ''ASTime
