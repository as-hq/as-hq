{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Commits where

import AS.Types.Cell
import AS.Types.Eval
import AS.Types.CondFormat
import AS.Types.Updates
import AS.Types.Bar

import GHC.Generics
import Data.Aeson hiding (Success)
import Data.List
import qualified Data.Text as T

import Data.Serialize (Serialize)


----------------------------------------------------------------------------------------------------------------------------------------------
-- Version Control

data ASTime = Time {day :: String, hour :: Int, minute :: Int, sec :: Int} deriving (Show, Read, Eq, Generic)

data ASCommit = Commit { barDiff :: BarDiff
                       , cellDiff :: CellDiff
                       , rangeDescriptorDiff :: DescriptorDiff
                       , time :: ASTime }
                       deriving (Show, Read, Generic)

data CommitSource = CommitSource { srcSheetId :: ASSheetId, srcUserId :: ASUserId } deriving (Generic)

type CommitTransform = ASCommit -> ASCommit

flipCommit :: ASCommit -> ASCommit
flipCommit (Commit bd cd rdd time) = Commit bd' cd' rdd' time 
  where 
    bd'  = flipDiff bd
    cd'  = flipDiff cd 
    rdd' = flipDiff rdd 

-- Represents a set of collections of a sheet. 
data SheetUpdate = SheetUpdate { cellUpdates :: CellUpdate
                               , barUpdates :: BarUpdate
                               , descriptorUpdates :: DescriptorUpdate 
                               , condFormatRulesUpdates :: CondFormatRuleUpdate
                               }
                               deriving (Show, Read, Generic)

sheetUpdateFromCommit :: ASCommit -> SheetUpdate
sheetUpdateFromCommit (Commit bd cd rdd t0) = SheetUpdate cu bu rdu cfu
  where 
    bu  = diffToUpdate bd 
    cu  = diffToUpdate cd 
    rdu = diffToUpdate rdd
    cfu = Update [] [] -- #incomplete conditional formatting updates have not been implemented yet 


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

generateCommitFromCells :: [ASCell] -> IO ASCommit
generateCommitFromCells cells = do 
  time <- getASTime
  let cdiff = Diff { beforeVals = [], afterVals = cells }
  return $ Commit emptyDiff cdiff emptyDiff time

