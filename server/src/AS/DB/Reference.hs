{-# LANGUAGE OverloadedStrings #-}

module AS.DB.Reference where

import Prelude

import AS.Types.Core hiding (location,expression,value,min)
import AS.Types.DB
import AS.Util as U
import qualified AS.DB.Util as DU
import AS.Parsing.Substitutions (getDependencies)
import AS.DB.Graph as G

import Data.List (zip4,head,partition,nub,intercalate)
import Data.Maybe (isNothing,fromJust,catMaybes)

import Foreign
import Foreign.C.Types
import Foreign.C.String(CString(..))
import Foreign.C

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Time
import Database.Redis hiding (decode)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson hiding (Success)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Unsafe as BU
import Data.List.Split
-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

refToCells :: ExRef -> ASSheetId -> IO [ASCell]
refToCells ref sid = 
  let ref = exRefToASRef ref sid
  in do
    mcells <- DB.getCell ref
-- | Update the ancestor relationships in the DB based on the expressions and locations of the
-- cells passed in. (E.g. if a cell is passed in at A1 and its expression is "C1 + 1", C1->A1 is
-- added to the graph.)
setCellsAncestors :: [ASCell] -> EitherTExec [[ASReference]]
setCellsAncestors cells = G.setRelations relations >> return depSets
  where
    depSets = map (\(Cell l e _ _) -> getDependencies (locSheetId l) e) cells
    zipSets = zip cells depSets
    relations = map (\((Cell l _ _ _), depSet) -> (l, concat $ catMaybes $ map refToIndices depSet)) zipSets

-- | Should only be called when undoing or redoing commits, which should be guaranteed to not
-- introduce errors.
setCellsAncestorsForce :: [ASCell] -> IO ()
setCellsAncestorsForce cells = do
  runEitherT (setCellsAncestors cells)
  return ()