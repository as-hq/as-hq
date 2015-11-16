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
