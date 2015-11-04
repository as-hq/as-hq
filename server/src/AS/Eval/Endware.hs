module AS.Eval.Endware where

import Prelude
import AS.Types.Core
import AS.Util as U
import qualified Data.List as L

import Data.Char (isPunctuation, isSpace, toUpper)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception 
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson hiding (Success)
import Data.ByteString.Char8 hiding (putStrLn,filter,any,length)
import Data.ByteString.Lazy.Char8 as B hiding (putStrLn,filter,any,length)
import qualified Network.WebSockets as WS

import AS.Daemon as DM


-- | Here, we apply a stack of endwares.
-- | Endware for producing tags post-eval e.g. streaming or styling
-- | Examples: green(x) in python -> produces styled value with string in output -> string parsed to Color tag
-- | Bloomberg(x) in java -> produces json with stream specs -> converted to Stream tag, kickoff daemon

evalEndware :: MVar ServerState -> [ASCell] -> CommitSource -> [ASCell] -> IO [ASCell]
evalEndware state finalCells (_, uid) origCells = do 
  let newCells = changeExcelExpressions finalCells
  mapM_ (DM.possiblyCreateDaemon state uid) origCells
  return newCells
  -- let newCells = (tagStyledCells . (changeExcelExpressions origCell)) finalCells
   
----------------------------------------------------------------------------------------------------------------------------------------------
-- Endwares

tagStyledCells :: [ASCell] -> [ASCell]
tagStyledCells = id

changeExcelExpressions :: [ASCell] -> [ASCell]
changeExcelExpressions = id
-- L.map upperCase
-- 	where
-- 		upperCase :: ASCell -> ASCell
-- 		upperCase (Cell l (Expression e Excel) v t) = Cell l (Expression e' Excel) v t 
-- 			where 
-- 				e' = L.map toUpper e
-- 		upperCase c = c
-- #incomplete should change all function names to upper-cased forms


----------------------------------------------------------------------------------------------------------------------------------------------





