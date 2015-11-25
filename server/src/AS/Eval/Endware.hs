module AS.Eval.Endware where

import Prelude
import AS.Types.Core
import AS.Util as U
import AS.Eval.Core
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

import Database.Redis as R
import AS.Daemon as DM


-- | Here, we apply a stack of endwares.
-- | Endware for producing tags post-eval e.g. streaming or styling
-- | Examples: green(x) in python -> produces styled value with string in output -> string parsed to Color tag
-- | Bloomberg(x) in java -> produces json with stream specs -> converted to Stream tag, kickoff daemon

evalEndware :: MVar ServerState -> [ASCell] -> CommitSource -> [ASCell] -> IO [ASCell]
evalEndware state finalCells (sid, uid) origCells = do 
  mapM_ (DM.possiblyCreateDaemon state uid) origCells
  let cells1 = changeExcelExpressions finalCells
  conn <- dbConn <$> readMVar state
  cells2 <- conditionallyFormatCells conn sid cells1
  return cells2
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

conditionallyFormatCells :: R.Connection -> ASSheetId -> [ASCell] -> EitherTExec [ASCell]
conditionallyFormatCells conn origSid cells = do 
  rules <- getCondFormattingRules conn origSid
  let transforms = map (ruleToCellTransform sid) rules
  mapM transforms cells

ruleToCellTransform :: ASSheetId -> CondFormatRule -> (ASCell -> EitherTExec ASCell)
ruleToCellTransform sid (CondFormatRule rngs cond format) c@(Cell l e v ps) = do
  let inside =  any (flip U.rangeContainsIndex l) rngs
  if !inside 
    then return c
    else do 
  satisfiesCond <- meetsCondition sid cond v
  if satisfiesCond
    then return $ Cell l e v (setProp format ps)
    else return c

meetsCondition :: ASSheetId -> ASExpression -> ASValue -> EitherTExec Bool
meetsCondition sid (Expression xp lang) v = do
  evaluateLanguageWithSubstitutions sid lang 