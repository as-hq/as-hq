{-# LANGUAGE OverloadedStrings, GADTs, DataKinds #-}

module AS.DB.Internal where

import Prelude()
import AS.Prelude

import AS.Types.DB
import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Eval
import AS.Types.Network

import AS.Util as U
import AS.Logging
import AS.Parsing.Common (tryParseListNonIso)
import AS.Parsing.Read (integer)
import AS.Parsing.Show (showPrimitive)
import qualified AS.Serialize as S

import qualified Data.List                     as L
import qualified Data.Text                     as T
import           Data.List.Split
import Data.Maybe (fromJust, catMaybes)
import Data.SafeCopy (SafeCopy)

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC

import Database.Redis hiding (decode, Message)

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Lens

import Network.Socket.Internal

----------------------------------------------------------------------------------------------------------------------
-- Settings

connectRedis :: AppSettings -> IO Connection
connectRedis settings = connect $ ConnInfo
    { connectHost           = settings^.redisHost
    , connectPort           = PortNumber $ fromIntegral (settings^.redisPort)
    , connectAuth           = settings^.redisPassword
    , connectDatabase       = 0
    , connectMaxConnections = 100
    , connectMaxIdleTime    = 1000000
    }
    
----------------------------------------------------------------------------------------------------------------------
-- Private DB functions

-- given "Untitled" and ["Untitled1", "Untitled2"], produces "Untitled3"
-- only works on integer suffixes
getUniquePrefixedName :: String -> [String] -> String
getUniquePrefixedName pref strs = pref ++ (show idx)
  where
    strs'   = filter (L.isPrefixOf pref) strs
    strs''  = map (drop . length $ pref) strs'
    idxs    = tryParseListNonIso integer strs''
    idx     = case idxs of
      [] -> 1
      _  -> (L.maximum idxs) + 1

----------------------------------------------------------------------------------------------------------------------
-- Fat cells

--  #expert
toDecoupled :: ASCell -> ASCell
toDecoupled c@(Cell { _cellRangeKey = Just _ }) = c & cellExpression .~ e' & cellRangeKey .~ Nothing  
  where 
    lang = c^.cellExpression.language
    v = c^.cellValue
    e' = case v of 
             NoValue   -> Expression "" lang
             otherwise -> Expression (showPrimitive lang v) lang
toDecoupled c = c

-- | Converts a coupled cell to a normal cell
toUncoupled :: ASCell -> ASCell
toUncoupled c@(Cell { _cellRangeKey = Just _ }) = c & cellRangeKey .~ Nothing