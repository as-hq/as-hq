{-# LANGUAGE OverloadedStrings, GADTs, DataKinds #-}

module AS.DB.Internal where

import AS.Prelude
import AS.Config.Settings
import AS.Types.DB
import AS.Types.Cell
import AS.Types.CellProps
import AS.Types.Eval
import AS.Types.Network
import AS.Util as U
import AS.Logging
import AS.Parsing.Show (showPrimitive)

import Database.Redis hiding (decode, Message)

----------------------------------------------------------------------------------------------------------------------
-- Settings

connectRedis :: IO Connection
connectRedis = do
  host <- getSetting dbHost
  port <- getSetting dbPort
  pwd <- getSetting dbPassword
  connect ConnInfo
    { connectHost           = host
    , connectPort           = PortNumber $ fromIntegral port
    , connectAuth           = pwd
    , connectDatabase       = 0
    , connectMaxConnections = 100
    , connectMaxIdleTime    = 1000000
    }

----------------------------------------------------------------------------------------------------------------------
-- Fat cells

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