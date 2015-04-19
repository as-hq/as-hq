{-# LANGUAGE OverloadedStrings #-}

module AS.HandlerLibrary where

import Import
import Data.Aeson
import Data.String
import Data.Text

interactHandlerJson :: (FromJSON a, ToJSON b) => String -> (a -> Handler b) -> Handler Value
interactHandlerJson argName process = do
  let stringName = fromString argName
  maybeData <- lookupGetParam stringName
  case maybeData of
    Nothing -> invalidArgs [stringName]
    Just paramData ->
      case (decode $ fromString $ Data.Text.unpack $ paramData) of
        Nothing -> invalidArgs [stringName]
        Just procData -> process procData >>= returnJson

