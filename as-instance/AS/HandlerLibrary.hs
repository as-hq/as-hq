module AS.HandlerLibrary where

import Import

interactHandlerJson :: (FromJSON a, ToJSON b) => String -> (a -> Handler b) -> Handler Value
interactHandlerJson argName process = do
  maybeData <- lookupGetParam argName
  case maybeData of
    Nothing -> invalidArgs [argName]
    Just paramData ->
      case (parseJSON paramData) of
        Nothing -> invalidArgs [argName]
        Just procData -> process procData >>= returnJson

