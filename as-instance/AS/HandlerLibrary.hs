{-# LANGUAGE OverloadedStrings #-}

module AS.HandlerLibrary where

import Import hiding (toStrict, toLazyText)
import Data.Aeson
import Data.Aeson.Encode
import Data.String
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

interactHandlerJson :: (FromJSON a, ToJSON a, ToJSON b) => (a -> Handler b) -> Handler Value
interactHandlerJson process = parseJsonBody >>= \obj ->
	case obj of
	    Error s -> do
	    	$(logInfo) $ "Error: " ++ (fromString s)
	    	invalidArgs []
	    Success procData -> do
	    	$(logInfo) $ "procData: " ++ (toStrict . toLazyText . encodeToTextBuilder . toJSON $ procData)
	    	result <- process procData
	    	$(logInfo) $ "Success: " ++ (toStrict . toLazyText . encodeToTextBuilder . toJSON $ result)
	    	returnJson result