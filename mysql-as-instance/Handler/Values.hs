module Handler.Values where

import Import
import AS.HandlerLibrary
import AS.Types hiding (error)
import qualified AS.Dispatch as DP

optionsValuesR :: Handler RepPlain
optionsValuesR = do
    addCorsHeaders
    return $ RepPlain $ toContent ("" :: Text)

postValuesR :: Handler Value
postValuesR = interactHandlerJson DP.evaluatePrimitive

putValuesR :: Handler Value
putValuesR = interactHandlerJson DP.insertCellImmediate