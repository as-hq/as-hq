module Handler.Clear where

import Import
import AS.Config.Settings
import AS.HandlerLibrary
import qualified AS.DB as DB
import AS.Eval as E
import AS.Types

optionsClearR :: Handler RepPlain
optionsClearR = do
    addCorsHeaders
    return $ RepPlain $ toContent ("" :: Text)

getClearR :: Handler ()
getClearR = (runDB $ do
	deleteWhere ([] :: [Filter ASCellDB])
	deleteWhere ([] :: [Filter RelationDB]))
	>> E.clearReplRecord Python
    >> redirect (frontend_url :: String)

postClearR :: Handler Value
postClearR = interactHandlerJson DB.deleteCell