module Handler.Clear where

import Import
import AS.Config.Settings

getClearR :: Handler Html
getClearR = runDB $ do
	deleteWhere ([] :: [Filter ASCellDB])
	deleteWhere ([] :: [Filter RelationDB])
	redirect (frontend_url :: String)
