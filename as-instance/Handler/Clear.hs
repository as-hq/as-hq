module Handler.Clear where

import Import

getClearR :: Handler Html
getClearR = runDB $ do
	deleteWhere ([] :: [Filter ASCellDB])
	deleteWhere ([] :: [Filter RelationDB])
	redirect ("http://asrini-host.mit.edu/app" :: String)
