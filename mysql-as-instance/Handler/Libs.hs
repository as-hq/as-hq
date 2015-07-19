module Handler.Libs where

import Import
import AS.HandlerLibrary
import AS.Types
import AS.DB 

optionsLibsR :: Handler RepPlain
optionsLibsR = do
    addCorsHeaders
    return $ RepPlain $ toContent ("" :: Text)

getLibsR :: Handler Value
getLibsR = 
	runDB (selectList [] []) >>= jsonToRepJson . asASFuncEntities
  where
    asASFuncEntities :: [Entity ASFunc] -> [Entity ASFunc]
    asASFuncEntities = id

postLibsR :: Handler ()
postLibsR = do
	func <- parseJsonBody_
	(setFunc func) >> return ()
