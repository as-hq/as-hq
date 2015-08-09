module Handler.Initiate where

import Import
import AS.HandlerLibrary
import AS.Types hiding (error)

optionsInitiateR :: Handler RepPlain
optionsInitiateR = do
    addCorsHeaders
    return $ RepPlain $ toContent ("" :: Text)

postInitiateR :: Handler Value
postInitiateR = do
    websockets socketApp

socketApp :: WebSocketsT Handler ()
socketApp = do
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    App writeChan <- getYesod
    readChan <- atomically $ do
        writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan
    race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg ->
            atomically $ writeTChan writeChan $ name <> ": " <> msg))