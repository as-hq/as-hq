module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation)
import ClassyPrelude         as X
import Database.Persist      as X hiding (get)
import Database.Persist.MongoDB hiding (master)
import Foundation            as X
import Model                 as X
import Settings              (appDatabaseConf)
import Test.Hspec            as X
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test            as X
import Database.MongoDB.Query (allCollections)
import Database.MongoDB.Admin (dropCollection)
import Control.Monad.Trans.Control (MonadBaseControl)

runDB :: Action IO a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> Action IO a -> IO a
runDBWithApp app action = do
  liftIO $ runMongoDBPool
    (mgAccessMode $ appDatabaseConf $ appSettings app)
    action
    (appConnPool app)


withApp :: SpecWith App -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    return foundation

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app =
  void $ runDBWithApp app $ do
    cols <- allCollections
    let validCols = filter (not . isSystemCollection) cols
    mapM dropCollection validCols
  where
    isSystemCollection = isPrefixOf "system."

