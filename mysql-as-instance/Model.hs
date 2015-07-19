{-# LANGUAGE FlexibleInstances #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity ASFunc) where
    toJSON (Entity funcId (ASFunc alias replace importName importCommand location lang)) = object
        [ "alias"           .= alias
        , "replace"         .= replace
        , "importName"      .= importName
        , "importCommand"   .= importCommand
        , "location"        .= location
        , "lang"            .= lang
        ]
instance FromJSON ASFunc where
    parseJSON (Object o) = ASFunc
        <$> o .: "alias"
        <*> o .: "replace"
        <*> o .: "importName"
        <*> o .: "importCommand"
        <*> o .: "location"
        <*> o .: "lang"
    parseJSON _ = fail "Invalid func declaration"