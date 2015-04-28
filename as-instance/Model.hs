{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
let mongoSettings = (mkPersistSettings (ConT ''MongoContext))
 in share [mkPersist mongoSettings]
    $(persistFileWith upperCaseSettings "config/models")

instance ToJSON (Entity ASFunc) where
    toJSON (Entity funcId (ASFunc alias replace importName importCommand lang)) = object
        [ "alias" .= alias
        , "replace" .= replace
        , "importName" .= importName
        , "importCommand" .= importCommand
        , "lang" .= lang
        ]
instance FromJSON ASFunc where
    parseJSON (Object o) = ASFunc
        <$> o .: "alias"
        <*> o .: "replace"
        <*> o .: "importName"
        <*> o .: "importCommand"
        <*> o .: "lang"
    parseJSON _ = fail "Invalid func declaration"