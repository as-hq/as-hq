{-# LANGUAGE TemplateHaskell #-}

module AS.ASJSON where

import AS.Prelude
import Language.Haskell.TH
import Data.Aeson
import Data.Aeson.TH

asToFromJSON :: Name -> Q [Dec]
asToFromJSON name = [d|
  instance FromJSON $(conT name) 
  instance ToJSON $(conT name) |]

asToJSON :: Name -> Q [Dec]
asToJSON name = [d|
  instance ToJSON $(conT name) |]

asFromJSON :: Name -> Q [Dec]
asFromJSON name = [d|
  instance FromJSON $(conT name) |]


-- This code taken from http://stackoverflow.com/questions/28123867/aeson-and-lens-with-derivegeneric-and-makelenses-names-dont-line-up
-- For fields with underscores in front, this drops the underscore when reading to/from JSONs
asLensedToJSON :: Name -> Q [Dec]
asLensedToJSON name = [d|
  instance ToJSON $(conT name) where 
    toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 1} |]

asLensedFromJSON :: Name -> Q [Dec]
asLensedFromJSON name = [d|
  instance FromJSON $(conT name) where 
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1} |]

asLensedToFromJSON :: Name -> Q [Dec]
asLensedToFromJSON name = [d|
  instance ToJSON $(conT name) where 
    toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 1}
  instance FromJSON $(conT name) where 
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1} |]