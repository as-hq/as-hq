{-# LANGUAGE TemplateHaskell #-}

module AS.ASJSON where

import Language.Haskell.TH
import Data.Aeson
import Data.Serialize (Serialize)

asToFromJSON :: Name -> Q [Dec]
asToFromJSON name = [d|
  instance FromJSON $(conT name) 
  instance ToJSON $(conT name) 
  instance Serialize $(conT name)|]

asToJSON :: Name -> Q [Dec]
asToJSON name = [d|
  instance ToJSON $(conT name) 
  instance Serialize $(conT name)|]

asFromJSON :: Name -> Q [Dec]
asFromJSON name = [d|
  instance FromJSON $(conT name) 
  instance Serialize $(conT name)|]