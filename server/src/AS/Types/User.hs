{-# LANGUAGE DeriveGeneric #-}

module AS.Types.User where

import GHC.Generics
import Data.Text
import Data.Aeson

type ASUserId = Text


data ASUserGroup = Group {groupMembers :: [ASUserId], groupAdmins :: [ASUserId], groupName :: Text} deriving (Show, Read, Eq, Generic)
data ASUserEntity = EntityGroup ASUserGroup |
                EntityUser ASUserId
                deriving (Show, Read, Eq, Generic)

data ASPermissions = Blacklist [ASUserEntity] |
                     Whitelist [ASUserEntity]
                      deriving (Show, Read, Eq, Generic)

instance FromJSON ASUserGroup
instance ToJSON ASUserGroup

instance FromJSON ASUserEntity
instance ToJSON ASUserEntity

instance FromJSON ASPermissions
instance ToJSON ASPermissions
