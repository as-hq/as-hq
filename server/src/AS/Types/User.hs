{-# LANGUAGE DeriveGeneric #-}

module AS.Types.User where

import AS.Types.Common

import GHC.Generics
import Data.Text hiding (any)
import Data.Aeson
import Data.Serialize (Serialize)

type ASUserId = Text

data ASUserGroup = Group {groupMembers :: [ASUserId], groupAdmins :: [ASUserId], groupName :: Text} deriving (Show, Read, Eq, Generic)
data ASUserEntity = EntityGroup ASUserGroup |
                EntityUser ASUserId
                deriving (Show, Read, Eq, Generic)

data ASPermissions = Blacklist [ASUserEntity] |
                     Whitelist [ASUserEntity]
                      deriving (Show, Read, Eq, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

instance FromJSON ASUserGroup
instance ToJSON ASUserGroup

instance FromJSON ASUserEntity
instance ToJSON ASUserEntity

instance FromJSON ASPermissions
instance ToJSON ASPermissions

instance Serialize ASPermissions
instance Serialize ASUserEntity
instance Serialize ASUserGroup

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers

isGroupMember :: ASUserId -> ASUserGroup -> Bool
isGroupMember uid group = any ((==) uid) (groupMembers group)

isGroupAdmin :: ASUserId -> ASUserGroup -> Bool
isGroupAdmin uid group = any ((==) uid) (groupAdmins group)

isInEntity :: ASUserId -> ASUserEntity -> Bool
isInEntity uid (EntityGroup group) = isGroupMember uid group
isInEntity uid (EntityUser userid) = uid == userid

hasPermissions :: ASUserId -> ASPermissions -> Bool
hasPermissions uid (Blacklist entities) = not $ any (isInEntity uid) entities
hasPermissions uid (Whitelist entities) = any (isInEntity uid) entities
