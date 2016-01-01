{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module AS.Types.User where

import AS.ASJSON

import AS.Types.Common

import GHC.Generics
import Data.Text hiding (any)

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

asToFromJSON ''ASUserGroup
asToFromJSON ''ASUserEntity
asToFromJSON ''ASPermissions

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
