
{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module AS.Types.User where

import AS.Types.Sheets

import AS.ASJSON
import Data.Set

import GHC.Generics
import Data.Text hiding (any)
import Data.SafeCopy

import Control.Lens

type ASUserId = Text

data ASUserGroup = Group {groupMembers :: [ASUserId], groupAdmins :: [ASUserId], groupName :: Text}
  deriving (Show, Read, Eq, Generic)
  
data ASUserEntity = 
    EntityGroup ASUserGroup 
  | EntityUser ASUserId
  deriving (Show, Read, Eq, Generic)

data ASPermissions = 
    Blacklist [ASUserEntity]
  | Whitelist [ASUserEntity]
  deriving (Show, Read, Eq, Generic)

data ASUser = User {_sheetIds :: Set ASSheetId, _userId :: ASUserId, _lastOpenSheet :: ASSheetId}
  deriving (Show, Read, Eq, Generic)

makeLenses ''ASUser
deriveSafeCopy 1 'base ''ASUser
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
