{-# LANGUAGE TypeFamilies #-}

module AS.Types.User where

import AS.Types.Sheets

import AS.ASJSON
import Data.Set

import GHC.Generics
import Data.Text hiding (any)
import Data.SafeCopy

import Control.Lens

import Data.SafeCopy
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import qualified Data.Set as Set

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

data ASUser = 
  User {_sheetIds :: Set ASSheetId, _sharedSheetIds :: Set ASSheetId, _userId :: ASUserId, _lastOpenSheet :: ASSheetId}
  deriving (Show, Read, Eq, Generic)

makeLenses ''ASUser
deriveSafeCopy 2 'extension ''ASUser
----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

asToFromJSON ''ASUserGroup
asToFromJSON ''ASUserEntity
asToFromJSON ''ASPermissions

instance NFData ASPermissions
instance NFData ASUserEntity
instance NFData ASUserGroup
deriveSafeCopy 1 'base ''ASPermissions
deriveSafeCopy 1 'base ''ASUserEntity
deriveSafeCopy 1 'base ''ASUserGroup
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

----------------------------------------------------------------------------
-------------------------------- MIGRATIONS -------------------------------- 

-- [ASUser, breaking commit: 215aecf5]
data ASUser0 = User0 {_sheetIds0 :: Set ASSheetId, _userId0 :: ASUserId, _lastOpenSheet0 :: ASSheetId} deriving (Generic)
makeLenses ''ASUser0
deriveSafeCopy 1 'base ''ASUser0

instance Migrate ASUser where
  type MigrateFrom ASUser = ASUser0
  migrate (User0 sids uid ls) = User sids Set.empty uid ls

-- [TYPE_CHANGED, breaking commit: BREAKING_COMMMIT]
-- ^^ copy this line when you start a migration, 
--    and write the migration above this footer.