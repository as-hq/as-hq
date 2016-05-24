{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AS.Types.User where

import Data.Text hiding (any)
import Data.SafeCopy
import Data.SafeCopy
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Data.Set (Set)
import qualified Data.Set as S
import Data.ByteString (ByteString)

import AS.Prelude
import AS.Types.Sheets
import AS.ASJSON

--------------------------------------------------------------------------------
-- Types

type UserID = Text

data User = User 
  { _userId :: UserID
  , _workbookIds :: Set WorkbookID
  , _lastOpenWorkbook :: WorkbookID
  } deriving (Show, Read, Eq, Generic)

makeLenses ''User
deriveSafeCopy 3 'extension ''User

--------------------------------------------------------------------------------
-- Migrations

-- [User, breaking commit: 215aecf5]

data User0 = User0 
  { _sheetIds0 :: Set Text
  , _userId0 :: Text
  , _lastOpenSheet0 :: Text
  } deriving (Generic)
makeLenses ''User0
deriveSafeCopy 1 'base ''User0

data User1 = User1 
  { _sheetIds1 :: Set Text
  , _sharedSheetIds1 :: Set Text
  , _userId1 :: Text
  , _lastOpenSheet1 :: Text}
  deriving (Show, Read, Eq, Generic)
makeLenses ''User1
deriveSafeCopy 2 'extension ''User1

instance Migrate User1 where
  type MigrateFrom User1 = User0
  migrate (User0 sids uid ls) = User1 sids S.empty uid ls

--------------------------------------------------------------------------------
-- Current migration

instance Migrate User where 
  type MigrateFrom User = User1
  migrate (User1 sids ssids uid los) = User uid (sids `S.union` ssids) los

--------------------------------------------------------------------------------