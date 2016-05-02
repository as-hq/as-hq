{-# LANGUAGE TypeFamilies #-}

module AS.Types.Sheets where

import AS.Prelude 

import AS.ASJSON

import Data.SafeCopy
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)

import Data.Text
import Data.SafeCopy

type WorkbookName = String
type SheetName = String
type ASSheetId = Text

-- sheetOwner == ASUserId (type Text), but not resolving import cycles now 
-- so just using Text. (anand 3/11)
data ASSheet = Sheet 
  { sheetId :: ASSheetId
  , sheetName :: SheetName
  , sheetOwner :: Text
  , inPauseMode :: Bool
  } deriving (Show, Read, Eq, Generic, Data)
-- ^ ASSheet has a sheetOwner type so that we don't have to enumerate the
-- set of all users in order to find a sheet's owner. This way, users have a
-- forward pointer to sheets, and sheets have backward pointers to their owners.
deriveSafeCopy 3 'extension ''ASSheet

-- should probably be a list of ASSheet's rather than ASSheetId's. 
data ASWorkbook = Workbook 
  { workbookName :: WorkbookName
  , workbookSheets :: [ASSheetId]
  } deriving (Show, Read, Eq, Generic)

data WorkbookSheet = WorkbookSheet 
  { wsName :: WorkbookName
  , wsSheets :: [ASSheet]
  } deriving (Show, Read, Eq, Generic)

asToFromJSON ''ASSheet
asToFromJSON ''ASWorkbook
asToFromJSON ''WorkbookSheet

instance NFData ASSheet

----------------------------------------------------------------------------
-------------------------------- MIGRATIONS -------------------------------- 

-- [TYPE_CHANGED, breaking commit: BREAKING_COMMMIT]

data ASSheet0 = Sheet0 
  { sheetId0 :: ASSheetId
  , sheetName0 :: SheetName
  } deriving (Generic)
deriveSafeCopy 1 'base ''ASSheet0

instance Migrate ASSheet1 where
  type MigrateFrom ASSheet1 = ASSheet0
  migrate (Sheet0 sid sname) = Sheet1 sid sname "alphasheetsdemo@gmail.com"

data ASSheet1 = Sheet1
  { sheetId1 :: ASSheetId
  , sheetName1 :: SheetName
  , sheetOwner1 :: Text
  } deriving (Generic)
deriveSafeCopy 2 'extension ''ASSheet1

instance Migrate ASSheet where
  type MigrateFrom ASSheet = ASSheet1
  migrate (Sheet1 sid sname owner) = Sheet sid sname owner False

-- [TYPE_CHANGED, breaking commit: BREAKING_COMMMIT]


