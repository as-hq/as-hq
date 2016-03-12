{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module AS.Types.Sheets where

import AS.Prelude 
import Prelude()

import AS.ASJSON

import Data.SafeCopy
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)

import GHC.Generics
import Data.Text
import Data.SafeCopy

type WorkbookName = String
type SheetName = String
type ASSheetId = Text

-- sheetOwner == ASUserId (type Text), but not resolving import cycles right now, so just using Text. (anand 3/11)
data ASSheet = Sheet {sheetId :: ASSheetId, sheetName :: SheetName, sheetOwner :: Text} deriving (Show, Read, Eq, Generic)
-- ^ ASSheet has a sheetOwner type so that we don't have to enumerate the
-- set of all users in order to find a sheet's owner. This way, users have a forward 
-- pointer to sheets, and sheets have backward pointers to their owners.

-- should probably be a list of ASSheet's rather than ASSheetId's. 
data ASWorkbook = Workbook {workbookName :: WorkbookName, workbookSheets :: [ASSheetId]} deriving (Show, Read, Eq, Generic)
-- this type needs to be refactored away. It's used in a frontend API in basically exactly the
-- same way that ASWorkbook is supposed to be used. (Alex 11/3) 
data WorkbookSheet = WorkbookSheet {wsName :: WorkbookName, wsSheets :: [ASSheet]} deriving (Show, Read, Eq, Generic)

asToFromJSON ''ASSheet
asToFromJSON ''ASWorkbook
asToFromJSON ''WorkbookSheet

instance NFData ASSheet
deriveSafeCopy 1 'base ''ASSheet
