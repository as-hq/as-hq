{-# LANGUAGE DeriveGeneric #-}

module AS.Types.Sheets
  ( module AS.Types.Sheets
  , module AS.Types.User
  ) where

import AS.Types.User

import GHC.Generics
import Data.Aeson
import Data.Text

type WorkbookName = String
type SheetName = String
type ASSheetId = Text

data ASSheet = Sheet {sheetId :: ASSheetId, sheetName :: SheetName, sheetPermissions :: ASPermissions} deriving (Show, Read, Eq, Generic)
-- should probably be a list of ASSheet's rather than ASSheetId's. 
data ASWorkbook = Workbook {workbookName :: WorkbookName, workbookSheets :: [ASSheetId]} deriving (Show, Read, Eq, Generic)
-- this type needs to be refactored away. It's used in a frontend API in basically exactly the
-- same way that ASWorkbook is supposed to be used. (Alex 11/3) 
data WorkbookSheet = WorkbookSheet {wsName :: WorkbookName, wsSheets :: [ASSheet]} deriving (Show, Read, Eq, Generic)

instance ToJSON ASSheet
instance FromJSON ASSheet

instance FromJSON ASWorkbook
instance ToJSON ASWorkbook

instance FromJSON WorkbookSheet
instance ToJSON WorkbookSheet
