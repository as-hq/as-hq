{-# LANGUAGE TypeFamilies #-}

module AS.Types.Sheets where

import Data.SafeCopy
import Data.Aeson
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Data.SafeCopy
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

import AS.Prelude 
import AS.ASJSON

--------------------------------------------------------------------------------
-- Types

type WorkbookName   = String
type SheetName      = String

type SheetID      = Text
type WorkbookID   = Text

type SheetOwner     = Text
type WorkbookOwner  = Text 
-- above two are UserID, really, but import cycles. 

data Sheet = Sheet 
  { _sheetId :: SheetID
  , _sheetName :: SheetName
  , _sheetOwner :: SheetOwner
  , _inPauseMode :: Bool
  } deriving (Show, Read, Eq, Generic, Data)
-- ^ Sheet has a sheetOwner type so that we don't have to enumerate the
-- set of all users in order to find a sheet's owner. This way, users have a
-- forward pointer to sheets, and sheets have backward pointers to their owners.

data Workbook = Workbook 
  { _workbookId :: WorkbookID
  , _workbookName :: WorkbookName
  , _workbookSheetIds :: Set SheetID
  , _workbookOwner :: WorkbookOwner
  , _lastOpenSheet :: SheetID
  } deriving (Show, Read, Eq, Generic)

-- | Type that exists solely for communicating workbook updates to frontend.
data OpenedWorkbook = OpenedWorkbook
  { openedWorkbookId :: WorkbookID
  , openedWorkbookName :: WorkbookName
  , openedWorkbookOwner :: WorkbookOwner
  , openedWorkbookSheets :: [Sheet]
  , openedSheet :: SheetID
  } deriving (Show, Read, Eq, Generic)

-- | Exists solely to communicate list of user's workbooks to frontend.
data WorkbookRef = WorkbookRef 
  { workbookRefId :: WorkbookID
  , workbookRefName :: WorkbookName
  , workbookRefOwner :: WorkbookOwner
  } deriving (Show, Read, Eq, Generic)

makeLenses ''Sheet
makeLenses ''Workbook

deriveSafeCopy 3 'extension ''Sheet

instance ToJSON Sheet where
  toJSON s = object [ "id" .= (s^.sheetId)
                    , "name" .= (s^.sheetName)
                    , "owner" .= (s^.sheetOwner)
                    , "inPauseMode" .= (s^.inPauseMode)
                    ]

instance ToJSON OpenedWorkbook where
  toJSON w = object [ "id" .= openedWorkbookId w
                    , "name" .= openedWorkbookName w
                    , "owner" .= openedWorkbookOwner w
                    , "sheets" .= openedWorkbookSheets w
                    , "openedSheet" .= openedSheet w
                    ]

instance ToJSON WorkbookRef where
  toJSON w = object [ "id" .= workbookRefId w
                    , "name" .= workbookRefName w
                    , "owner" .= workbookRefOwner w
                    ]

--------------------------------------------------------------------------------
-- Instances

instance NFData Sheet
instance NFData Workbook

--------------------------------------------------------------------------------
-- Migrations

-- Note that, if you use things like "SheetID" in these definitions, and those
-- type definitions change (say Text -> ByteString), these can fail in subtle
-- ways. Always expand out the types, or create type SheetID1 = ...

-- [TYPE_CHANGED, breaking commit: BREAKING_COMMMIT]

data Sheet0 = Sheet0 
  { sheetId0 :: Text
  , sheetName0 :: String
  } deriving (Generic)
deriveSafeCopy 1 'base ''Sheet0

data Sheet1 = Sheet1
  { sheetId1 :: Text
  , sheetName1 :: String
  , sheetOwner1 :: Text
  } deriving (Generic)
deriveSafeCopy 2 'extension ''Sheet1

instance Migrate Sheet1 where
  type MigrateFrom Sheet1 = Sheet0
  migrate (Sheet0 sid sname) = Sheet1 sid sname "alphasheetsdemo@gmail.com"

--------------------------------------------------------------------------------
-- [TYPE_CHANGED, breaking commit: 05f3d3b54d65778675f068f7883c5944c9e26952]

instance Migrate Sheet where
  type MigrateFrom Sheet = Sheet1
  migrate (Sheet1 sid sname owner) = Sheet sid sname owner False

--------------------------------------------------------------------------------
