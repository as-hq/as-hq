{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AS.Types.EvalHeader where

import AS.Prelude

import AS.ASJSON
import AS.Config.Constants

import AS.Types.Cell

import Data.SafeCopy
import Data.Text (Text)

data EvalHeader = EvalHeader 
  { _evalHeaderWorkbookId :: WorkbookID
  , _evalHeaderLang    :: ASLanguage
  , _evalHeaderExpr    :: String
  } 
  deriving (Show, Read, Eq, Data, Typeable, Generic)                           
deriveSafeCopy 3 'extension ''EvalHeader

defaultHeaderText :: ASLanguage -> String
defaultHeaderText Python = pythonHeaderDefaultText
defaultHeaderText R      = rHeaderDefaultText
defaultHeaderText _      = ""

isDefaultHeader :: String -> Bool
isDefaultHeader s = s `elem` [pythonHeaderDefaultText, rHeaderDefaultText]

asLensedToFromJSON ''EvalHeader
makeLenses ''EvalHeader

----------------------------------------------------------------------------------------------------------------------------------------------
-- Migrations

data EvalHeader0 = EvalHeader0
  { _evalHeaderSheetId0 :: Text
  , _evalHeaderLang0 :: ASLanguage
  , _evalHeaderExpr0 :: String
  } deriving (Show, Eq, Generic)

deriveSafeCopy 2 'base ''EvalHeader0
makeLenses ''EvalHeader0

instance Migrate EvalHeader where
  type MigrateFrom EvalHeader = EvalHeader0
  -- shove sheetId into workbookId field for usage by manual migrater.
  migrate (EvalHeader0 sid lang expr) = EvalHeader sid lang expr 