module AS.Types.EvalHeader where

import AS.Prelude
import Prelude()

import AS.ASJSON

import AS.Types.Cell

import GHC.Generics
import Control.Lens
import Control.Lens.TH
import Data.SafeCopy

data EvalHeader = EvalHeader { _evalHeaderSheetId :: ASSheetId
                             , _evalHeaderLang    :: ASLanguage
                             , _evalHeaderExpr    :: String
                             } 
                             deriving (Show, Read, Eq, Data, Typeable, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

deriveSafeCopy 1 'base ''EvalHeader

asLensedToFromJSON ''EvalHeader
makeLenses   ''EvalHeader