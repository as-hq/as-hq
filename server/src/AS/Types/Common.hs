module AS.Types.Common where

import Data.Text
import Data.Serialize as DS

import GHC.Generics

instance Serialize Text where
  put = put . unpack
  get = pack <$> get