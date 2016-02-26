{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveGeneric #-}

module Types where

-- Basic
import qualified Data.List as L
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe hiding (fromJust)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import GHC.Generics
import Control.Exception
import Control.DeepSeq
import Control.DeepSeq.Generics
import Data.SafeCopy 
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Database.Redis hiding (decode)
import Data.Typeable

-- Our Types
import Prelude()
import AS.Prelude
import AS.Types.Cell
import AS.Types.Bar
import AS.Types.BarProps (BarProp, ASBarProps) 
import AS.Types.Messages
import AS.Types.DB 
import AS.Types.EvalHeader 
import AS.Types.CellProps
import AS.Types.Errors
import AS.Types.Eval
import AS.Types.CondFormat
import AS.Types.Updates
import AS.Types.User
import qualified AS.Serialize as S

-- A raw key is a key from the Redis database, along with its number when doing keys *
type RawKey = (Int, ByteString)

-- A DBSetterObject has metadata needed to set the DB for each key. It has the old
-- encoding of the key, the actual key type, and the newly encoded values associated with 
-- the key.
data DBSetterObject = DBSetterObject {
	origEncoding :: ByteString, 
	setterKey :: DBKey, 
	setterVals :: [ByteString]
}

-- A MigrateError occurs when we can't decode some keys, when we can't decode some values
-- (in which case we report the rawkey, the decoded RedisQuery, and the undecoded values 
-- corresponding to that key), or a RedisError (for example, smembers on a list)
data MigrateError = CouldNotDecodeKeys [RawKey] 
	| CouldNotDecodeValues RawKey DBKey [ByteString]
	| RedisError Reply deriving (Show, Typeable)
	
instance Exception MigrateError








  