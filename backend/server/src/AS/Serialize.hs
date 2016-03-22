module AS.Serialize where

import AS.Prelude

import Data.SafeCopy
import Data.Serialize.Put (runPut, runPutLazy)
import Data.Serialize.Get (runGet, runGetLazy)

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

decode :: (SafeCopy a) => B.ByteString -> Either String a
decode = runGet safeGet

decodeLazy :: (SafeCopy a) => BL.ByteString -> Either String a
decodeLazy = runGetLazy safeGet

maybeDecode :: (SafeCopy a) => B.ByteString -> Maybe a
maybeDecode b = either (const Nothing) Just (decode b)

encode :: (SafeCopy a) => a -> B.ByteString
encode = runPut . safePut

encodeLazy :: (SafeCopy a) => a -> BL.ByteString
encodeLazy = runPutLazy . safePut