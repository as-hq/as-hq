module DB where

import AS.Prelude hiding (set, get)

import Control.Exception
import Database.Redis
import Data.ByteString (ByteString)

import Types (MigrateError(..))

-------------------------------------------------------------------------------------
-- Helpers

-- Throw an error if we get a Redis error (Left _)
handleErr :: Redis (Either Reply a) -> Redis a
handleErr r = r >>= f
  where
    f (Left e) = throw $ RedisError e
    f (Right x) = return x

-------------------------------------------------------------------------------------
-- Getters and setters

-- A Getter gets all of the values associated with a key in Redis. 
-- A Setter sets a key with a bunch of values in the DB. 

-- We have getters and setters for key -> value, key -> list, and key -> set

type Getter = Connection -> ByteString -> IO [ByteString]
type Setter = Connection -> ByteString -> [ByteString] -> IO ()

getVRaw :: Getter
getVRaw conn bs = runRedis conn $ do 
  v <- handleErr $ get bs 
  case v of 
    Nothing -> return []
    Just x -> return [x]

setVRaw :: Setter
setVRaw conn bs lbs = void $ runRedis conn $ mapM_ (set bs) lbs

getLRaw :: Getter
getLRaw conn bs = runRedis conn $ handleErr $ lrange bs 0 (-1)

setLRaw :: Setter
setLRaw conn bs lbs = void $ runRedis conn $ handleErr $ rpush bs lbs

getSRaw :: Getter
getSRaw conn bs = runRedis conn $ handleErr $ smembers bs 

setSRaw :: Setter
setSRaw conn bs lbs = void $ runRedis conn $ handleErr $ sadd bs lbs

vPair :: (Getter, Setter)
vPair = (getVRaw, setVRaw)

lPair :: (Getter, Setter)
lPair = (getLRaw, setLRaw)

sPair :: (Getter, Setter)
sPair = (getSRaw, setSRaw)