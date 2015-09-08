module AS.DB.Graph where

import Prelude
import System.ZMQ4.Monadic
import Data.ByteString.Char8 as B(pack, unpack)
import Data.List.NonEmpty as N (fromList)
import Control.Monad (forM)

import AS.Types
import AS.Config.Settings as S
import AS.DB.Util as DBU
import AS.Util

getDescendants :: [ASLocation] -> IO (Either ASExecError [ASLocation])
getDescendants = query GetDescendants

setRelations :: [(ASLocation, [ASLocation])] -> IO (Either ASExecError ())
setRelations rels = queryMulti queries >>= (\result -> case result of 
    ((Left e):[]) -> return (Left e)
    ((Right _):[]) -> return (Right ()))
    where
        queries = map (\r -> (SetRelation, r)) flatRels
        flatRels = map (\(root, deps)-> (root:deps)) rels

query :: GraphQuery -> [ASLocation] -> IO (Either ASExecError [ASLocation])
query q locs = runZMQ $ do
    liftIO $ printTimed "Connecting to graph database."  
    reqSocket <- socket Req
    connect reqSocket S.graphDbHost
    let msg = (:) (B.pack . show $ q) (map (B.pack . show) locs)
    sendMulti reqSocket $ N.fromList msg 
    (status:reply) <- receiveMulti reqSocket
    return $ case (B.unpack status) of
        "OK" -> Right $ map (\l -> read (B.unpack l) :: ASLocation) reply 
        "ERROR" -> Left DBGraphUnreachable

queryMulti :: [(GraphQuery, [ASLocation])] -> IO [Either ASExecError [ASLocation]]
queryMulti querySets = runZMQ $ do
    liftIO $ printTimed "Connecting to graph database."  
    reqSocket <- socket Req
    connect reqSocket S.graphDbHost
    results <- forM querySets $ \(q, locs) -> do
        let msg = (:) (B.pack . show $ q) (map (B.pack . show) locs)
        sendMulti reqSocket $ N.fromList msg
        (status:reply) <- receiveMulti reqSocket
        return $ case (B.unpack status) of
            "OK" -> case q of 
                SetRelation -> Right []
            "ERROR" -> Left DBGraphUnreachable
    return results