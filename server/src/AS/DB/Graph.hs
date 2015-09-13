module AS.DB.Graph where

import Prelude
import System.ZMQ4.Monadic
import Data.ByteString.Char8 as B(pack, unpack)
import Data.List.NonEmpty as N (fromList)
import Control.Monad (forM)
import Data.List

import AS.Types
import AS.Config.Settings as S
import AS.DB.Util as DBU
import AS.Util

getDescendants :: [ASLocation] -> IO (Either ASExecError [ASLocation])
getDescendants = query GetDescendants

getImmediateAncestors :: [ASLocation] -> IO (Either ASExecError [ASLocation])
getImmediateAncestors = query GetImmediateAncestors

setRelations :: [(ASLocation, [ASLocation])] -> IO (Either ASExecError ())
setRelations rels = 
    let 
        flatRels = map (\(root, deps)-> (root:deps)) rels
    in do
        results <- queryMulti SetRelations flatRels 
        return $ case results of
            (Right _) -> Right ()
            (Left e) -> Left DBGraphUnreachable

query :: GraphQuery -> [ASLocation] -> IO (Either ASExecError [ASLocation])
query q locs = runZMQ $ do
    liftIO $ printTimed "Connecting to graph database."  
    reqSocket <- socket Req
    connect reqSocket S.graphDbHost
    let msg = (:) (B.pack . show $ q) (map (B.pack . show) locs)
    sendMulti reqSocket $ N.fromList msg 
    liftIO $ printTimed "sent message"  
    reply <- receiveMulti reqSocket
    --liftIO $ printTimed $ "graph db reply:  " ++ (show reply)
    case (B.unpack $ last reply) of
        "OK" -> do
            let filtered = filter ((/=) "|") $ map B.unpack $ init reply
            let result = Right $ map (\l -> read l:: ASLocation) filtered
            --liftIO $ printTimed $ "Graph DB result: " ++ (show $ init reply)
            return result
        "ERROR" -> do
            liftIO $ printTimed "Graph DB error"
            return $ Left DBGraphUnreachable

queryMulti :: GraphQuery -> [[ASLocation]] -> IO (Either ASExecError [ASLocation])
queryMulti q locSets = runZMQ $ do
    liftIO $ printTimed "Connecting to graph database for multi query."  
    reqSocket <- socket Req
    connect reqSocket S.graphDbHost
    let locs = map (\l -> map (B.pack . show) l) locSets
    liftIO $ printTimed "building message"  
    let msg = (:) (B.pack . show $ q) $! intercalate [(B.pack "|")] locs
    liftIO $ printTimed "built message"  
    --liftIO $ printTimed $ "sending message: " ++ (show msg)
    _ <- sendMulti reqSocket $ N.fromList msg
    liftIO $ printTimed "sent message"  
    reply <- receiveMulti reqSocket
    liftIO $ printTimed $ "received message of length: " ++ (show . length $ reply)  
    --liftIO $ printTimed $ "graph db reply multi: " ++ (show reply)
    --liftIO $ printTimed $ "query type: " ++ (show q)
    return $ case (B.unpack $ last reply) of
        "OK" -> Right []
        "ERROR" -> Left DBGraphUnreachable

--queryMulti :: GraphQuery -> [[ASLocation]] -> IO (Either ASExecError [ASLocation])
--queryMulti q locSets = do
--    let locs = map (\l -> map (B.pack . show) l) locSets
--    liftIO $ printTimed "building message"  
--    let msg = (:) (B.pack . show $ q) $! intercalate [(B.pack "|")] locs
--    liftIO $ printTimed "built message"  
--    liftIO $ printTimed "sent message"  
--    liftIO $ printTimed $ "received message of length: "   
--    --liftIO $ printTimed $ "graph db reply multi: " ++ (show reply)
--    --liftIO $ printTimed $ "query type: " ++ (show q)
--    let reply = ["OK"]
--    return $ case (B.unpack $ last reply) of
--        "OK" -> Right []
--        "ERROR" -> Left DBGraphUnreachable
