module AS.DB.Graph where

import Prelude
import System.ZMQ4.Monadic
import Data.ByteString.Char8 as B(pack, unpack)
import Data.List.NonEmpty as N (fromList)
import Control.Monad (forM)
import Data.List
import qualified Text.Show.ByteString          as BS

import AS.Types.Core
import AS.Types.DB
import AS.Config.Settings as S
import AS.DB.Util 
import AS.Util

getDescendants :: [ASIndex] -> IO (Either ASExecError [ASIndex])
getDescendants = query GetDescendants

getImmediateAncestors :: [ASIndex] -> IO (Either ASExecError [ASIndex])
getImmediateAncestors = query GetImmediateAncestors

query :: GraphQuery -> [ASIndex] -> IO (Either ASExecError [ASIndex])
query q locs = 
    let
        elements = (show q):(map show2 locs)
        msg = BS.show $ intercalate msgPartDelimiter elements

    in runZMQ $ do
        liftIO $ printTimed "Connecting to graph database."  
        reqSocket <- socket Req
        connect reqSocket S.graphDbHost

        send' reqSocket [] msg   -- using lazy bytestring send function
        liftIO $ printTimed "sent message to graph db"  
        reply <- receiveMulti reqSocket
        -- liftIO $ printTimed $ "graph db reply:  " ++ (show reply)
        case (B.unpack $ last reply) of
            "OK" -> do
                let filtered = map B.unpack $ init reply
                let result = Right $ map read2 filtered
                return result
            "ERROR" -> do
                liftIO $ printTimed "Graph DB error"
                return $ Left DBGraphUnreachable

setRelations :: [(ASIndex, [ASIndex])] -> IO (Either ASExecError ())
setRelations rels = 
    let
        locSets = map (\(root, deps)-> (root:deps)) rels
        relations = map (\lset -> intercalate relationDelimiter $ map show2 lset) locSets
        elements = (show SetRelations):relations
        msg = BS.show $ intercalate msgPartDelimiter elements

    in runZMQ $ do
        liftIO $ printTimed "Connecting to graph database for multi query."  
        reqSocket <- socket Req
        connect reqSocket S.graphDbHost

        --liftIO $ printTimed $ "sending message: " ++ (show msg)

        send' reqSocket [] msg
        liftIO $ printTimed "sent message"  

        reply <- receiveMulti reqSocket
        liftIO $ printTimed $ "received message of length: " ++ (show . length $ reply)  
        --liftIO $ printTimed $ "graph db reply multi: " ++ (show reply)
        --liftIO $ printTimed $ "query type: " ++ (show q)
        return $ case (B.unpack $ last reply) of
            "OK" -> Right ()
            "ERROR" -> Left DBGraphUnreachable
