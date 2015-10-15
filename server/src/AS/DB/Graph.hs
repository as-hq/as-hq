module AS.DB.Graph where

import Prelude
import System.ZMQ4.Monadic
import Data.ByteString.Char8 as B(pack, unpack)
import Data.List.NonEmpty as N (fromList)
import Control.Monad (forM)
import Data.List
import qualified Text.Show.ByteString          as BS
import qualified Data.ByteString.Char8         as BC

import AS.Types.Core
import AS.Types.DB
import AS.Config.Settings as S
import AS.DB.Util 
import AS.Util

-- EitherT
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

getDescendants :: [ASIndex] -> EitherTExec [ASIndex]
getDescendants = query GetDescendants

getImmediateAncestors :: [ASIndex] -> EitherTExec [ASIndex]
getImmediateAncestors = query GetImmediateAncestors

-- TODO: should really be returning EitherTExec (), kind of like clear. 
rollbackGraph :: EitherTExec [ASIndex]
rollbackGraph = query RollbackGraph []

query :: GraphQuery -> [ASIndex] -> EitherTExec [ASIndex]
query q locs = 
    let
        elements = (show q):(map show2 locs)
        msg = BS.show $ intercalate msgPartDelimiter elements
    in EitherT $ runZMQ $ do
        liftIO $ printWithTime $ "graph query:  " -- ++ (show elements)
        reqSocket <- socket Req
        connect reqSocket S.graphDbHost
        send' reqSocket [] msg   -- using lazy bytestring send function
        liftIO $ printWithTime "sent message to graph db"  
        reply <- receiveMulti reqSocket
        case (B.unpack $ last reply) of
            "OK" -> do
                let filtered = map B.unpack $ init reply
                let result = Right $ map read2 filtered
                return $ result
            "CIRC_DEP" -> do
                let circDepLoc = read2 $ B.unpack $ head reply
                return $ Left $ CircularDepError circDepLoc
            _ -> do 
                return $ Left DBGraphUnreachable

-- | Takes in a list of (cell, [list of ancestors of that cell])'s and sets the ancestor relationship in the DB. 
-- (ASRelations is of type [(ASIndex, [ASIndex])]).
setRelations :: ASRelations -> EitherTExec ()
setRelations rels = 
    let
        locSets = map (\(root, deps)-> (root:deps)) rels
        relations = map (\lset -> intercalate relationDelimiter $ map show2 lset) locSets
        elements = (show SetRelations):relations
        msg = BS.show $ intercalate msgPartDelimiter elements
    in EitherT $ runZMQ $ do
        liftIO $ printWithTime "Connecting to graph database for multi query."  
        reqSocket <- socket Req
        connect reqSocket S.graphDbHost
        send' reqSocket [] msg
        liftIO $ printWithTime "sent message"  
        reply <- receiveMulti reqSocket
        liftIO $ printWithTime $ "received message of length: " ++ (show . length $ reply)  
        --liftIO $ printWithTime $ "graph db reply multi: " ++ (show reply)
        --liftIO $ printWithTime $ "query type: " ++ (show q)
        case (B.unpack $ last reply) of
            "OK" -> return $ Right ()
            "CIRC_DEP" -> do
                let circDepLoc = read2 $ B.unpack $ head reply
                return $ Left $ CircularDepError circDepLoc
            _ -> do 
                return $ Left DBGraphUnreachable

clear :: IO ()
clear = runZMQ $ do
    reqSocket <- socket Req
    connect reqSocket S.graphDbHost
    send reqSocket [] $ BC.pack "\"Clear\""
    return ()