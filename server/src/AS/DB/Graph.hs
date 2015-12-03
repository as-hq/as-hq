module AS.DB.Graph where

import Prelude
import qualified Data.List as L
import Data.List.NonEmpty as N (fromList)

import System.ZMQ4.Monadic

import Data.ByteString.Char8 as B 
import Control.Monad (forM)
import qualified Text.Show.ByteString      as BS
import qualified Data.ByteString.Char8     as BC 
import qualified Data.ByteString.Lazy      as BL

import AS.Types.Locations
import AS.Types.DB
import AS.Types.Eval

import AS.Config.Settings as S
import AS.DB.Util
import AS.Logging

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either



------------------------------------------------------------------------------------------------------------------
-- Basic helper functions

-- Use ZMQ to send a message to the graph DB and get the list of ByteStrings as a multi-part reply 
-- Currently using request-reply architecture 
execGraphQuery :: BL.ByteString -> IO [B.ByteString]
execGraphQuery msg = runZMQ $ do
  reqSocket <- socket Req
  connect reqSocket S.graphDbHost
  send' reqSocket [] msg   -- using lazy bytestring send function
  liftIO $ printObj "sent message to graph db" msg
  receiveMulti reqSocket

-- Given the graph reply as a list of ByteStrings, case on the last part (status) and possibly throw an error
processGraphReply :: (Read2 a, Show2 a) => [B.ByteString] -> EitherTExec [a]
processGraphReply reply = case (B.unpack $ L.last reply) of
  "OK" -> do
    let filtered = L.map B.unpack $ L.init reply
    right $ L.map read2 filtered
  "CIRC_DEP" -> do
    let circDepLoc = (read2 (B.unpack $ L.head reply)) :: ASIndex
    left $ CircularDepError circDepLoc
  _ -> do
    left $ UnknownGraphError

------------------------------------------------------------------------------------------------------------------
-- Deal with reading from the graph (getX)

-- Given a read request type (such as GetDescendants) and a list of read inputs (index or range), 
-- produce the message to send the graph, currently a giant lazy bytestring
produceReadMessage :: GraphReadRequest -> [GraphReadInput] -> BL.ByteString
produceReadMessage r input = BS.show $ L.intercalate msgPartDelimiter elements
  where
    elements = (show r):(L.map show2 input)

-- Deal with the entire cycle of a read request. First produce the read message, then execute the query, 
-- then process the reply. 
processReadRequest :: (Read2 a, Show2 a) => GraphReadRequest -> [GraphReadInput] -> EitherTExec [a]
processReadRequest readType input = do 
  let msg = produceReadMessage readType input
  reply <- liftIO $ execGraphQuery msg
  processGraphReply reply

getDescendants :: [GraphReadInput] -> EitherTExec [GraphDescendant]
getDescendants = processReadRequest GetDescendants

-- Given a list of indices, returns descendants as a list of indices
getDescendantsIndices :: [ASIndex] -> EitherTExec [ASIndex]
getDescendantsIndices indices = fmap descendantsToIndices $ getDescendants $ indicesToGraphReadInput indices

getProperDescendantsIndices :: [ASIndex] -> EitherTExec [ASIndex]
getProperDescendantsIndices indices = fmap descendantsToIndices $ getProperDescendants $ indicesToGraphReadInput indices

getProperDescendants :: [GraphReadInput] -> EitherTExec [GraphDescendant]
getProperDescendants = processReadRequest GetProperDescendants

getImmediateAncestors :: [GraphReadInput] -> EitherTExec [GraphAncestor]
getImmediateAncestors = processReadRequest GetImmediateAncestors

-- #incomplete. In the one place this is called, the non-immediate descendants don't cause any
-- problems, so this works as a temporary solution. 
getImmediateDescendants :: [GraphReadInput] -> EitherTExec [GraphDescendant]
getImmediateDescendants = getDescendants

-- ugly and bad
getImmediateDescendantsForced :: [GraphReadInput] -> IO [GraphDescendant]
getImmediateDescendantsForced locs = do 
  e <- runEitherT $ getImmediateDescendants locs
  case e of 
    Left _ -> return []
    Right ancLocs -> return ancLocs 

------------------------------------------------------------------------------------------------------------------
-- Deal with writing to the graph

-- Just send a write-related message to graph, and ignore the response
execGraphWriteQuery :: GraphWriteRequest -> IO ()
execGraphWriteQuery q = runZMQ $ do 
  reqSocket <- socket Req
  connect reqSocket S.graphDbHost
  send reqSocket [] $ BC.pack (show $ show q) -- graph db requires quotes around message
  return ()

-- TODO: should really be returning EitherTExec (), kind of like clear
rollbackGraph :: IO ()
rollbackGraph = execGraphWriteQuery RollbackGraph

-- Takes in a list of (index, [list of ancestors of that cell])'s and sets the ancestor relationship in the graph
-- Note: ASRelation is of type (ASIndex, [ASReference])
setRelations :: [ASRelation] -> EitherTExec ()
setRelations [] = return ()
setRelations rels = do 
  let showRel = \rel -> (show2 (fst rel)):(L.map show2 (snd rel))
  let relStrs = L.map (L.intercalate relationDelimiter . showRel) rels
  let elements = (show SetRelations):relStrs
  let msg = BS.show $ L.intercalate msgPartDelimiter elements
  -- ^ You can have multiple relations sent per "batch message", intercalated with a msgPartDelimiter
  -- ^ Each separate relation is separated by a relationDelimiter
  reply <- liftIO $ execGraphQuery msg
  -- Map Right [] to Right () and keep error messages the same as usual
  bimapEitherT id (\_ -> ()) $ ((processGraphReply reply) :: EitherTExec [ASIndex])
  