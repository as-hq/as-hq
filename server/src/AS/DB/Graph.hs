module AS.DB.Graph where

import Prelude
import qualified Data.List as L
import Data.List.NonEmpty as N (fromList)

import System.ZMQ4.Monadic

import Control.Monad (forM)
import qualified Text.Show.ByteString      as BS
import qualified Data.ByteString.Char8     as BC 
import qualified Data.ByteString.Char8     as B 
import qualified Data.ByteString.Lazy      as BL
import qualified Database.Redis as R

import AS.Types.Cell
import AS.Types.Locations
import AS.Types.DB
import AS.Types.Eval

import AS.Config.Settings as S
import qualified AS.DB.API as DB
import AS.DB.Internal
import AS.Logging
import AS.Parsing.Substitutions (getDependencies)

import Data.Maybe
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

----------------------------------------------------------------------------------------------------------------------
-- Ancestors

-- Update the ancestor relationships in the DB based on the expressions and locations of the
-- cells passed in. (E.g. if a cell is passed in at A1 and its expression is "C1 + 1", C1 -> A1 is
-- added to the graph.)
-- Note that a relation is (ASIndex, [ASReference]), where a graph ancestor can be any valid reference type, 
-- including pointer, range, and index. 
setCellsAncestors :: [ASCell] -> EitherTExec ()
setCellsAncestors cells = do
  setRelations relations 
  printObjT "Set cell ancestors" relations
  where
    depSets = map getAncestorsForCell cells
    relations = (zip (mapCellLocation cells) depSets) :: [ASRelation]

-- If a cell is a fat cell head or a normal cell, you can parse its ancestors from the expression. However, for a non-fat-cell-head coupled
-- cell, we only want to set an edge from it to the head of the list (for checking circular dependencies).
getAncestorsForCell :: ASCell -> [ASReference]
getAncestorsForCell c = if not $ isEvaluable c
  then [IndexRef . keyIndex . fromJust $ c^.cellRangeKey]
  else getDependencies (locSheetId $ c^.cellLocation) (c^.cellExpression)

-- | It'll parse no dependencies from the blank cells at these locations, so each location in the
-- graph DB gets all its ancestors removed. 
removeAncestorsAt :: [ASIndex] -> EitherTExec ()
removeAncestorsAt = setCellsAncestors . blankCellsAt

-- | Should only be called when undoing or redoing commits, which should be guaranteed to not
-- introduce errors. 
setCellsAncestorsForce :: [ASCell] -> IO ()
setCellsAncestorsForce cells = runEitherT (setCellsAncestors cells) >> return ()

removeAncestorsAtForced :: [ASIndex] -> IO ()
removeAncestorsAtForced locs = runEitherT (removeAncestorsAt locs) >> return ()


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
  where elements = (show r):(L.map show2 input)

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
getImmediateDescendantsForced :: [ASIndex] -> IO [ASIndex]
getImmediateDescendantsForced locs = do 
  e <- runEitherT $ getImmediateDescendants (indicesToGraphReadInput locs)
  case e of 
    Left _ -> return []
    Right ancLocs -> return (descendantsToIndices ancLocs)

------------------------------------------------------------------------------------------------------------------
-- Deal with writing to the graph

-- Just send a write-related message to graph, and ignore the response
execGraphWriteQuery :: GraphWriteRequest -> IO ()
execGraphWriteQuery q = runZMQ $ do 
  reqSocket <- socket Req
  connect reqSocket S.graphDbHost
  send reqSocket [] $ BC.pack (show $ show q) -- graph db requires quotes around message
  return ()

shouldSetRelationsOfCellWhenRecomputing :: ASCell -> Bool 
shouldSetRelationsOfCellWhenRecomputing cell = case cell^.cellRangeKey of 
  Just (RangeKey idx _) -> cell^.cellLocation == idx -- should be a fat cell head
  Nothing -> True -- keep normal cells

recompute :: R.Connection -> IO ()
recompute conn = do
  cells <- filter shouldSetRelationsOfCellWhenRecomputing <$> DB.getAllCells conn
  clear
  setCellsAncestorsForce cells
  return ()

clear = execGraphWriteQuery Clear

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
  
