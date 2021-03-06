module AS.DB.Graph where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data.List.NonEmpty as N (fromList)
import qualified Data.Text as T (unpack)

import System.ZMQ4.Monadic
import Control.Monad (forM)
import qualified Text.Show.ByteString      as BS
import qualified Data.ByteString.Char8     as BC 
import qualified Data.ByteString.Char8     as B 
import qualified Data.ByteString.Lazy      as BL
import qualified Database.Redis as R

import AS.Prelude
import AS.Types.Cell
import AS.Types.Locations
import AS.Types.Graph
import AS.Types.Eval
import AS.Types.Network
import AS.Types.User
import AS.Types.Commits

import AS.Config.Settings as S
import AS.Logging

import qualified AS.DB.API as DB
import AS.DB.Internal

import AS.Parsing.References (getDependencies)

import Database.Redis (Connection)

----------------------------------------------------------------------------------------------------------------------
-- Ancestors

-- Update the ancestor relationships in the DB based on the expressions and locations of the
-- cells passed in. (E.g. if a cell is passed in at A1 and its expression is "C1 + 1", C1 -> A1 is
-- added to the graph.)
-- Note that a relation is (ASIndex, [ASReference]), where a graph ancestor can be any valid reference type, 
-- including pointer, range, and index. 
setCellsAncestors :: Connection -> UserID -> [ASCell] -> EitherTExec ()
setCellsAncestors conn uid cells = do
  sheets <- liftIO $ DB.getOpenedSheets conn uid
  let depSets = map (getAncestorsForCell sheets) cells
      relations = (zip (mapCellLocation cells) depSets) :: [ASRelation]
  setRelations relations 

-- If a cell is a fat cell head or a normal cell, you can parse its ancestors from the expression. 
-- However, for a non-fat-cell-head coupled cell, we only want to set an edge from it to the head of the list 
-- (for checking circular dependencies).
getAncestorsForCell :: [Sheet] -> ASCell -> [ASReference]
getAncestorsForCell sheets c = if not $ isEvaluable c
  then [IndexRef . keyIndex . fromJust $ c^.cellRangeKey]
  else getDependencies (c^.cellLocation.locSheetId) sheets (c^.cellExpression)

-- | It'll parse no dependencies from the blank cells at these locations, so each location in the
-- graph DB gets all its ancestors removed. 
removeAncestorsAt :: Connection -> [ASIndex] -> EitherTExec ()
removeAncestorsAt conn = setCellsAncestors conn "" . blankCellsAt

-- | Should only be called when undoing or redoing commits, which should be guaranteed to not
-- introduce errors. 
setCellsAncestorsForce :: Connection -> UserID -> [ASCell] -> IO ()
setCellsAncestorsForce conn uid cells = runEitherT_ (setCellsAncestors conn uid cells) 

removeAncestorsAtForced :: Connection -> [ASIndex] -> IO ()
removeAncestorsAtForced conn locs = runEitherT_ (removeAncestorsAt conn locs) 

------------------------------------------------------------------------------------------------------------------
-- Basic helper functions

-- Use ZMQ to send a message to the graph DB and get the list of ByteStrings as a multi-part reply 
-- Currently using request-reply architecture 
execGraphQuery :: BL.ByteString -> IO [B.ByteString]
execGraphQuery msg = do
  addr <- S.getSetting S.graphAddress
  runZMQ $ do
    reqSocket <- socket Req
    connect reqSocket addr
    send' reqSocket [] msg  -- using lazy bytestring send function
    receiveMulti reqSocket

-- Given the graph reply as a list of ByteStrings, case on the last part (status) and possibly throw an error
processGraphReply :: (Read2 a, Show2 a) => [B.ByteString] -> EitherTExec [a]
processGraphReply reply = case B.unpack $ last reply of
  "OK" -> do
    let filtered = map B.unpack $ init reply
    right $ map read2 filtered
  "CIRC_DEP" -> do
    let circDepLoc = (read2 (B.unpack $ head reply)) :: ASIndex
    left $ CircularDepError circDepLoc
  _ -> do
    left UnknownGraphError

------------------------------------------------------------------------------------------------------------------
-- Deal with reading from the graph (getX)

-- Given a read request type (such as GetDescendants) and a list of read inputs (index or range), 
-- produce the message to send the graph, currently a giant lazy bytestring
produceReadMessage :: GraphReadRequest -> BL.ByteString
produceReadMessage req = BS.show $ intercalate msgPartDelimiter elements
  where 
    elements = case req of -- #needsrefactor Data.Data makes this cleaner, apparently
                 GetDescendants inputs          -> "GetDescendants":(map show2 inputs)
                 GetImmediateDescendants inputs -> "GetImmediateDescendants":(map show2 inputs)
                 GetProperDescendants inputs    -> "GetProperDescendants":(map show2 inputs)
                 GetImmediateAncestors inputs   -> "GetImmediateAncestors":(map show2 inputs)
                 GetAllAncestors inputs         -> "GetAllAncestors":(map show2 inputs)

-- Deal with the entire cycle of a read request. First produce the read message, then execute the query, 
-- then process the reply. 
processReadRequest :: (Read2 a, Show2 a) => GraphReadRequest -> EitherTExec [a]
processReadRequest readRequest = do 
  let msg = produceReadMessage readRequest
  reply <- liftIO $ execGraphQuery msg
  processGraphReply reply

getDescendants :: [AncestryRequestInput] -> EitherTExec [GraphDescendant]
getDescendants = processReadRequest . GetDescendants

-- Given a list of indices, returns descendants as a list of indices
getDescendantsIndices :: [ASIndex] -> EitherTExec [ASIndex]
getDescendantsIndices indices = fmap descendantsToIndices $ getDescendants $ indicesToAncestryRequestInput indices

getProperDescendantsIndices :: [ASIndex] -> EitherTExec [ASIndex]
getProperDescendantsIndices indices = fmap descendantsToIndices $ getProperDescendants $ indicesToAncestryRequestInput indices

getProperDescendants :: [AncestryRequestInput] -> EitherTExec [GraphDescendant]
getProperDescendants = processReadRequest . GetProperDescendants

getImmediateAncestors :: [AncestryRequestInput] -> EitherTExec [GraphAncestor]
getImmediateAncestors = processReadRequest . GetImmediateAncestors

getAllAncestors :: [AncestryRequestInput] -> EitherTExec [ASReference]
getAllAncestors = processReadRequest . GetAllAncestors

-- #incomplete. In the one place this is called, the non-immediate descendants don't cause any
-- problems, so this works as a temporary solution. 
getImmediateDescendants :: [AncestryRequestInput] -> EitherTExec [GraphDescendant]
getImmediateDescendants = getDescendants

-- ugly and bad
getImmediateDescendantsForced :: [ASIndex] -> IO [ASIndex]
getImmediateDescendantsForced locs = do 
  e <- runEitherT $ getImmediateDescendants (indicesToAncestryRequestInput locs)
  case e of 
    Left _ -> return []
    Right ancLocs -> return (descendantsToIndices ancLocs)

------------------------------------------------------------------------------------------------------------------
-- Deal with writing to the graph

produceWriteMessage :: GraphWriteRequest -> BL.ByteString
produceWriteMessage req = BS.show $ intercalate msgPartDelimiter elements
  where 
    elements = case req of 
                 SetRelations rels -> "SetRelations":(map (intercalate relationDelimiter . showRel) rels)
                   where showRel rel = (show2 (fst rel)):(map show2 (snd rel))
                 ClearAllDAGs -> ["Clear"]
                 ClearSheetDAG sid -> ["ClearSheet", T.unpack sid]

-- Deal with the entire cycle of a read request. First produce the read message, then execute the query, 
-- then process the reply. 
processWriteRequest :: GraphWriteRequest -> EitherTExec ()
processWriteRequest writeRequest = do 
  let msg = produceWriteMessage writeRequest
  reply <- liftIO $ execGraphQuery msg
  processGraphReply reply :: EitherTExec [ASIndex] 
  -- ^ kind of a hack; the list *should* be empty, so it really doesn't matter what 
  -- type it gets read as, so we're just passing it an arbitrary type. 
  return ()

-- Just send a write-related message to graph, and ignore the response
execGraphWriteQuery :: GraphWriteRequest -> IO ()
execGraphWriteQuery q = runZMQ $ do 
  addr <- liftIO $ S.getSetting S.graphAddress
  reqSocket <- socket Req
  connect reqSocket addr
  send reqSocket [] $ BC.pack (show $ show q) -- graph db requires quotes around message
  return ()

shouldSetRelationsOfCellWhenRecomputing :: ASCell -> Bool 
shouldSetRelationsOfCellWhenRecomputing cell =  maybe True ((== cell^.cellLocation) . keyIndex) $ cell^.cellRangeKey

recomputeAllDAGs :: R.Connection -> IO ()
recomputeAllDAGs conn = do
  sheets <- DB.getAllSheets conn
  forM_ sheets $ \s -> do
    let src = CommitSource (s^.sheetId) (s^.sheetOwner)
    recomputeSheetDAG conn src

recomputeSheetDAG :: R.Connection -> CommitSource -> IO ()
recomputeSheetDAG conn src = do
  let sid = srcSheetId src
  let uid = srcUserId src
  cells <- filter shouldSetRelationsOfCellWhenRecomputing <$> DB.getCellsInSheet conn sid
  clearSheetDAG sid
  setCellsAncestorsForce conn uid cells
  -- Note that clearSheetDAG is usually redundant with setCellsAncestorsForce, since usually
  -- what was previously in the DAG is a subset of all cells in the sheet. There's no guarantee
  -- that's the case though, so it's safe to add it here anyway. 

-- #needsrefactor the fact that this is IO () and not EitherTExec () concerns me. 
clearAllDAGs :: IO ()
clearAllDAGs = void . runEitherT $ processWriteRequest ClearAllDAGs

-- #needsrefactor the fact that this is IO () and not EitherTExec () concerns me. 
clearSheetDAG :: SheetID -> IO ()
clearSheetDAG sid = void . runEitherT $ processWriteRequest (ClearSheetDAG sid)

-- Takes in a list of (index, [list of ancestors of that cell])'s and sets the ancestor relationship in the graph
-- Note: ASRelation is of type (ASIndex, [ASReference])
-- #needsrefactor OutOfBounds should return a cell-level error, not throw a left that halts evaluation
setRelations :: [ASRelation] -> EitherTExec ()
setRelations [] = return ()
setRelations rels = if any isOutOfBounds rels 
  then left ReferenceOutOfBounds
  else processWriteRequest $ SetRelations rels
  where 
    isOutOfBounds (_, refs) = any (== OutOfBounds) refs
  -- Map Right [] to Right () and keep error messages the same as usual
