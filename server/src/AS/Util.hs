{-# LANGUAGE TemplateHaskell #-}
module AS.Util where

import AS.Types.Cell
import AS.Types.Network
import AS.Types.CellProps (emptyProps)
import AS.Logging

import Prelude

import qualified Data.Map as M
import Data.List (foldl', find)
import Data.Maybe (isJust)

import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import qualified Network.WebSockets as WS
import Data.Aeson
import Debug.Trace 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Language.Haskell.TH

-------------------------------------------------------------------------------------------------------------------------
-- For debugging purposes only 

trace' :: (Show a) => String -> a -> a
trace' s x = trace ("\n\n\n" ++ s ++ "\n" ++ (show x) ++ "\n\n\n") x

err :: Q Exp
err = locatedError =<< location

locatedError :: Loc -> Q Exp
locatedError loc = do
    let prefix = formatLoc loc ++ " ----> "
    [| (\msg -> error ("Error at " ++ $(litE $ stringL prefix) ++ msg)) |]

formatLoc :: Loc -> String
formatLoc loc = let file = loc_filename loc
                    (line, col) = loc_start loc
                in concat [file, ":", show line, ":", show col]

-- takes a cell transform to convert the test cell to something desired
testCell :: ASCell
testCell = Cell (Index "" (1,1)) (Expression "=1+1" Excel) NoValue emptyProps Nothing

--------------------------------------------------------------------------------------------------------------
-- Misc

sendMessage :: (ToJSON a, Show a) => a -> WS.Connection -> IO ()
sendMessage msg conn = do
  WS.sendTextData conn (encode msg)
  printObj "Server sent message" msg

-- | Generates a random number
getUniqueId :: IO String
getUniqueId = return . toString =<< nextRandom

-- Inserts multiple elements into a map
insertMultiple :: (Ord k) => M.Map k v -> [k] -> [v] -> M.Map k v
insertMultiple mp keys values = foldl' (\curMap (key,value) -> M.insert key value curMap) mp assoc
  where assoc = zip keys values

filterBy :: (a -> b) -> (b -> Bool) -> [a] -> [a]
filterBy f filt l = map snd $ filter (\(fe, _) -> filt fe) $ zip (map f l) l

fromRight :: Either a b -> b
fromRight (Right a) = a

nub' :: (Eq a, Ord a) => [a] -> [a]
nub' xs = map fst $ M.toList . M.fromList $ zip xs (repeat ())

(<++>) a b = (++) <$> a <*> b
