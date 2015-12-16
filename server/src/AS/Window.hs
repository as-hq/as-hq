{-# LANGUAGE DeriveGeneric #-}

module AS.Window
  ( ASWindow(..)
  , intersectViewingWindow
  , intersectViewingWindowLocs
  , getScrolledLocs
  ) where

import AS.Types.Locations
import AS.Types.Cell

import GHC.Generics
import Data.Aeson
import Data.Serialize (Serialize)

data ASWindow = Window {windowSheetId :: ASSheetId, topLeft :: Coord, bottomRight :: Coord} deriving (Show,Read,Eq,Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

instance ToJSON ASWindow where
  toJSON (Window sid (c,r) (c2, r2)) = object ["tag" .= ("window" :: String),
                                               "sheetId" .= sid,
                                               "range" .= object [ 
                                                  "tl" .= object [ "row"  .= r, 
                                                                   "col"  .= c],
                                                  "br" .= object [ "row"  .= r2, 
                                                                   "col"  .= c2]]]
instance FromJSON ASWindow where
  parseJSON (Object v) = do
    rng <- v .: "window" 
    (tl, br) <- (,) <$> rng .: "tl" <*> rng .: "br"
    tl' <- (,) <$> tl .: "col" <*> tl .: "row"
    br' <- (,) <$> br .: "col" <*> br .: "row"
    sid <- v .: "sheetId"
    return $ Window sid tl' br'
  parseJSON _          = fail "client message JSON attributes missing"

instance Serialize ASWindow

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------------------------------------------------

inRange :: Int -> Int -> Int -> Bool
inRange x start len = ((x >= start) && (x <= (start + len)))

inViewingWindow :: ASWindow -> ASIndex -> Bool
inViewingWindow (Window wSheetId (tlc, tlr) (brc, brr)) (Index cSheetId (col,row)) = and [ 
  wSheetId == cSheetId,
  inRange col tlc (brc-tlc),
  inRange row tlr (brr-tlr)]

intersectViewingWindow :: [ASCell] -> ASWindow -> [ASCell]
intersectViewingWindow cells vw = filter ((inViewingWindow vw) . cellLocation) cells

-- new function, so that we don't have to do the extra filter/lookup by using just one
intersectViewingWindowLocs :: [ASIndex] -> ASWindow -> [ASIndex]
intersectViewingWindowLocs locs vw = filter (inViewingWindow vw) locs

-- | Computes set difference of window 2 minus window 1. Represented as a union of ASRange's. 
getScrolledLocs :: ASWindow -> ASWindow -> [ASRange]
getScrolledLocs (Window _ (-1,-1) (-1,-1)) (Window sheetid tl br) = [(Range sheetid (tl, br))]
getScrolledLocs w1@(Window _ (y,x) (y2,x2)) w2@(Window sheetid tl@(y',x') br@(y2',x2'))
  | windowsIntersect w1 w2 = getUncoveredLocs sheetid overlapping (tl, br)
  | otherwise = [(Range sheetid (tl, br))]
    where 
      overlapping = ((max y y', max x x'), (min y2 y2', min x2 x2'))
      windowsIntersect (Window _ (y,x) (y2,x2)) (Window _ (y',x') (y2',x2'))
        | y2 > y' = False 
        | y < y2' = False
        | x2 < x' = False 
        | x > x2' = False
        | otherwise = True 
            
getUncoveredLocs :: ASSheetId -> (Coord, Coord) -> (Coord, Coord) -> [ASRange]
getUncoveredLocs sheet (tlo, bro) (tlw, brw) = [Range sheet corners | corners <- cs]
    where
      trw = (col brw, row tlw)
      blw = (col tlw, row brw)
      tro = (col bro, row tlo)
      blo = (col tlo, row bro)
      cs = [(tlw, tro), (trw, bro), (brw, blo), (blw, tlo)]