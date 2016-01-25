module AS.Window
  ( ASWindow(..)
  , intersectViewingWindow
  , getScrolledLocs
  ) where

import AS.Prelude
import Prelude()

import AS.Types.Locations
import AS.Types.Cell

import GHC.Generics
import Data.Aeson
import Data.SafeCopy
import Control.Lens hiding ((.=))

data ASWindow = Window {windowSheetId :: ASSheetId, topLeft :: Coord, bottomRight :: Coord} 
                deriving (Show, Read, Eq, Data, Typeable, Generic)

----------------------------------------------------------------------------------------------------------------------------------------------
-- Instances

instance ToJSON ASWindow where
  toJSON (Window sid coord1 coord2) = object ["tag" .= ("window" :: String),
                                              "sheetId" .= sid,
                                              "range" .= object [ 
                                                 "tl" .= object [ "row"  .= r, 
                                                                  "col"  .= c],
                                                 "br" .= object [ "row"  .= r2, 
                                                                  "col"  .= c2]]]
                                                  where c  = view col coord1 
                                                        r  = view row coord1
                                                        c2 = view col coord2
                                                        r2 = view row coord2

instance FromJSON ASWindow where
  parseJSON (Object v) = do
    rng <- v .: "window" 
    (tl, br) <- (,) <$> rng .: "tl" <*> rng .: "br"
    tl' <- Coord <$> tl .: "col" <*> tl .: "row"
    br' <- Coord <$> br .: "col" <*> br .: "row"
    sid <- v .: "sheetId"
    return $ Window sid tl' br'
  parseJSON _          = fail "client message JSON attributes missing"

deriveSafeCopy 1 'base ''ASWindow

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------------------------------------------------

inRange :: Int -> Int -> Int -> Bool
inRange x start len = ((x >= start) && (x <= (start + len)))

inViewingWindow :: ASWindow -> ASIndex -> Bool
inViewingWindow (Window wSheetId tl br) (Index cSheetId indexCoord) = and [ 
  wSheetId == cSheetId,
  inRange (view col indexCoord) tlc (brc-tlc),
  inRange (view row indexCoord) tlr (brr-tlr)]
    where tlc = view col tl
          tlr = view row tl
          brc = view col br
          brr = view row br

intersectViewingWindow :: [ASCell] -> ASWindow -> [ASCell]
intersectViewingWindow cells vw = filter ((inViewingWindow vw) . view cellLocation) cells

-- | Computes set difference of window 2 minus window 1. Represented as a union of ASRange's. 
getScrolledLocs :: ASWindow -> ASWindow -> [ASRange]
getScrolledLocs (Window _ (Coord (-1) (-1)) (Coord (-1) (-1))) (Window sheetid tl br) = [(Range sheetid (tl, br))]
getScrolledLocs w1@(Window _ tl br) w2@(Window sheetid tl' br')
  | windowsIntersect w1 w2 = getUncoveredLocs sheetid overlapping (tl', br')
  | otherwise = [(Range sheetid (tl', br'))]
    where 
      y  = view col tl ; x  = view row tl ; y2  = view col br ; x2  = view row br
      y' = view col tl'; x' = view row tl'; y2' = view col br'; x2' = view row br'
      overlapping = (Coord (max y y') (max x x'), Coord (min y2 y2')  (min x2 x2'))
      windowsIntersect (Window _ (Coord y x) (Coord y2 x2)) (Window _ (Coord y' x') (Coord y2' x2'))
        | y2 > y' = False 
        | y < y2' = False
        | x2 < x' = False 
        | x > x2' = False
        | otherwise = True 
            
getUncoveredLocs :: ASSheetId -> (Coord, Coord) -> (Coord, Coord) -> [ASRange]
getUncoveredLocs sheet (tlo, bro) (tlw, brw) = [Range sheet corners | corners <- cs]
    where
      trw = (Coord (view col brw) (view row tlw))
      blw = (Coord (view col tlw) (view row brw))
      tro = (Coord (view col bro) (view row tlo))
      blo = (Coord (view col tlo) (view row bro))
      cs = [(tlw, tro), (trw, bro), (brw, blo), (blw, tlo)]
