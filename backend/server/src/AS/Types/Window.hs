module AS.Types.Window where

import AS.Prelude

import AS.Types.Locations
import AS.Types.Cell

import Data.Aeson
import Data.SafeCopy

data Window = Window 
  { _windowSheetId :: SheetID 
  , _windowWorkbookId :: WorkbookID
  , _topLeft :: Coord
  , _bottomRight :: Coord
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

makeLenses ''Window
deriveSafeCopy 1 'base ''Window

----------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------------------------------------------------

inRange :: (Num a, Ord a) => a -> a -> a -> Bool
inRange x start len = ((x >= start) && (x <= (start + len)))

inViewingWindow :: Window -> ASIndex -> Bool
inViewingWindow w (Index cSheetId indexCoord) = and [ 
  w^.windowSheetId == cSheetId,
  inRange (view col indexCoord) tlc (brc-tlc),
  inRange (view row indexCoord) tlr (brr-tlr)]
    where tlc = w^.topLeft.col
          tlr = w^.topLeft.row
          brc = w^.bottomRight.col
          brr = w^.bottomRight.row

intersectViewingWindow :: [ASCell] -> Window -> [ASCell]
intersectViewingWindow cells vw = filter ((inViewingWindow vw) . view cellLocation) cells

-- | Computes set difference of window 2 minus window 1. Represented as a union of ASRange's. 
getScrolledLocs :: Window -> Window -> [ASRange]
getScrolledLocs (Window _ _ (Col (-1), Row (-1)) (Col (-1), Row (-1))) (Window sheetid _ tl br) = [(Range sheetid (tl, toExtendedCoord br))]
getScrolledLocs w1@(Window _ _ tl br) w2@(Window sheetid _ tl' br')
  | windowsIntersect w1 w2 = getUncoveredLocs sheetid overlapping (tl', br')
  | otherwise = [(Range sheetid (tl', toExtendedCoord $ br'))]
    where 
      y  = view col tl ; x  = view row tl ; y2  = view col br ; x2  = view row br
      y' = view col tl'; x' = view row tl'; y2' = view col br'; x2' = view row br'
      overlapping = (makeCoord (max y y') (max x x'), makeCoord (min y2 y2') (min x2 x2'))
      windowsIntersect (Window _ _ (y, x) (y2, x2)) (Window _ _ (y', x') (y2', x2'))
        | y2 > y' = False 
        | y < y2' = False
        | x2 < x' = False 
        | x > x2' = False
        | otherwise = True 
            
getUncoveredLocs :: SheetID -> (Coord, Coord) -> (Coord, Coord) -> [ASRange]
getUncoveredLocs sheet (tlo, bro) (tlw, brw) = map (Range sheet . (\(a, b) -> (a, toExtendedCoord $ b))) cs
    where
      trw = makeCoord (view col brw) (view row tlw)
      blw = makeCoord (view col tlw) (view row brw)
      tro = makeCoord (view col bro) (view row tlo)
      blo = makeCoord (view col tlo) (view row bro)
      cs = [(tlw, tro), (trw, bro), (brw, blo), (blw, tlo)]
