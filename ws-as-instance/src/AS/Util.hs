module AS.Util where

import AS.Types
import Data.Time.Clock

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Error Handling

generateErrorMessage :: ASExecError -> IO String
generateErrorMessage e = return "hi"

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Time

getTime :: IO String
getTime = fmap (show . utctDayTime) getCurrentTime

printTimed :: String -> IO ()
printTimed str = do 
  time <- getTime
  putStrLn $ "[" ++ (show time) ++ "] " ++ str 

getASTime :: IO ASTime
getASTime = return $ Time "hi" 1 2 3

----------------------------------------------------------------------------------------------------------------------------------------------
-- | viewing windows 

intersectViewingWindows :: [ASCell] -> [ASWindow] -> [ASCell]
intersectViewingWindows cells vws = concat $ map (intersectViewingWindow cells) vws 

intersectViewingWindow :: [ASCell] -> ASWindow -> [ASCell]
intersectViewingWindow cells vw = filter (inVW vw) cells
  where
    inVW :: ASWindow -> ASCell -> Bool
    inVW (Window wSheetId (tlc, tlr) (brc, brr)) (Cell (Index (Sheet cSheetId _) (col,row)) _ _ _) = ((wSheetId==cSheetId) && (inRange col tlc (brc-tlc)) && (inRange row tlr (brr-tlr)))
    inRange :: Int -> Int -> Int -> Bool
    inRange elem start len = ((elem >= start) && (elem <= (start + len)))

fromRight :: Either a b -> b
fromRight (Right b) = b

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Users

updateMessageUser :: ASUserId -> ASMessage -> ASMessage
updateMessageUser uid (Message _ a r p) = Message uid a r p 

----------------------------------------------------------------------------------------------------------------------------------------------
-- | Tags

containsStreamTag :: [ASCellTag] -> Bool
containsStreamTag [] = False
containsStreamTag ((Streaming _ _):tags) = True
containsStreamTag (tag:tags) = containsStreamTag tags

-- unsafe for empty lists
getStreamTag :: [ASCellTag] -> ASCellTag
getStreamTag (s@(Streaming _ _):tags) = s
getStreamTag (tag:tags) = getStreamTag tags