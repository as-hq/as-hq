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

printTime :: String -> IO ()
printTime str = do 
  time <- (getCurrentTime >>= return . utctDayTime)
  putStrLn $ str ++ " " ++ (show time)

getUpdateTime :: ASTime
getUpdateTime = Time "hi" 1 2 3

----------------------------------------------------------------------------------------------------------------------------------------------

intersectViewingWindow :: [ASCell] -> ASViewingWindow -> [ASCell]
intersectViewingWindow cells vw = filter (inVW vw) cells
  where
    inVW :: ASViewingWindow -> ASCell -> Bool
    inVW (ASViewingWindow tlc tlr w h) (Cell (Index _ (col,row)) _ _) = ((inRange col tlc w) && (inRange row tlr h))
      where
        inRange :: Int -> Int -> Int -> Bool
        inRange elem start len = ((elem >= start) && (elem <= (start + len)))

initialViewingWindow :: ASViewingWindow
initialViewingWindow = ASViewingWindow 0 0 100 100 


fromRight :: Either a b -> b
fromRight (Right b) = b

