module AS.Util where

import Data.Time.Clock

getTime :: IO String
getTime = fmap (show . utctDayTime) getCurrentTime

printTimed :: String -> IO ()
printTimed msg = do
    time <- getTime
    putStrLn $ "[" ++ time ++ "] " ++ msg