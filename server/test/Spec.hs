import AS.Types
import AS.DAG
import Data.Text as T
import qualified Data.List as L

testLocs :: Int -> [ASLocation]
testLocs n = [Index (T.pack "hi") (i,i) | i <-[0..n]]

testEdges :: Int -> [(ASLocation,ASLocation)]
testEdges n = L.zip (testLocs n) (testLocs n)

desc :: Int -> IO [ASLocation]
desc n = return $ descendants (testLocs 1) (testEdges n)


main :: IO ()
main = do 
    putStrLn $ "Starting test"
    -- let int = L.intersect  (testLocs 10000) (testLocs 1)
    let d = toposort (testEdges 10000)
    putStrLn $ show d
    putStrLn $ "Ending test"
    return ()
