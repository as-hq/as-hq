import Control.Monad.Identity (runIdentity)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Control.Monad (replicateM)

-- Template haskell doesn't work well when you have a nontrivial build process.

data ArgType = O | V deriving (Eq, Show)

makeArgs :: String -> [ArgType]
makeArgs = map (\x -> if x == 'V' then V else O)

dispatcher :: [ArgType] -> (Int -> a, a) -> (Int -> a, a) -> [a]
dispatcher sig (f1arg, f1res) (f2arg, f2res) = dispatcher' 1 sig where
    dispatcher' _ (x:[]) = case x of
                             V -> [f1res]
                             O -> [f2res]
    dispatcher' n (x:xs) = (case x of
                             V -> f1arg n
                             O -> f2arg n) : dispatcher' (n+1) xs

toPyObjectGen :: [ArgType] -> String
toPyObjectGen sig = intercalate "\n" $ catMaybes $ toPyObjectGen' sig

toPyObjectGen' :: [ArgType] -> [Maybe String]
toPyObjectGen' sig = dispatcher sig (\n -> (Just $ "    x" ++ show n ++ " <- toPyObject input" ++ show n), Nothing) (const Nothing, Nothing)

fromPyObjectGen :: [ArgType] -> String
fromPyObjectGen sig = intercalate "\n" $ catMaybes $ fromPyObjectGen' sig

fromPyObjectGen' :: [ArgType] -> [Maybe String]
fromPyObjectGen' sig = dispatcher sig (const Nothing, Just ("    b <- fromPyObject fr\n    return b")) (const Nothing, Just ("    return fr"))


defCallGen :: [ArgType] -> String
defCallGen sig = concat [ "    fr <- def"
                        , show (length sig)
                        , " s \""
                        , concatMap show sig
                        , "\" "
                        , intercalate " " ["x" ++ show n | n <- [1..(length sig - 1)]]
                        ]

typeargsGen :: [ArgType] -> String
typeargsGen sig = let components = typeargsGen' sig in
    case components of
      [] -> ""
      xs -> intercalate " -> " xs

typeargsGen' :: [ArgType] -> [String]
typeargsGen' sig = dispatcher sig (f1arg, f1res) (f2arg, f2res) where
    f1arg = \n -> "a" ++ show n
    f1res = "IO b"
    f2arg = \n -> "(PyObject a" ++ show n ++ ")"
    f2res = "IO (PyObject b)"

typeclassesGen :: [ArgType] -> String
typeclassesGen sig = let components = catMaybes (typeclassesGen' sig) in
    case components of
      [] -> ""
      xs -> concat [ "("
                   , intercalate ", " xs
                   ,  ") => " 
                   ]


typeclassesGen' :: [ArgType] -> [Maybe String]
typeclassesGen' sig = dispatcher sig (f1arg, f1res) (f2arg, f2res) where
    f1arg = \n -> Just ("ToJSON a" ++ show n)
    f1res = Just ("FromJSON b")
    f2arg = const Nothing
    f2res = Nothing

funcname :: [ArgType] -> String
funcname sig = "def" ++ concat (map show sig)

typesigGen :: [ArgType] -> String
typesigGen sig = concat [ funcname sig
                        , " :: " 
                        , typeclassesGen sig
                        , "String -> "
                        , typeargsGen sig 
                        ]

funcsigGen :: [ArgType] -> String
funcsigGen sig = funcname sig ++ " s " ++ args ++ " = do" where
    xs = init $ dispatcher sig (\n -> "input" ++ show n, "") (\n -> "x" ++ show n, "")
    args = intercalate " " xs

codeGen :: [ArgType] -> String
codeGen sig = runIdentity $ do
    return . intercalate "\n" . filter ((>0) . length) $ map (flip ($) sig)
        [ typesigGen 
        , funcsigGen 
        , toPyObjectGen 
        , defCallGen 
        , fromPyObjectGen 
        ]

main = do
    x <- return $ concat $ map (flip replicateM "OV") [1..5]
    x2 <- return $ map makeArgs x
    result <- return $ intercalate "\n\n" $ map codeGen x2
    putStrLn $ intercalate "\n" $ map (\sig -> "    " ++ funcname sig ++ ",") x2
    putStrLn result
