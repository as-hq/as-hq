module AS.Kernels.Excel.Lib where

import AS.Util (rangeToIndicesRowMajor)
import AS.Types.Core
import AS.Types.Excel
import AS.Kernels.Excel.Util
import AS.Kernels.Excel.Compiler
import qualified Data.Map.Strict as M
import Data.Either
import Data.Maybe
import Control.Monad.Except
import Data.List
import qualified Data.Vector as V

import Text.Read as TR
import Data.Char (toLower,toUpper)
import qualified Data.Text as T
import qualified Data.String.Utils as SU

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr

import qualified Statistics.Distribution as SD
import qualified Statistics.Distribution.Binomial as SDB
import qualified Statistics.Distribution.Normal as SDN
import Data.Ord (comparing)

import qualified Data.Map.Lazy as ML

import AS.Parsing.Out (exRefToASRef)
import Control.Exception.Base hiding (try)

import AS.Util

data RefMap = RefMap {refMap :: M.Map ERef EEntity, refDim :: (Col,Row)} deriving (Show,Read)
type Arg a = (Int,a)
type Dim = (Col,Row)
type Offset = (Col,Row)

-- | TODO: might need a wrapper around stuff to get 0.999999 -> 1 for things like correl
-- | TODO: use unboxing if performance is a problem
-- | TODO: implement indirect helper
-- | TODO: implement rand without IO creep
-- | TODO: test missing arguments; f(a1,a2,,a3) and refactor; EMissing isn't really an EValue
-- | TODO: test hypothesis that scalarizing a ref is actually row col int based always
-- | TODO: make sure that ifFunc's can deal with array constants as arguments if acceptable

-- | TODO: index is only array-mode at this point (tuples needed for ref mode)
-- | TODO: apparently index(A1:B2,{1;2},{1,2}) works WTF

-- | NOTE: Any reference with an unmappable ASValue -> EValue returns error (we get to define this behavior)

--------------------------------------------------------------------------------------------------------------
-- | Function callbacks and enumeration of type of function

-- | Description of a function. Argument numbers start at 1.
data FuncDescriptor = FuncDescriptor {
  scalarArgsIfNormal :: [Int],    -- Arguments to scalarize in normal mode
  mapArgsIfArrayFormula :: [Int], -- Arguments to map over in array formula mode ("unexpected arrays")
  arrFormEvalArgs :: [Int],       -- Evaluate these arguments as AF in normal mode (and in AF mode as well)
  replaceRefArgs :: [Int],        -- Before evaluation, replace reference arguments by context/DB
  maxNumArgs :: Maybe Int,        -- Upper bound on number of arguments (Nothing means no bound, like sum)
  callback :: EFuncResult
}

-- | Scalarize in normal mode, map over all arguments for array formula, replace all refs
normalD :: Int -> EFunc -> FuncDescriptor
normalD n f = FuncDescriptor [1..] [1..] [] [1..] (Just n) (transform f)

-- | Similar to above, but only map/scalarize over some arguments
normalD' :: Int -> [Int] -> EFunc -> FuncDescriptor
normalD' n nonRanges f = FuncDescriptor nonRanges nonRanges [] [1..] (Just n) (transform f)

-- | Vector functions like sum; don't scalarize or map (args can be ranges!), replace all refs, no arg limit
vectorD :: EFunc -> FuncDescriptor
vectorD f = FuncDescriptor [] [] [] [1..] Nothing (transform f)

-- | Same as above, but eval some args as AF's
vectorAFD :: [Int] -> EFunc -> FuncDescriptor
vectorAFD lst f = FuncDescriptor [] [] lst [1..] Nothing (transform f)

-- | Functions like sumproduct, correl
vector2 :: EFunc -> FuncDescriptor
vector2 f = FuncDescriptor [] [] [1,2] [1..] (Just 2) (transform f)

infixD :: EFunc -> FuncDescriptor
infixD = normalD 2 

-- | Map function names to function descriptors
functions :: M.Map String FuncDescriptor
functions =  M.fromList $
    -- | Excel infix functions
   [("+"              , infixD eAdd),
    ("-"              , infixD eMinus),
    ("*"              , infixD eMult),
    ("/"              , infixD eDivide),
    ("^"              , infixD ePower),
    ("="              , infixD eEquals),
    ("<>"             , infixD eNotEquals),
    (">="             , infixD eGreaterE),
    ("<="             , infixD eLessE),
    (">"              , infixD eGreater),
    ("<"              , infixD eLess),
    ("&"              , infixD eAmpersand),
    (" "              , FuncDescriptor [1,2] [1,2] [] [] (Just 2) (transform eSpace)), -- don't replace refs

    -- | Excel information functions
    ("isblank"        , normalD 1 eIsBlank),
    ("iserror"        , FuncDescriptor [1] [1] [] [1] (Just 1) eIsError), -- efuncresult
    ("islogical"      , normalD 1 eIsLogical),
    ("isnumber"       , normalD 1 eIsNumber),

    -- | Excel logical functions
    ("iferror"        , FuncDescriptor [1,2] [1,2] [] [1,2] (Just 2) eIfError), -- efuncresult
    ("if"             , normalD 3 eIf),
    ("and"            , vectorD eAnd),
    ("or"             , vectorD eOr),
    ("not"            , normalD 1 eNot),

    -- | Excel lookup and reference functions
      -- | Address(A1:A2,B1:B2) is an Excel valerror in normal mode, but ArrForm works mapping over all args
    ("address"        , FuncDescriptor [] [1..] [] [1..] (Just 5) (transform eAddress)),
      -- | Column(A1:A2) returns topleft, don't scalarize (func will return matrix, top level takes top left)
      -- the function will handle ranges (don't map over AF), don't replace refs obv
    ("column"         , FuncDescriptor [] [] [] [] (Just 1) (transform eColumn)),
    ("row"            , FuncDescriptor [] [] [] [] (Just 1) (transform eRow)),
      -- | Indirect(array) = error; don't scalarize, replace refs because arg1 = string, arg2 = bool
    ("indirect"       , FuncDescriptor [] [1..] [] [1..] (Just 2) (transform eIndirect)),
      -- | First arg = ref, no array formula (returns reference)
    ("offset"         , FuncDescriptor [2..] [] [] [2..] (Just 5) (transform eOffset)),
      -- | Array mode is normal in 2nd, 3rd args, replace all args
    ("index"          , normalD' 3 [2,3] eIndex),
      -- | Normal in first and third, second is a matrix
    ("match"          , normalD' 3 [1,3] eMatch),
      -- | Don't scalarize; tranpose(arr) is error in normal mode
    ("transpose"      , FuncDescriptor [] [] [] [1] (Just 1) (transform eTranspose)),

    -- | Excel math and trig functions
    ("product"        , vectorD eProduct),
    ("abs"            , normalD 1 eAbs),
    ("exp"            , normalD 1 eExp),
    ("pi"             , normalD 0 ePi),
    ("sqrt"           , normalD 1 eSqrt),
    ("sqrtpi"         , normalD 0 eSqrtPi),
    ("sum"            , vectorD eSum),
    -- | Don't replace refs for ranges (resizing)
    ("sumif"          , FuncDescriptor [2] [2] [] [2] (Just 3) (transform eSumIf)),
    ("sumifs"         , FuncDescriptor [3,5..] [3,5..] [] [1..] Nothing (transform eSumIfs)),
    ("sumsq"          , vectorD eSumSq),
    ("sumx2my2"       , vector2 eSumx2my2),
    ("sumx2py2"       , vector2 eSumx2py2),
    ("sumxmy2"        , vector2 eSumxmy2),
    ("sumproduct"     , vector2 eSumProduct),

    -- | Excel statistical functions
    ("avedev"         , vectorD eAvedev),
    ("average"        , vectorD eAverage),
    -- | Don't replace refs (resizing)
    ("averageif"      , FuncDescriptor [2] [2] [] [2] (Just 3) (transform eAverageIf)),
    ("averageifs"     , FuncDescriptor [3,5..] [3,5..] [] [1..] Nothing (transform eAverageIfs)),
    ("binom.dist"     , normalD 4 eBinomDist),
    ("correl"         , vector2 eCorrel),
    ("count"          , vectorD eCount),
    -- | Similar to averageif, but only two args
    ("countif"        , FuncDescriptor [2] [2] [] [2] (Just 2) (transform eCountIf)),
    -- | Similar to averageifs but no "sum range" argument shifts the criteria arguments
    ("countifs"       , FuncDescriptor [2,4..] [2,4..] [] [1..] Nothing (transform eCountIfs)),
      -- | 2nd argument is a range; only map and scalarize over the first
    ("large"          , normalD' 2 [1] eLarge),
    ("min"            , vectorD eMin),
    ("max"            , vectorD eMax),
    ("median"         , vectorD eMedian),
    ("mode.mult"      , vectorAFD [1] eModeMult),
    ("mode.sngl"      , vectorAFD [1] eModeSngl),
    ("norm.dist"      , normalD 4 eNormDist),
    ("norm.inv"       , normalD 4 eNormInv),
    ("pearson"        , vector2 ePearson),
      -- | 2nd argument is a range, only map and scalarize over the first (for rank)
    ("rank.eq"        , normalD' 3 [1,3] eRankEq),
    ("rank.avg"       , normalD' 3 [1,3] eRankAvg),
    ("slope"          , vector2 eSlope),
    ("stdev.s"        , vectorD eStdS),
    ("stdev.p"        , vectorD eStdP),
    ("var.s"          , vectorD eVarS),
    ("var.p"          , vectorD eVarP),
    ("covar"          , vector2 eCoVar),

    -- | Excel text functions
    ("find"           , normalD 3 eFind),
    ("len"            , normalD 1 eLen),
    ("rept"           , normalD 2 eRept),
      -- | Right function has a "number of bytes" optional 3rd argument that I'm ignoring bc wtf
    ("right"          , normalD 2 eRight),
    ("search"         , normalD 3 eSearch),   
    ("substitute"     , normalD 4 eSubstitute),
    ("trim"           , normalD 1 eTrim),
    ("upper"          , normalD 1 eUpper)]

-- | Many functions are simpler to implement as [EEntity] -> EResult
-- | This function maps those EFuncs to EFuncResults by returning an error if any args were errors
transform :: EFunc -> EFuncResult
transform f = \c r -> do
  args <- compressErrors r
  f c args

getFunc :: String -> ThrowsError FuncDescriptor
getFunc f = case (M.lookup (map toLower f) functions) of
  Nothing -> throwError $ NotFunction $ map toUpper f
  Just fd -> Right fd

--------------------------------------------------------------------------------------------------------------
-- | AST Evaluation functions

-- | Make sure that a function doesn't have too many args
checkNumArgs :: String -> Maybe Int -> Int -> ThrowsError ()
checkNumArgs name (Just max) len =  if len > max
  then Left $ TooManyArgs name
  else Right ()
checkNumArgs _ Nothing _ = Right ()

-- | If eval basic formula produces a matrix, only take top left value
topLeftForMatrix :: String -> EResult ->  EResult
topLeftForMatrix f (Right (EntityMatrix (EMatrix _ _ v)))
  | V.null v = Left $ EmptyMatrix f
  | otherwise = valToResult $ V.head v
topLeftForMatrix _ r = r

evalBasicFormula :: Context -> BasicFormula -> EResult
evalBasicFormula c (Ref exLoc) = locToResult $ exRefToASRef (shName (curLoc c)) exLoc
evalBasicFormula c (Var val)   = valToResult val
evalBasicFormula c (Fun f fs)  = do
  fDes <- getFunc f
  let args = map (getFunctionArg c fDes) (zip [1..] fs)
  let argsRef = substituteRefsInArgs c fDes args
  checkNumArgs f (maxNumArgs fDes) (length argsRef)
  topLeftForMatrix f $ (callback fDes) c argsRef

evalFormula :: Context -> Formula -> EResult
evalFormula c (Basic b) = evalBasicFormula c b
evalFormula c (ArrayConst b) = do
  let lstChildren = map (map (evalBasicFormula c)) b
  ac <- compressErrors $ concat lstChildren
  fmap EntityMatrix $ arrConstToResult c $ map rights lstChildren

evalArrayFormula :: Context -> Formula -> EResult
evalArrayFormula c (Basic (Ref exLoc)) = locToResult $ exRefToASRef (shName (curLoc c)) exLoc
evalArrayFormula c (Basic (Var val)) = valToResult val
evalArrayFormula c f@(ArrayConst b) = evalFormula c f
evalArrayFormula c f@(Basic (Fun name fs)) = do
  fDes <- getFunc name
  let s = shName (curLoc c)
  refMap <- unexpectedRefMap c (getUnexpectedRefs s f)
  checkNumArgs name (maxNumArgs fDes) (length fs)
  case refMap of
    Just mp -> evalArrayFormula' c mp f
    Nothing -> do
      let args = map (evalArrayFormula c) fs
      let argsRef = substituteRefsInArgs c fDes args
      (callback fDes) c argsRef

-- | Evaluate a (valid) array formula given a RefMap to replace unexpected array references
-- | In particular, calling this method means the callback will return a Matrix
evalArrayFormula' :: Context -> RefMap -> Formula -> EResult
evalArrayFormula' c mp (Basic (Fun f fs)) = do
  let args = map (evalArrayFormula' c mp) fs
  let replacedArgs = map (replace (refMap mp)) args
  fDes <- getFunc f
  mapArgs (refDim mp) fDes c replacedArgs
evalArrayFormula' c mp f = evalArrayFormula c f

--------------------------------------------------------------------------------------------------------------
-- | AST Normal Evaluation helpers

-- | If the argument is an array constant to be scalarized, replace with top left value
-- | Depending on arg number, do normal eval or array formula eval
-- | If the resulting EResult is a reference to be scalarized, compute intersection/throw error
-- | The callback function itself will throw an error if it has an invalid argument type
getFunctionArg :: Context -> FuncDescriptor -> Arg Formula -> EResult
getFunctionArg c fd (argNum,(ArrayConst lst)) = scalarizeArrConst c fd argNum lst
getFunctionArg c fd (argNum,f) = scalarizeRef (curLoc c) fd (argNum,res)
  where
    res | elem argNum (arrFormEvalArgs fd) = evalArrayFormula c f
      | otherwise = evalFormula c f

scalarizeArrConst :: Context -> FuncDescriptor -> Int -> [[BasicFormula]] -> EResult
scalarizeArrConst c fd argNum lst
  | (elem argNum (scalarArgsIfNormal fd)) = case (topLeftLst lst) of
    Nothing -> throwError $ EmptyArrayConstant
    Just tl -> evalBasicFormula c tl
  | otherwise =  evalFormula c (ArrayConst lst)

scalarizeRef :: ASReference -> FuncDescriptor -> Arg EResult -> EResult
scalarizeRef curLoc fd (argNum,r@(Right (EntityRef (ERef loc))))
  |(elem argNum (scalarArgsIfNormal fd)) = case (scalarizeLoc curLoc loc) of
    Nothing -> throwError $ ScalarizeIntersectionError curLoc loc
    Just newLoc -> locToResult newLoc
  |otherwise = r
scalarizeRef _ _ x = snd x

--------------------------------------------------------------------------------------------------------------
-- | AST Array Formula Evaluation helpers

-- | Extract all unexpected range references out of a (valid) formula
getUnexpectedRefs :: ASSheetId -> Formula -> [ERef]
getUnexpectedRefs _ (Basic (Var s)) = []
getUnexpectedRefs _ (Basic (Ref e)) = []
getUnexpectedRefs s (ArrayConst b) = concat $ map (getUnexpectedRefs s) $ map Basic (concat b)
getUnexpectedRefs s (Basic (Fun f fs)) = concat $ map (getRangeRefs s fDes) enum
  where
    (Right fDes) = getFunc f
    enum = zip [1..] fs

-- | Helper: Given an argument, return the (possible) underlying range refs
getRangeRefs :: ASSheetId -> FuncDescriptor -> Arg Formula -> [ERef]
getRangeRefs s fDes (numArg,(Basic (Ref exLoc)))
  | (elem numArg (mapArgsIfArrayFormula fDes)) = if (isRange (exRefToASRef s exLoc))
    then [ERef (exRefToASRef s exLoc)]
    else []
  | otherwise = []
getRangeRefs s _ (_,f) = getUnexpectedRefs s f

-- | Returns a map of replacements for "unexpected" range references
-- | All must have same dimension, same width and height (or 1), or same height and width (or 1)
unexpectedRefMap :: Context -> [ERef] -> ThrowsError (Maybe RefMap)
unexpectedRefMap c refs = case dim' of
  Nothing  -> Right Nothing
  Just dim -> case (compressErrors entities) of
    Left e -> throwError e
    Right es -> Right $ Just $  RefMap (M.fromList (zip refs es)) dim
    where
      entities = map (modifyRefToEntity c dim) refs
  where
    dim' = getCommonDimension refs

-- | Helper: gets the common dimensionality of the unexpected references
getCommonDimension :: [ERef] -> Maybe Dim
getCommonDimension [] = Nothing
getCommonDimension refs = dim
  where
    cols = map (\(ERef l) -> fst (dimension l)) refs
    rows = map (\(ERef l) -> snd (dimension l)) refs
    dim | and [allTheSame cols, allTheSame rows]      = Just $ (head cols, head rows)
      | and [allTheSame cols, allTheSameOrOne rows] = Just $ (head cols, maximum rows)
      | and [allTheSameOrOne cols, allTheSame rows] = Just $ (maximum cols, head rows)
      | otherwise = Nothing

-- | Given the correct dimension and a reference, replace it with a matrix of the right dimension, possibly using replication
modifyRefToEntity :: Context -> Dim -> ERef -> ThrowsError EEntity
modifyRefToEntity con (c,r) ref@(ERef l) = case (refToEntity con ref) of
  Left e   -> throwError e
  Right (EntityMatrix (EMatrix dCol dRow vec)) -> Right $ EntityMatrix (EMatrix c r res)
    where
      res | and [dCol==c,dRow==r] = vec
        | and [dCol==c,dRow==1] = V.concat $ map (\a -> vec) [1..r]
        | and [dCol==1,dRow==r] = flattenMatrix $ V.map (V.replicate c) vec

--------------------------------------------------------------------------------------------------------------
-- | AST Array Constant helpers

-- | Converts an entity to a value if it can be an element of an array constant (not range refs, large matrices)
toValueAC:: Context -> EEntity -> ThrowsError EValue
-- | Don't accept range references
toValueAC c (EntityRef r@(ERef l))
  | isRange l = Left  $ ArrayConstantDim
  | otherwise = do
    entity <- refToEntity c r
    toValueAC c entity
toValueAC _ (EntityVal v) = Right v
-- | Only accept 1x1 matrices
toValueAC _ (EntityMatrix (EMatrix c r v)) = case (c,r) of
  (1,1) -> Right $ V.head v
  otherwise -> Left  $ ArrayConstantDim

-- | Converts an array constant to a matrix (replaces non-range references)
-- | Throws error for wrong dimensionality
arrConstToResult :: Context -> [[EEntity]] -> ThrowsError EMatrix
arrConstToResult c [[]] = Left $ ArrayConstantDim
arrConstToResult c es = do
  if (aligned es)
    then do
      vals <- compressErrors $ concat $ map (map (toValueAC c)) es
      return $ EMatrix (length (head es)) (length es) (V.fromList vals)
    else Left $ ArrayConstantDim

--------------------------------------------------------------------------------------------------------------
-- | Reference lookup/replacement and DB functions


-- | Given context, maps a reference to an entity by lookup, then converting ASValue -> EEntity (matrix)
-- | Assumes that context has ranges -> ValueL and ValueL [] = row, ValueL [[]] = column of rows
-- | Seems OK to map over this function, since any given formula won't have too many references requiring DB
refToEntity :: Context -> ERef -> ThrowsError EEntity
refToEntity c (ERef l@(IndexRef i)) = case (asValueToEntity v) of
  Nothing -> Left $ CannotConvertToExcelValue l
  Just e  -> Right e
  where
    v = case (M.lookup l (evalMap c)) of
      Nothing -> dbLookup i
      Just v' -> v'
refToEntity c (ERef (l@(RangeRef r))) = if any isNothing vals
  then Left $ CannotConvertToExcelValue l
  else Right $ EntityMatrix $ EMatrix (getWidth l) (getHeight l) $ V.fromList $ catMaybes vals
  where
    mp = evalMap c
    idxs = rangeToIndicesRowMajor r
    (inMap,needDB) = partition (((flip M.member) mp).IndexRef) idxs
    mapVals = catMaybes $ map (((flip M.lookup) mp).IndexRef) inMap
    dbVals = dbLookupBulk needDB
    vals = map toEValue $ mapVals ++ dbVals

replace :: (M.Map ERef EEntity) -> EResult -> EResult
replace mp r@(Right (EntityRef ref)) = case (M.lookup ref mp) of
  Nothing -> r
  Just e' -> Right e'
replace mp x = x

-- | Replace results (ref -> entity) wherever the function descriptor says to
substituteRefsInArgs :: Context -> FuncDescriptor -> [EResult] -> [EResult]
substituteRefsInArgs c fDes es = map (subsRef c fDes) enum
  where
    enum = zip [1..] es
    subsRef :: Context -> FuncDescriptor -> Arg EResult -> EResult
    subsRef c fDes (argNum,a@(Right (EntityRef r)))
      | elem argNum (replaceRefArgs fDes)= refToEntity c r
      | otherwise = a
    subsRef _ _ x = snd x

--------------------------------------------------------------------------------------------------------------
-- | Mapping arguments of array formulas

-- | At this point, the unexpected array function arguments are matrices
-- | This function defines how we map over them
-- | Allows for future different function implementations (MapReduce in Cloud Haskell etc)
-- | Since functions have all info they need to eval

-- | The function descriptor contains which arguments to map for an array formula
-- | Mapping requires that entity to be a matrix
mapArgs :: Dim -> FuncDescriptor -> EFuncResult
mapArgs (c,r) fDes con e = do
  -- | Row-major accumulation of results
  let evalAF = resToVal . ((callback fDes) con) . (getOffsetArgs fDes e)
  let results = [evalAF (c',r') | r'<-[0..(r-1)],c'<-[0..(c-1)]]
  errResults <- compressErrors results
  return $ EntityMatrix (EMatrix c r (V.fromList errResults))

-- | Given a function descriptor, a list of arguments (results) and an offset
-- | Replace each argument with the offsetted value if fDes says to
getOffsetArgs :: FuncDescriptor -> [EResult] -> Offset -> [EResult]
getOffsetArgs fDes rs offset = map (getEntityElem fDes offset) (zip [1..] rs)
  where
    getEntityElem :: FuncDescriptor -> Offset -> Arg EResult -> EResult
    getEntityElem fDes offset (argNum,r@(Right (EntityMatrix m)))
      | (elem argNum (mapArgsIfArrayFormula fDes)) = valToResult $ matrixIndex offset m
      | otherwise = r
    getEntityElem _ _ x = snd x

--------------------------------------------------------------------------------------------------------------
-- | Function evaluation helpers

-- | V.sum starts from 0.0, but we want to keep ints preserved; different version of sum
sumInt :: V.Vector ENumeric ->  ENumeric
sumInt = V.foldl' (+) (EValueI 0)

-- | Make sure that the number of arguments is right
testNumArgs :: Int -> String -> [a] -> ThrowsError [a]
testNumArgs n name lst
  | (length lst) == n = Right lst
  | otherwise = Left $ NumArgs name n (length lst)

-- | Make sure that the number of arguments isn't too high
testNumArgsUpper :: Int -> String -> [a] -> ThrowsError [a]
testNumArgsUpper n name lst
  | (length lst) <= n = Right lst
  | otherwise = Left $ NumArgs name n (length lst)

-- | Try to cast a value to numeric
numVal :: EValue -> Maybe ENumeric
numVal (EValueNum n) = Just n
numVal _ = Nothing

filterNum :: V.Vector EValue -> V.Vector ENumeric
filterNum v = V.map (\(EValueNum n) -> n) $ V.filter (isJust . numVal) v

flattenMatrix :: V.Vector (V.Vector a) -> V.Vector a
flattenMatrix = V.concat . V.toList

-- | Useful for functions like average; takes input and converts it (possibly) to a numeric vector
-- | If any argument is an error (even within a matrix), return an error
-- | Try to cast strings and booleans if they are arguments (0,1,"4") but ignore them within ranges/matrices
argsToNumVec :: [EEntity] -> ThrowsError (V.Vector ENumeric)
argsToNumVec es = do
  vecs <- compressErrors $ map argToNumVec es
  return $ V.concat vecs

-- | Helper for above; Takes an entity and returns a numeric vector (often, a singleton)
argToNumVec :: EEntity -> ThrowsError (V.Vector ENumeric)
argToNumVec (EntityVal (EValueNum n)) = Right $ V.singleton n
argToNumVec (EntityVal (EValueS s))   = case ((TR.readMaybe s)::Maybe Int) of
  Nothing -> case ((TR.readMaybe s)::Maybe Double) of
    Nothing -> Left $ VAL $ "Argument is not numeric"
    Just d  -> Right $ V.singleton $ EValueD d
  Just i -> Right $ V.singleton $ EValueI i
argToNumVec (EntityVal (EValueE e)) = Left $ Default e
argToNumVec (EntityVal (EValueB True)) = Right $ V.singleton $ EValueI 1
argToNumVec (EntityVal (EValueB False)) = Right $ V.singleton $ EValueI 0
-- | Ignore all non-numeric values within a matrix
argToNumVec (EntityMatrix m) = do
  (EMatrix c r v) <- matrixError m
  Right $ filterNum v
argToNumVec _  = Left $ VAL $ "Argument is not numeric"


-- | For functions like sum/product that collapse matrices and loop over all their arguments
-- | Simply provide an accumulation function and this function does the rest
collapseNumeric :: (ENumeric -> ENumeric -> ENumeric) -> EFunc
collapseNumeric f c e = do
  nums <- argsToNumVec e
  if (V.null nums)
    then valToResult $ EValueNum $ EValueI 0 -- return 0 by default if no numeric arguments received
    else valToResult $ EValueNum $ V.foldl1' f nums

-- | For functions like sumsq that collapse matrices and loop over all their arguments
-- | (Provide a fold function with init)
collapseNumeric' :: (ENumeric -> ENumeric -> ENumeric) -> EFunc
collapseNumeric' f c e = do
  let init = (EValueI 0)
  let vecFold =  V.foldl' f init
  nums <- argsToNumVec e
  if (V.null nums)
    then valToResult $ EValueNum $  EValueI 0 -- return 0 by default if no numeric arguments received
    else valToResult $ EValueNum $ V.foldl' f init nums

-- | Functions like sumxmy2 have a "zipper" (what to map corresponding elements to) as a main distinguishing element
-- | Given that function and function name, produce EFunc
zipNumericSum2 :: (ENumeric -> ENumeric -> ENumeric) -> String -> EFunc
zipNumericSum2 zipper name c e = do
  -- | Make sure that there are two arguments, both matrices
  (EMatrix c1 r1 v1) <- getRequired "matrix" name 1 e :: ThrowsError EMatrix
  (EMatrix c2 r2 v2) <- getRequired "matrix" name 2 e :: ThrowsError EMatrix
  -- | Throw error if dimensions don't match
  if ((c1,r1) /= (c2,r2))
    then Left $ NA $ "Arguments for " ++ name ++ " had different sizes"
    else valToResult $ EValueNum $ sumInt $ V.zipWith zipFunc (V.map numVal v1) (V.map numVal v2)
      where
        -- | If both elements aren't numeric, replace with 0
        -- | Eg sumxmy2 with string arrays will return 0
        zipFunc :: Maybe ENumeric -> Maybe ENumeric -> ENumeric
        zipFunc (Just a) (Just b) = zipper a b
        zipFunc _ _ = EValueI 0

-- | Template for functions like sumif etc; produces the filtered vector to apply some lambda function to
ifFunc :: String ->  Context -> [EEntity] -> ThrowsError (V.Vector EValue)
ifFunc f c e = do
  -- | We want refs because we may need to resize later (Excel sucks)
  critRange <- getRequired "ref" f 1 e :: ThrowsError ERef
  criteria <- getRequired "value" f 2 e :: ThrowsError EValue
  valRange <- getOptional "ref" critRange f 3 e :: ThrowsError ERef
  -- | Make sure that the criteria range and sum range have the same dimension before replacing refs
  let valRange' = matchDimension critRange valRange
  -- | refToEntity will throw an error if any ASValue cannot be mapped to an Excel value
  critEntity <- refToEntity c critRange
  valEntity <- refToEntity c valRange'
  let matcher = getLambda criteria :: (EValue -> Bool)
  case critEntity of
    -- | Excel *IF functions work with non-range references; return 0 as default if no match
    (EntityVal v) -> if (matcher v)
      then Right $ V.singleton v
      else Right $ V.singleton $ EValueNum $ EValueI 0
    (EntityMatrix (EMatrix nc nr critVec)) -> do
      let (EntityMatrix (EMatrix _ _ valVec)) = valEntity
      Right $ V.ifilter (\i _ -> matcher ((V.!) critVec i)) valVec

-- | Template for functions like sumifs 
ifsFunc :: String -> [EEntity] -> ThrowsError (V.Vector EValue)
ifsFunc f e = do
  if (length e < 3)
    then Left $ NumArgs f 3 (length e)
    else Right ()
  (EMatrix _ _ valVec) <- getRequired "matrix" f 1 e :: ThrowsError EMatrix
  criteriaMatrices <- mapM (\n -> getRequired "matrix" f n e) [2*arg | arg<-[1..(length e) `div` 2]]
  criteria <- mapM (\n -> getRequired "value" f n e) [2*arg+1 | arg<-[1..(length e-1) `div` 2]]
  let matches =  map getLambda criteria -- [EValue -> Bool]
  let dims = map matrixDim criteriaMatrices
  if (allTheSame dims) -- make sure that all ranges have the same dimension
    then do
      let critVecs = map content criteriaMatrices
      -- | Should the ith element be included?
      let include vecs ms i = and $ map (\(f,val) -> f val) $ zip ms $ map (\v -> (V.!) v i) vecs
      -- | Only include values in the value vector where all criteria are met (and function)
      let filteredValVec = V.ifilter (\i _ -> include critVecs matches i) valVec
      Right $ filteredValVec
    else Left $ VAL "Not all ranges in had the same size"

-- | Cast ENumeric vector to Double vector
toDouble :: V.Vector ENumeric -> V.Vector Double
toDouble = V.map f
  where
    f (EValueI i) = fromIntegral i
    f (EValueD d) = d

filterBool :: V.Vector EValue -> V.Vector Bool
filterBool v = V.map (\(EValueB b) -> b) $ V.filter (isJust . boolVal) v
  where
    boolVal :: EValue -> Maybe Bool
    boolVal (EValueB b) = Just b
    boolVal _ = Nothing

-- | Helper; converts an entity to a boolean if possible (uses the function to fold over matrix)
toBool :: (V.Vector Bool -> Bool) -> EEntity -> ThrowsError (Maybe Bool)
toBool _ (EntityVal (EValueB b)) =  Right $ Just b
toBool _ (EntityVal (EValueNum (EValueI 1))) = Right $ Just True
toBool _ (EntityVal (EValueNum (EValueI 0))) = Right $ Just False
toBool f (EntityMatrix m) = do
  (EMatrix c r v) <- matrixError m
  let filtered = filterBool v
  if (V.null filtered)
    then Right Nothing
    else Right $ Just $ f filtered
toBool _ _ = Left $ Default "Argument is not boolean"

collapseBool :: (Bool -> Bool -> Bool) -> EFunc
collapseBool f c e = do
  let vecFold = V.foldl1' f
  args <- compressErrors $ map (toBool vecFold) e
  let bools = catMaybes args
  if (null bools)
    then valToResult $ EValueB False -- return false by default if no bool arguments received
    else valToResult $ EValueB $ foldl1' f bools

--------------------------------------------------------------------------------------------------------------
-- | Excel match parsing

-- | Helper; Given a value (eg string like "(*)"), produces a lambda for equality to an EValue
getLambda :: EValue -> EValue -> Bool
getLambda (EValueS "") EBlank = True
getLambda (EValueS s) v = case (outerComparator s) of
  Nothing -> case v of
    EValueS s' -> criteria s s'
    otherwise  -> False
  Just (f,cs) -> case (valParser cs) of
    Nothing -> False -- couldn't parse matcher
    Just v' -> f v v'
getLambda v1 v2 = v1 == v2

valParser :: String -> Maybe EValue
valParser s = either (\_ -> Nothing) (\(Basic (Var v)) -> Just v) $ parse excelValue "" s

-- | Extracts a possible outer comparator and remaining string ; ">34" -> (>,"34")
outerComparator :: String -> Maybe ((EValue -> EValue -> Bool),String)
outerComparator "" = Nothing
outerComparator ('>':('=':cs)) = Just ((>=),cs)
outerComparator ('>':cs) = Just ((>),cs)
outerComparator ('<':('>':cs)) = Just ((/=),cs)
outerComparator ('<':('=':cs)) = Just ((<=),cs)
outerComparator ('<':cs) = Just ((<),cs)
outerComparator ('=':cs) = Just ((==),cs)
outerComparator _ = Nothing

-- | Given an Excel regex and a test input string, see if they match
criteria :: String -> String -> Bool
criteria regex match = case (parse (stringMatch regex) "" match) of
  Left _ -> False
  Right _ -> True

-- | If this parser suceeds, then the input string matches the argument string
-- | Ignore case when matching strings
stringMatch :: String -> Parser ()
stringMatch "" = eof
stringMatch (c:cs) = case c of
  '~' -> do
    let (c':cs') = cs
    char (toLower c')
    stringMatch cs'
  -- | Keep matching characters until next parser works, but don't consume the recursive input (lookAhead)
  -- | Try is needed because the stringMatch and anyChar parsers overlap
  '*' -> manyTill anyChar (try (lookAhead (stringMatch cs))) >> (stringMatch cs)
  '?' -> anyChar >> (stringMatch cs)
  otherwise -> (char (toLower otherwise)) >> (stringMatch cs)


--------------------------------------------------------------------------------------------------------------
-- | Excel information functions

eIsBlank :: EFunc
eIsBlank c e = do
  e' <- testNumArgs 1 "isblank" e
  valToResult $ EValueB $ blank (head e')

eIsLogical :: EFunc
eIsLogical c e = do
  e' <- testNumArgs 1 "islogical" e
  valToResult $ EValueB $ boolEntity $ head e'

eIsNumber :: EFunc
eIsNumber c e = do
  e' <- testNumArgs 1 "isnumber" e
  valToResult $ EValueB $ numeric $ head e'

eIsError :: EFuncResult
eIsError c r = do
  r' <- testNumArgs 1 "iserror" r
  case (head r) of
    Left  _ ->  valToResult $ EValueB True
    Right _ -> valToResult $ EValueB False

-- | Helpers
boolEntity :: EEntity -> Bool
boolEntity (EntityVal (EValueB _)) = True
boolEntity _ = False

blank :: EEntity -> Bool
blank (EntityVal EBlank) = True
blank _ = False

numeric :: EEntity -> Bool
numeric (EntityVal (EValueNum _)) = True
numeric _ = False

--------------------------------------------------------------------------------------------------------------
-- | Excel logical functions

-- | If the first argument is an error (eval or otherwise), return the second
-- | Eval error will produce a Left, referencing an error cell (eg if A1 is #REF) is a EValueE
eIfError :: EFuncResult
eIfError c r = do
  r' <- testNumArgs 2 "iferror" r
  let errRes = r!!1
  case (head r') of
    Left e -> errRes
    Right (EntityVal (EValueE _)) -> errRes
    otherwise -> head r'

eIf :: EFunc
eIf c e = do
  let f = "if"
  b <- getRequired "bool" f 1 e :: ThrowsError Bool
  v1 <- getRequired "value" f 2 e :: ThrowsError EValue
  v2 <- getRequired "value" f 3 e :: ThrowsError EValue
  if b
    then valToResult v1
    else valToResult v2

-- | Returns the logical inverse of its only argument
eNot :: EFunc
eNot c e = do
  b <- (getRequired "bool" "not" 1 e)::(ThrowsError Bool)
  valToResult $ EValueB (not b)

-- | Returns the logical and of its args, ignoring non-logical arguments/values within matrices
-- | Returns a value error if no logical arguments
eAnd :: EFunc
eAnd = collapseBool (&&)

-- | See eAnd
eOr :: EFunc
eOr = collapseBool (||)

--------------------------------------------------------------------------------------------------------------
-- | Excel lookup and reference functions

-- | Given a column and row (and possibly reference type, abs/relative combination type, and sheet), return string with reference
eAddress :: EFunc
eAddress c e = do
  let f  = "address"
  row <- getRequired "int" f 1 e :: ThrowsError Int
  col <- getRequired "int" f 2 e :: ThrowsError Int
  refType <- getOptional "int" 1 f 3 e :: ThrowsError Int
  a1Bool <- getOptional "bool" True f 4 e :: ThrowsError Bool
  sheet <- getOptional "string" "" f 5 e :: ThrowsError String
  if a1Bool -- A1 style reference
    then do
      ref <- refToString col row refType
      valToResult $ EValueS $ sheet ++ ref
    else do -- R1C1 style reference
      let ref = "R"++(show row)++"C"++(intToCol col)
      valToResult $ EValueS $ sheet ++ ref

-- | Helper for address; produces $A$1 style string from col num, row num, and relative/absolute type
refToString :: Col -> Row -> Int -> ThrowsError String
refToString col row 1 = Right $ "$" ++ (intToCol col) ++ "$" ++ (show row)
refToString col row 2 = Right $ (intToCol col) ++ "$" ++ (show row)
refToString col row 3 = Right $ "$" ++ (intToCol col) ++ (show row)
refToString col row 4 = Right $  (intToCol col)  ++ (show row)
refToString _ _ _ = Left $ VAL "Third argument of ADDRESS is invalid."

-- | TODO: copy from somewhere
-- | Given a column number, produce the column letter (3 -> C)
intToCol :: Col -> String
intToCol a = "A"

-- | Returns the column of a reference; returns a matrix if the input is a range reference
-- | If not in array formula mode, the eval function will automatically return only the top left
-- | No argument = column of current selection
eColumn :: EFunc
eColumn c e = do
  (ERef loc) <- getOptional "ref" (ERef (curLoc c)) "column" 1 e :: ThrowsError ERef
  case loc of
    IndexRef (Index _ (a,b)) -> valToResult $ EValueNum $ EValueI a
    RangeRef (Range _ ((a,b),(c,d))) -> Right $ EntityMatrix $ EMatrix (c-a+1) (d-b+1) (flattenMatrix m)
      where
        m = V.replicate (d-b+1) firstRow
        firstRow = V.map (EValueNum . EValueI) $ V.enumFromN a (c-a+1)

-- | See eColumn
eRow :: EFunc
eRow c e = do
  (ERef loc) <- getOptional "ref" (ERef (curLoc c)) "row" 1 e :: ThrowsError ERef
  case loc of
    IndexRef (Index _ (a,b)) -> valToResult $ EValueNum $ EValueI b
    RangeRef (Range _((a,b),(c,d))) -> Right $ EntityMatrix $ EMatrix (c-a+1) (d-b+1) (flattenMatrix m)
      where
        m = V.map (V.replicate (d-b+1)) colValues
        colValues = V.map (EValueNum . EValueI) $ V.enumFromN a (c-a+1)

-- | If the argument is a string that parses as a reference, return that reference
-- | If the reference is fed into another function, that function will replace the reference (via DB/context) if necessary
eIndirect :: EFunc
eIndirect c e = do
  refString <- getRequired "string" "indirect" 1 e :: ThrowsError String
  a1Bool <- getOptional "bool" True "indirect" 2 e :: ThrowsError Bool
  case (stringToLoc a1Bool refString) of
    Nothing -> Left $ REF "Indirect did not refer to valid reference as first argument"
    Just loc -> return $ EntityRef (ERef loc)

-- | TODO: finish
-- | Given boolean (True = A1, False = R1C1) and string, cast into ASLocation if possible (eg "A$1" -> Index (1,1))
stringToLoc :: Bool -> String -> Maybe ASReference
stringToLoc b s  = Just $ IndexRef $ Index (T.pack "") (1,1)

-- | Takes a reference, height/width/col/row parameters, and returns an offsetted reference
eOffset :: EFunc
eOffset c e = do
  (ERef loc) <- getRequired "ref" "offset" 1 e :: ThrowsError ERef
  rows <- getRequired "int" "offset" 2 e :: ThrowsError Int
  cols <- getRequired "int" "offset" 3 e :: ThrowsError Int
  height <- getOptional "int" (getHeight loc) "offset" 4 e :: ThrowsError Int
  width <- getOptional "int" (getWidth loc) "offset" 5 e :: ThrowsError Int
  let (a,b) = topLeftLoc loc
  let tl = (a+cols,b+rows)
  let loc' = case (height,width) of
                (1,1) -> IndexRef $ Index (shName loc) tl
                (h,w) -> RangeRef $ Range (shName loc) (tl,(a+cols+w-1,b+rows+h-1))
  verifyInBounds loc'

-- | Makes sure that an ASLocation doesn't have negative coordinates etc.
verifyInBounds :: ASReference -> EResult
verifyInBounds l@(IndexRef (Index _ a)) = if tupleOK a
  then locToResult l
  else Left $ Default "Location index out of bounds"
verifyInBounds l@(RangeRef (Range _ (a,b))) = if tupleOK a && tupleOK b
  then locToResult l
  else Left $ Default "Location index out of bounds"

tupleOK :: (Int,Int) -> Bool
tupleOK (a,b) = a > 0 && b > 0


-- | Depending on match type (-1,0,1; 0=equality), return the index (starting at 1) of the matrix
-- | Allowed to use wildcards if val = string and type = 0, doesn't care about upper/lower case for strings,
-- | Return NA if no match
eMatch :: EFunc
eMatch c e = do
  lookupVal <- getRequired "value" "match" 1 e :: ThrowsError EValue
  lookupRange <- getRequired "matrix" "match" 2 e :: ThrowsError EMatrix
  vec <- case (to1D lookupRange) of
    Nothing -> Left $ VAL "Lookup range for MATCH cannot be two dimensional"
    Just v -> if (V.null v)
      then Left $ VAL "Lookup range for MATCH cannot be empty"
      else return v
  lookupType <- getOptional "int" 1 "match" 3 e :: ThrowsError Int
  let matcher = getLambda lookupVal :: (EValue -> Bool)
  case lookupType of
    -- | Type 0 enables regex
    0 -> case (V.findIndex matcher vec) of
      Nothing -> Left $ NA "No match found"
      Just i -> valToResult $ EValueNum $ EValueI i
    1 -> valToResult $ EValueNum $ EValueI $ V.maxIndex $ V.filter (<=lookupVal) vec
    -1 -> valToResult $ EValueNum $ EValueI $ V.minIndex $ V.filter (>=lookupVal) vec
    otherwise -> Left $ VAL $ "Last argument for MATCH must be -1,0, or 1 (default)"

-- | Has a "reference mode" and a "value mode", currently only doing value mode
eIndex :: EFunc
eIndex c e = do
  arr <- getRequired "matrix" "index" 1 e :: ThrowsError EMatrix
  row <- getOptionalMaybe "int" "index" 2 e :: ThrowsError (Maybe Int)
  col <- getOptionalMaybe "int" "index" 3 e :: ThrowsError (Maybe Int)
  (row',col') <- case (row,col) of
    (Nothing,Nothing) -> Left $ VAL "Not enough arguments for Index"
    (a,b) -> Right $ (f a,f b)
      where
        f Nothing = 1
        f (Just i) = i
  indexOrSlice arr row' col'

-- | Row and Col start at 1. Row/col value of 0 = slice
indexOrSlice :: EMatrix -> Int -> Int -> EResult
indexOrSlice m@(EMatrix nCol nRow v) row col
  | row>=nRow || col>=nCol || row<0 || col<0 = err
  | row==0 && col>=1 = Right $ EntityMatrix $ EMatrix 1 nRow vertical
  | row>=1 && col==0 = Right $ EntityMatrix $ EMatrix nCol 1 horizontal
  | row==0 && col==0 = Right $ EntityMatrix m -- whole matrix
  | row>=1 && col>=1 = valToResult $ matrixIndex (col,row) m
    where
      err = Left $ REF $ "Index out of bounds"
      horizontal = V.unsafeSlice ((row-1)*nCol) nCol v
      -- Is an index of v correct for the vertical slice?
      goodVertical i = (i `mod` nCol) == col-1
      vertM = V.imap (\i a -> if (goodVertical i) then (Just a) else Nothing) v
      vertical = V.fromList $ catMaybes $ V.toList vertM


-- | Transpose a matrix
eTranspose :: EFunc
eTranspose c e = do
  m <- getRequired "matrix" "transpose" 1 e :: ThrowsError EMatrix
  let lst = matrixTo2DList m -- [[EValue]]
  let newVec = V.fromList $ concat lst
  return $ EntityMatrix $ EMatrix (emRows m) (emCols m) newVec


--------------------------------------------------------------------------------------------------------------
-- | Excel Math and Trig functions

-- | If a function takes in a numeric value and returns a double, this is a convenient wrapper
oneArgDouble :: (Num a) => String -> (Double -> Double) -> EFunc
oneArgDouble name f c e = do

  num <- getRequired "numeric" name 1 e :: ThrowsError ENumeric
  let ans = case num of
                EValueI i -> f (fromIntegral i)
                EValueD d -> f d
  valToResult $ EValueNum (EValueD ans)

-- | Absolute value
eAbs :: EFunc
eAbs c e = do
  num <- getRequired "numeric" "abs" 1 e :: ThrowsError ENumeric
  valToResult $ EValueNum (abs num)

eExp :: EFunc
eExp = oneArgDouble "exp" exp

ePi :: EFunc
ePi c e = do
  e' <- testNumArgs 0 "pi" e
  valToResult $ EValueNum (EValueD pi)

eSqrtPi :: EFunc
eSqrtPi c e = do
  e' <- testNumArgs 0 "pi" e
  valToResult $ EValueNum (EValueD (sqrt pi))

eSqrt :: EFunc
eSqrt = oneArgDouble "sqrt" sqrt

-- | Finds the product of all of its arguments, decomposing matrices
-- | Built-in product starts folding with ValueD, so we use foldl1'
eProduct :: EFunc
eProduct = collapseNumeric (*)

-- | Finds the sum of all of its arguments, decomposing matrices
-- | Built-in sum starts folding with ValueD, so we use foldl1'
eSum :: EFunc
eSum = collapseNumeric (+)

-- | Sums values in a range that satisfy criteria in another range
eSumIf :: EFunc
eSumIf c e = do
  vec <- ifFunc "sumif" c e
  valToResult $ EValueNum $ sumInt $ filterNum vec

-- | Generalization of sumif to multiple ranges for criteria
eSumIfs :: EFunc
eSumIfs c e = do
  vec <- ifsFunc "sumif" e
  valToResult $ EValueNum $ sumInt $ filterNum vec

-- | Sum of squares of elements
eSumSq :: EFunc
eSumSq = collapseNumeric' (\accum next -> accum + next * next)

eSumx2my2 :: EFunc
eSumx2my2 = zipNumericSum2 (\a b -> a*a-b*b) "sumxmy2"

eSumx2py2 :: EFunc
eSumx2py2 = zipNumericSum2 (\a b -> a*a+b*b) "sumxpy2"

eSumxmy2 :: EFunc
eSumxmy2 = zipNumericSum2 (\a b -> (a-b)*(a-b)) "sumxmy2"

eSumProduct :: EFunc
eSumProduct = zipNumericSum2 (\a b -> a*b) "sumproduct"

--------------------------------------------------------------------------------------------------------------
-- | Statistical vector functions

data Pair = Pair {-# UNPACK #-}!Int {-# UNPACK #-}!Double

-- | Find the mean of a vector, caller's job to make sure vector isn't null
mean :: V.Vector Double ->  Double
mean xs = s / fromIntegral n
    where
      Pair n s       = V.foldl' k (Pair 0 0) xs
      k (Pair n s) x = Pair (n+1) (s+x)

avedev :: V.Vector Double ->  Double
avedev xs = s / fromIntegral n
  where
    m = mean xs
    Pair n s       = V.foldl' k (Pair 0 0) xs
    k (Pair n s) x = Pair (n+1) (s + abs (x-m))

variance ::  V.Vector Double -> Double
variance xs = s / fromIntegral n
  where
    m = mean xs
    Pair n s       = V.foldl' k (Pair 0 0) xs
    k (Pair n s) x = Pair (n+1) (s + (x-m)*(x-m))

variance_s :: V.Vector Double -> Double
variance_s xs = s / fromIntegral (n-1)
  where
    m = mean xs
    Pair n s       = V.foldl' k (Pair 0 0) xs
    k (Pair n s) x = Pair (n+1) (s + (x-m)*(x-m))

sumSqDev :: V.Vector Double -> Double
sumSqDev xs = s
  where
    m = mean xs
    s = V.foldl' k 0 xs
    k s x = s + (x-m)*(x-m)

xyProd :: V.Vector Double -> V.Vector Double -> Double
xyProd xs ys = s
  where
    mx = mean xs
    my = mean ys
    zipped = V.zipWith (\a b -> (a,b)) xs ys
    s = V.foldl' k 0 zipped
    k s (x,y) = s+(x-mx)*(y-my)

covar :: V.Vector Double -> V.Vector Double -> Double
covar xs ys = s / fromIntegral n
  where
    mx = mean xs
    my = mean ys
    zipped = V.zipWith (\a b -> (a,b)) xs ys
    Pair n s = V.foldl' k (Pair 0 0) zipped
    k (Pair n s) (x,y) = Pair (n+1) (s+(x-mx)*(y-my))

--------------------------------------------------------------------------------------------------------------
-- | Excel statistical functions

-- | Compute average deviation of arguments
eAvedev :: EFunc
eAvedev c e = do
  v <- argsToNumVec e
  doubleToResult $ avedev $ toDouble v


-- | Compute average of arguments
eAverage :: EFunc
eAverage c e = do
  v <- argsToNumVec e
  if (V.null v)
    then Left DIV0
    else doubleToResult $ mean $ toDouble v

-- | Large(array,k) = kth largest number in array
eLarge :: EFunc
eLarge c e = do
  (EMatrix c r v) <- getRequired "matrix" "large" 1 e :: ThrowsError EMatrix
  k <- getRequired "int" "large" 2 e :: ThrowsError Int
  let nums = filterNum v
  if (V.null nums)
    then Left $ NA $ "Large received no numeric values in range"
    else do
      -- | Linear algorithm doesn't seem to exist
      let kLargestNum = (reverse $ sort $ V.toList nums) !!(k-1)
      valToResult $ EValueNum kLargestNum

eAverageIf :: EFunc
eAverageIf c e =  do
  vec <- ifFunc "averageif" c e
  if (V.null vec)
    then Left DIV0
    else doubleToResult $ mean $ toDouble $ filterNum $ vec

eAverageIfs :: EFunc
eAverageIfs c e =  do
  vec <- ifsFunc "averageif" e
  if (V.null vec)
    then Left DIV0
    else doubleToResult $ mean $ toDouble $ filterNum $ vec

-- | Used for count-type functions that don't care about numeric values, and include errors
argsToVec :: [EEntity] -> V.Vector EValue
argsToVec es = V.concat vecs
  where
    vecs = map argToVec es
    argToVec :: EEntity -> V.Vector EValue
    argToVec (EntityVal EBlank) = V.empty -- don't include blanks
    argToVec (EntityVal v) = V.singleton v -- include errors
    argToVec (EntityMatrix (EMatrix _ _ v)) = v

eCount ::  EFunc
eCount c e = intToResult $ V.length $ filterNum $ argsToVec e

eCountIf :: EFunc
eCountIf c e =  do
  vec <- ifFunc "countif" c e
  intToResult $ V.length vec

eCountIfs :: EFunc
eCountIfs c e =  do
  -- let the "sum range" argument be a duplicate of the first argument
  if (length e <= 0)
    then Left $ RequiredArgMissing "countif" 1 
    else do 
      vec <- ifsFunc "countifs" $ (head e):e
      intToResult $ V.length vec

eBinomDist ::  EFunc
eBinomDist c e = do
  succ'  <- getRequired "int" "binom.dist" 1 e   :: ThrowsError Int
  trials <- getRequired "int" "binom.dist" 2 e  :: ThrowsError Int
  p' <- getRequired "double" "binom.dist" 3 e :: ThrowsError Double
  cum <- getRequired "bool" "binom.dist" 4 e :: ThrowsError Bool
  p <- checkProb p'
  succ <- checkNumberBinom succ' trials
  let dist = SDB.binomial trials p
  if cum -- cumulative binomial
    then doubleToResult $ SD.cumulative dist (fromIntegral succ)
    else doubleToResult $ SD.probability dist succ

eNormDist :: EFunc
eNormDist c e = do
  x <- getRequired "double" "normdist" 1 e :: ThrowsError Double
  mu <- getRequired "double" "normdist" 2 e :: ThrowsError Double
  stdev <- getRequired "double" "normdist" 3 e :: ThrowsError Double
  cum <- getRequired "bool" "normdist" 4 e :: ThrowsError Bool
  if stdev < 0
    then Left $ NUM "Standard deviation for NORMDIST is less than zero"
    else do
      let dist = SDN.normalDistr mu stdev
      if cum
        then doubleToResult $ SD.cumulative dist x
        else doubleToResult $ SD.density dist x

eNormInv :: EFunc
eNormInv c e = do
  p' <- getRequired "double" "norminv" 1 e :: ThrowsError Double
  mu <- getRequired "double" "norminv" 2 e :: ThrowsError Double
  stdev <- getRequired "double" "norminv" 3 e :: ThrowsError Double
  if stdev < 0
    then Left $ NUM "Standard deviation for NORMINV is less than zero"
    else do
      p <- checkProb p'
      let dist = SDN.normalDistr mu stdev
      doubleToResult $ SD.quantile dist p

checkProb :: Double -> ThrowsError Double
checkProb d
  | d < 0 = Left $ NUM "Probability less than 0"
  | d > 1 = Left $ NUM "Probability greater than 1"
  | otherwise = Right d

checkNumberBinom :: Int -> Int -> ThrowsError Int
checkNumberBinom num trials
  | num > trials =  Left $ NUM "Number of successes is greater than number of trials"
  | trials < 0 = Left $ NUM $ "Number of trials is negative"
  | num  < 0 = Left $ NUM "Number of successes must be positive"
  | otherwise = Right $ num

-- | Find the minimum, 0 as default
eMin :: EFunc
eMin c e = do
  v <- argsToNumVec e
  if (V.null v)
    then valToResult $ EValueNum $ EValueI 0
    else valToResult $ EValueNum $ V.minimum v

-- | Find the maximum, 0 as default
eMax :: EFunc
eMax c e = do
  v <- argsToNumVec e
  if (V.null v)
    then valToResult $ EValueNum $ EValueI 0
    else valToResult $ EValueNum $ V.minimum v

eMedian :: EFunc
eMedian c e = do
  v <- argsToNumVec e
  valToResult $ EValueNum $ median $ V.toList v

eModeMult :: EFunc
eModeMult c e = do
  v <- argsToNumVec e
  let lst = V.toList v
  case (mode lst) of
    Nothing -> Left $ NA $ "All values are distinct for MODE.MULT"
    Just _ -> do
      let ms = map snd $ modes lst
      Right $ EntityMatrix $ EMatrix 1 (length ms) (V.map EValueNum (V.fromList ms))

eModeSngl :: EFunc
eModeSngl c e = do
  v <- argsToNumVec e
  let m = mode $ V.toList v
  case m of
    Nothing -> Left $ NA $ "All values are distinct for MODE.SNGL"
    Just mode' -> valToResult $ EValueNum mode'

-- |Median
median :: [ENumeric] -> ENumeric
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = avg $ take 2 $ drop i x'
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x
                        avg (p:[q]) = (p+q)*(EValueD 0.5)

-- |Modes returns a sorted list of modes in descending order
modes :: (Ord a) => [a] -> [(Int, a)]
modes xs = sortBy (comparing $ negate.fst) $ map (\x->(length x, head x)) $ (group.sort) xs

-- |Mode returns the mode of the list, otherwise Nothing
mode :: (Ord a) => [a] -> Maybe a
mode xs = case m of
            [] -> Nothing
            otherwise -> Just . snd $ head m
    where m = filter (\(a,b) -> a > 1) (modes xs)

eCorrel :: EFunc
eCorrel c e = do
  (EMatrix _ _ arr1) <- getRequired "matrix" "correl" 1 e  :: ThrowsError EMatrix
  (EMatrix _ _ arr2) <- getRequired "matrix" "correl" 2 e  :: ThrowsError EMatrix
  if (V.length arr1 /= V.length arr2)
    then Left $ NA $ "Arguments for CORREL had different lengths"
    else do
      let v1 = toDouble $ filterNum arr1
      let v2 = toDouble $ filterNum arr2
      if (V.length v1 /= V.length v2)
        then Left $ DIV0 -- Excel does this
        else do
          let cov = xyProd v1 v2
          let sx = sumSqDev v1
          let sy = sumSqDev v2
          if sx == 0 || sy == 0
            then Left $ DIV0
            else doubleToResult $ cov / (sqrt (sx * sy))

ePearson :: EFunc
ePearson = eCorrel

eCoVar :: EFunc
eCoVar c e = do 
  (EMatrix _ _ arr1) <- getRequired "matrix" "covar" 1 e  :: ThrowsError EMatrix
  (EMatrix _ _ arr2) <- getRequired "matrix" "covar" 2 e  :: ThrowsError EMatrix
  if (V.length arr1 /= V.length arr2)
    then Left $ NA $ "Arguments for COVAR had different lengths"
    else do
      let v1 = toDouble $ filterNum arr1
      let v2 = toDouble $ filterNum arr2
      if ((V.length v1 /= V.length v2 ) || (V.length v1 == 0))
        then Left $ DIV0 -- Excel does this
        else doubleToResult $ covar v1 v2

genericRank ::  String -> ([(ENumeric,Int)] -> [(ENumeric,EValue)]) -> EFunc
genericRank f rankGroup c e = do
  x <- getRequired "numeric" f 1 e :: ThrowsError ENumeric
  (EMatrix _ _ v) <- getRequired "matrix" f 2 e :: ThrowsError EMatrix
  order <- getOptional "int" 0 f 3 e :: ThrowsError Int
  let lst = V.toList $ filterNum v
  let sorted = if (order == 0)
                 then sort lst
                 else reverse $ sort lst
  -- | Enumerate ranks, then group by equal ranks, then determine the common rank of each group, and concatenate
  let ranks = concat $ map rankGroup $ groupBy (\x y -> (fst x == fst y)) $ zip sorted [1..]
  case (lookup x ranks) of
    Nothing -> Left $ VAL $ "Couldn't find given element for " ++ f
    Just r -> valToResult r

-- | Rank an element of a list in ascending/descending order, give equal ranks to ties
eRankEq :: EFunc
eRankEq = genericRank "rankeq" groupEq

-- | Rank an element of a list in ascending/descending order, give equal ranks to ties
eRankAvg :: EFunc
eRankAvg = genericRank "rankavg" groupAvg

-- | Helper for rankeq
groupEq :: [(ENumeric,Int)] -> [(ENumeric,EValue)]
groupEq group = zip (map fst group) (repeat commonRank)
  where
    ranks =  map snd group
    commonRank = EValueNum $ EValueI $ minimum ranks

-- | Helper for rankavg
groupAvg :: [(ENumeric,Int)] -> [(ENumeric,EValue)]
groupAvg group = zip (map fst group) (repeat commonRank)
  where
    ranks =  map snd group
    commonRank = EValueNum $ EValueD $ (fromIntegral (sum ranks))/(fromIntegral (length ranks))

-- | Slope of the regression line
eSlope :: EFunc
eSlope c e = do
  let f = "slope"
  (EMatrix _ _ v1) <- getRequired "matrix" f 1 e :: ThrowsError EMatrix
  (EMatrix _ _ v2) <- getRequired "matrix" f 2 e :: ThrowsError EMatrix
  if (V.length v1 /= V.length v2) || V.length v1 == 0 || V.length v2 == 0
    then Left $ NA "Invalid dimensions for SLOPE arguments"
    else do
      let y = toDouble $ filterNum v1
      let denom = sumSqDev y
      let x = toDouble $ filterNum v2
      if V.length x /= V.length y || denom == 0
        then Left $ DIV0
        else do
          let numerator = xyProd x y
          doubleToResult $ numerator/denom

eVarP :: EFunc
eVarP c e = do
  v <- argsToNumVec e
  let nums = toDouble v
  if (V.null nums)
    then Left $ DIV0
    else doubleToResult $ variance nums

eVarS :: EFunc
eVarS c e = do
  v <- argsToNumVec e
  let nums = toDouble v
  if (V.length nums <=1)
    then Left $ DIV0
    else doubleToResult $ variance_s nums


eStdP :: EFunc
eStdP c e = do
  v <- argsToNumVec e
  let nums = toDouble v
  if (V.null nums)
    then Left $ DIV0
    else doubleToResult $ sqrt $ variance nums

eStdS :: EFunc
eStdS c e = do
  v <- argsToNumVec e
  let nums = toDouble v
  if (V.length nums <=1)
    then Left $ DIV0
    else doubleToResult $ sqrt $ variance_s nums

--------------------------------------------------------------------------------------------------------------
-- | Excel text functions

-- | Find position of one string within another; casing on case-sensitivity
textFind :: Bool -> EFunc
textFind caseSensitive c e = do
  let f = "find"
  findText' <- getRequired "string" f 1 e :: ThrowsError String
  withinText' <- getRequired "string" f 2 e :: ThrowsError String
  let findText = normString caseSensitive findText'
  let withinText = normString caseSensitive withinText'
  -- | Optional starting position, starting from 1 (default)
  startNum <- getOptional "int" 1 f 3 e :: ThrowsError Int
  if startNum <=0 || startNum >= (length withinText)
    then Left $ VAL "Starting position out of bounds for FIND"
    else do
      if findText == ""
        then intToResult 1
        else do
          let shiftedWithin = drop (startNum-1) withinText
          case (elemIndex findText (tails shiftedWithin)) of
            Nothing -> Left $ VAL "Couldn't find smaller string in larger for FIND"
            -- | Answer is from beginning of original withinText (add one for 1-indexing)
            Just pos -> intToResult $ pos + 1 + startNum

-- | Helper for above
normString :: Bool -> String -> String
normString caseSensitive s
  | caseSensitive = s
  | otherwise = map toLower s

eFind :: EFunc
eFind = textFind True

eSearch :: EFunc
eSearch = textFind False

-- | Find the length of a string
eLen :: EFunc
eLen c e = do
  str <- getRequired "string" "len" 1 e :: ThrowsError String
  intToResult $ length str


-- |  Convert text to lowercase
eLower ::  EFunc
eLower c e = do
  str <- getRequired "string" "len" 1 e :: ThrowsError String
  stringResult $ map toLower str

-- |  Convert text to uppercase
eUpper ::  EFunc
eUpper c e = do
  str <- getRequired "string" "len" 1 e :: ThrowsError String
  stringResult $ map toUpper str

-- | Repeat a string n times
eRept ::  EFunc
eRept c e = do
  let f = "rept"
  str <- getRequired "string" f 1 e :: ThrowsError String
  n <- getRequired "int" f 2 e :: ThrowsError Int
  stringResult $ concat $ replicate n str

-- | Rightmost characters of a string
eRight :: EFunc
eRight c e = do
  let f =  "right"
  str <- getRequired "string" f 1 e :: ThrowsError String
  n <- getOptional "int" 1 f 2 e :: ThrowsError Int
  if n < 0
    then Left $ VAL "Number of right characters cannot be negative for RIGHT"
    else do
      let n' = min (length str) n -- Return whole string if n is large
      stringResult $ drop ((length str)-n') str

-- | Trim whitespace from a string
eTrim :: EFunc
eTrim c e = do
  str <- getRequired "string" "trim" 1 e :: ThrowsError String
  stringResult $ T.unpack $ T.strip $ T.pack str

-- | Substitute new string for old string in larger string
eSubstitute :: EFunc
eSubstitute c e = do
  let f = "substitute"
  str <- getRequired "string" f 1 e :: ThrowsError String
  old <- getRequired "string" f 2 e :: ThrowsError String
  new <- getRequired "string" f 3 e :: ThrowsError String
  n <- getOptional "int" 0 f 4 e :: ThrowsError Int -- which occurrence of old to replace (default = all)
  if n == 0
    then stringResult $ SU.replace old new str
    else do
      let parts = SU.split old str
      let (before,after) = splitAt n parts
      stringResult $ (concat before) ++ new ++ (concat after)


--------------------------------------------------------------------------------------------------------------
-- | Excel infix functions

numInfix :: String -> (ENumeric -> ENumeric -> ENumeric) -> EFunc
numInfix name f c e = do 
  a <- getRequired "numeric" name 1 e :: ThrowsError ENumeric
  b <- getRequired "numeric" name 2 e :: ThrowsError ENumeric
  valToResult $ EValueNum $ f a b

isZero :: ENumeric -> Bool 
isZero (EValueD 0) = True
isZero (EValueI 0) = True
isZero _ = False

isNonnegative :: ENumeric -> Bool
isNonnegative (EValueD a) = (a >= 0)
isNonnegative (EValueI a) = (a >= 0)

eAdd :: EFunc
eAdd = numInfix "+" (+)

eMinus :: EFunc
eMinus = numInfix "-" (-)

eMult :: EFunc
eMult = numInfix "*" (*)

eDivide :: EFunc
eDivide c e = do
  a <- getRequired "numeric" "/" 1 e :: ThrowsError ENumeric
  b <- getRequired "numeric" "/" 2 e :: ThrowsError ENumeric
  if (isZero b)
    then Left DIV0
    else valToResult $ EValueNum $ a / b

ePower :: EFunc
ePower c e = do
  a <- getRequired "numeric" "^" 1 e :: ThrowsError ENumeric
  b <- getRequired "numeric" "^" 2 e :: ThrowsError ENumeric
  if (isZero a && isZero b) 
    then Left ZeroToTheZero
    else case b of 
      EValueI _ -> valToResult $ EValueNum $ intExp a b
      EValueD _ -> if isNonnegative a 
        then valToResult $ EValueNum $ floatExp a b
        else Left NegExpBaseWithFloatingExp

boolInfix :: String -> (EValue -> EValue -> Bool) -> EFunc
boolInfix name f c e = do 
  a <- getRequired "value" name 1 e :: ThrowsError EValue
  b <- getRequired "value" name 2 e :: ThrowsError EValue
  valToResult $ EValueB $ f a b

eEquals :: EFunc
eEquals = boolInfix "=" (==)

eNotEquals :: EFunc
eNotEquals = boolInfix "<>" (/=)

eGreater :: EFunc
eGreater = boolInfix ">" (>)

eLess :: EFunc
eLess = boolInfix "<" (<)

eGreaterE :: EFunc
eGreaterE = boolInfix ">=" (>=)

eLessE :: EFunc
eLessE = boolInfix "<=" (<=)

eSpace :: EFunc
eSpace c e = do
  let f = "intersect(space)"
  (ERef l1) <- getRequired "ref" f 1 e :: ThrowsError ERef
  (ERef l2) <- getRequired "ref" f 2 e :: ThrowsError ERef
  case (locIntersect l1 l2) of
    Nothing ->  Left $ CannotIntersectRefs
    Just l -> locToResult l

eAmpersand :: EFunc
eAmpersand c e = do
  let f = "&"
  str1 <- getRequired "string" f 1 e :: ThrowsError String
  str2 <- getRequired "string" f 2 e :: ThrowsError String
  stringResult $ str1 ++ str2

--------------------------------------------------------------------------------------------------------------

