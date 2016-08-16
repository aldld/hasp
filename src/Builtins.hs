-- |Some common functions that are available to all hasp programs by default.
--
-- Note that this does not include syntactic forms such as if, define, lambda,
-- and and (since they require behaviour such as short circuiting and mutation
-- that are not available to hasp functions), as well as standard library
-- functions that can be more easily defined in the hasp language.

module Builtins
( globalEnv
, numericFold
, minusHNum
, numericBinOp
, numericUnaryOp
, numericBinPred
, list
, cons
, car
, cdr
) where

import qualified Data.Map as Map

import Error
import DataTypes

globalEnv :: Env
globalEnv = Env $ Map.fromList
    [ ("+", numericFold (+) 0)
    , ("*", numericFold (*) 1)
    , ("-", minusHNum)
    , ("/", numericBinOp divideHNum)
    , ("quotient", numericBinOp divHNum)
    , ("modulo", numericBinOp modHNum)
    , ("abs", numericUnaryOp abs)
    , ("sgn", numericUnaryOp signum)
    , ("eq?", numericBinPred (==))
    , ("=", numericBinPred (==))
    , ("<", numericBinPred (<))
    , ("<=", numericBinPred (<=))
    , (">", numericBinPred (>))
    , (">=", numericBinPred (>=))
    , ("!=", numericBinPred (/=))
    , ("list", list)
    , ("cons", cons)
    , ("car", car)
    , ("cdr", cdr)
    , ("empty?", testEmptyList) ]

-- Builtin numeric operations

foldlHNum :: (HNum -> HNum -> HNum) -> HNum -> [HData] -> ThrowsError HNum
foldlHNum _ x0 [] = return x0
foldlHNum f x0 ((HN x):xs) = foldlHNum f (x0 `f` x) xs 
foldlHNum f x0 (x:xs) = throw . errNotNum $ show x

foldlHNum1 :: (HNum -> HNum -> HNum) -> [HData] -> ThrowsError HNum
foldlHNum1 _ [] = throw errTooFewArgs
foldlHNum1 f ((HN x):xs) = foldlHNum f x xs
foldlHNum1 f (x:xs) = throw . errNotNum $ show x

numericFold :: (HNum -> HNum -> HNum) -> HNum -> HData
numericFold op x0 = HFunc emptyEnv $ \_ args -> do
    result <- foldlHNum op x0 args
    return $ HN result

minusHNum :: HData
minusHNum = HFunc emptyEnv $ \_ args ->
    case args of
        [HN x] -> return . HN $ negate x
        _ -> do
            result <- foldlHNum1 (-) args
            return $ HN result

divideByZeroError :: HaspError
divideByZeroError = Error "Division by zero"

divideHNum :: HNum -> HNum -> ThrowsError HNum
_ `divideHNum` (HInt 0) = throw divideByZeroError
_ `divideHNum` (HFloat 0) = throw divideByZeroError

(HInt x) `divideHNum` (HInt y) =
    return $ HFloat ((fromIntegral x) / (fromIntegral y))
(HInt x) `divideHNum` (HFloat y) = return $ HFloat ((fromIntegral x) / y)
(HFloat x) `divideHNum` (HInt y) = return $ HFloat (x / (fromIntegral y))
(HFloat x) `divideHNum` (HFloat y) = return $ HFloat (x / y)

numericBinOp :: (HNum -> HNum -> ThrowsError HNum) -> HData
numericBinOp op =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HN x, HN y] -> do
                result <- x `op` y
                return $ HN result
            [x, HN _] -> throw . errNotNum $ show x
            [HN _, y] -> throw . errNotNum $ show y
            [x, _] -> throw . errNotNum $ show x
            _ -> throw $ errNumArgs 2 (length args)

numericBinPred :: (HNum -> HNum -> Bool) -> HData
numericBinPred op =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HN x, HN y] -> return . HBool $ x `op` y
            [x, HN _] -> throw . errNotNum $ show x
            [HN _, y] -> throw . errNotNum $ show y
            [x, _] -> throw . errNotNum $ show x
            _ -> throw $ errNumArgs 2 (length args)

notIntegerError :: HData -> HaspError
notIntegerError x =
    TypeError $ "Value `" ++ show x ++ "` is not of type Integer."

divHNum :: HNum -> HNum -> ThrowsError HNum
_ `divHNum` (HInt 0) = throw divideByZeroError

_ `divHNum` y@(HFloat _) = throw $ notIntegerError (HN y)
x@(HFloat _) `divHNum` _ = throw $ notIntegerError (HN x)

(HInt x) `divHNum` (HInt y) = return $ HInt (x `div` y)

modHNum :: HNum -> HNum -> ThrowsError HNum
_ `modHNum` (HInt 0) = throw divideByZeroError

_ `modHNum` y@(HFloat _) = throw $ notIntegerError (HN y)
x@(HFloat _) `modHNum` _ = throw $ notIntegerError (HN x)

(HInt x) `modHNum` (HInt y) = return $ HInt (x `mod` y)

numericUnaryOp :: (HNum -> HNum) -> HData
numericUnaryOp f =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HN x] -> return . HN $ f x
            [_]    -> throw errWrongType
            _      -> throw $ errNumArgs 1 (length args)

-- Builtin list operations

list :: HData
list = HFunc emptyEnv $ \_ args -> return $ HList args

-- TODO: Properly support dotted lists.
cons :: HData
cons =
    HFunc emptyEnv $ \_ args ->
        case args of
            [x, HList xs] -> return $ HList (x:xs)
            [_, _] -> throw errWrongType
            _ -> throw $ errNumArgs 2 (length args)

car :: HData
car =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HList []] -> throw errEmptyList
            [HList (x:_)] -> return x
            [_] -> throw errWrongType
            _ -> throw $ errNumArgs 1 (length args)

cdr :: HData
cdr =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HList []] -> throw errEmptyList
            [HList (_:xs)] -> return $ HList xs
            [_] -> throw errWrongType
            _ -> throw $ errNumArgs 1 (length args)

testEmptyList :: HData
testEmptyList =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HList []] -> return $ HBool True
            [HList (_:_)] -> return $ HBool False
            [_] -> throw errWrongType
            _ -> throw $ errNumArgs 1 (length args)

