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

foldlHNum :: (HNum -> HNum -> HNum) -> HNum -> [HData] -> Either Error HNum
foldlHNum _ x0 [] = Right x0
foldlHNum f x0 ((HN x):xs) = foldlHNum f (x0 `f` x) xs 
foldlHNum f x0 (x:xs) = Left . errNotNum $ show x

foldlHNum1 :: (HNum -> HNum -> HNum) -> [HData] -> Either Error HNum
foldlHNum1 _ [] = Left errTooFewArgs
foldlHNum1 f ((HN x):xs) = foldlHNum f x xs
foldlHNum1 f (x:xs) = Left . errNotNum $ show x

numericFold :: (HNum -> HNum -> HNum) -> HNum -> HData
numericFold op x0 = HFunc emptyEnv $ \_ args ->
    case foldlHNum op x0 args of
        Left err -> Left err
        Right result -> Right $ HN result

minusHNum :: HData
minusHNum = HFunc emptyEnv $ \_ args ->
    case args of
        [HN x] -> Right . HN $ negate x
        _ -> case foldlHNum1 (-) args of
            Left err -> Left err
            Right result -> Right $ HN result

divideByZeroError :: Error
divideByZeroError = Error "Division by zero"

divideHNum :: HNum -> HNum -> Either Error HNum
_ `divideHNum` (HInt 0) = Left divideByZeroError
_ `divideHNum` (HFloat 0) = Left divideByZeroError

(HInt x) `divideHNum` (HInt y) =
    Right $ HFloat ((fromIntegral x) / (fromIntegral y))
(HInt x) `divideHNum` (HFloat y) = Right $ HFloat ((fromIntegral x) / y)
(HFloat x) `divideHNum` (HInt y) = Right $ HFloat (x / (fromIntegral y))
(HFloat x) `divideHNum` (HFloat y) = Right $ HFloat (x / y)

numericBinOp :: (HNum -> HNum -> Either Error HNum) -> HData
numericBinOp op =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HN x, HN y] ->
                case x `op` y of
                    Left err -> Left err
                    Right result -> Right $ HN result
            [x, HN _] -> Left . errNotNum $ show x
            [HN _, y] -> Left . errNotNum $ show y
            [x, _] -> Left . errNotNum $ show x
            _ -> Left $ errNumArgs 2 (length args)

numericBinPred :: (HNum -> HNum -> Bool) -> HData
numericBinPred op =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HN x, HN y] -> Right . HBool $ x `op` y
            [x, HN _] -> Left . errNotNum $ show x
            [HN _, y] -> Left . errNotNum $ show y
            [x, _] -> Left . errNotNum $ show x
            _ -> Left $ errNumArgs 2 (length args)

-- TODO: Unify the way in which type errors are handled.
notIntegerError :: HData -> Error
notIntegerError x =
    TypeError $ "Value `" ++ show x ++ "` is not of type Integer."

divHNum :: HNum -> HNum -> Either Error HNum
_ `divHNum` (HInt 0) = Left divideByZeroError

_ `divHNum` y@(HFloat _) = Left $ notIntegerError (HN y)
x@(HFloat _) `divHNum` _ = Left $ notIntegerError (HN x)

(HInt x) `divHNum` (HInt y) = Right $ HInt (x `div` y)

modHNum :: HNum -> HNum -> Either Error HNum
_ `modHNum` (HInt 0) = Left divideByZeroError

_ `modHNum` y@(HFloat _) = Left $ notIntegerError (HN y)
x@(HFloat _) `modHNum` _ = Left $ notIntegerError (HN x)

(HInt x) `modHNum` (HInt y) = Right $ HInt (x `mod` y)

numericUnaryOp :: (HNum -> HNum) -> HData
numericUnaryOp f =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HN x] -> Right . HN $ f x
            [_]    -> Left errWrongType
            _      -> Left $ errNumArgs 1 (length args)

-- Builtin list operations

list :: HData
list = HFunc emptyEnv $ \_ args -> Right $ HList args

-- TODO: Properly support dotted lists.
cons :: HData
cons =
    HFunc emptyEnv $ \_ args ->
        case args of
            [x, HList xs] -> Right $ HList (x:xs)
            [_, _] -> Left errWrongType
            _ -> Left $ errNumArgs 1 (length args)

car :: HData
car =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HList []] -> Left errEmptyList
            [HList (x:_)] -> Right x
            [_] -> Left errWrongType
            _ -> Left $ errNumArgs 1 (length args)

cdr :: HData
cdr =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HList []] -> Left errEmptyList
            [HList (_:xs)] -> Right $ HList xs
            [_] -> Left errWrongType
            _ -> Left $ errNumArgs 1 (length args)

testEmptyList :: HData
testEmptyList =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HList []] -> Right $ HBool True
            [HList (_:_)] -> Right $ HBool False
            [_] -> Left errWrongType
            _ -> Left $ errNumArgs 1 (length args)

