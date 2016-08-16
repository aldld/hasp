-- |Internal representation of data types in hasp. The types in this file differ
-- from those defined in Expressions.hs in that Expr represents syntactic forms
-- as they appear in hasp source code, whereas HData represnts hasp values, i.e.
-- results of computations.

module DataTypes
( HData(..)
, HNum(..)
, Env(..)
, emptyEnv
, toMap
) where

import qualified Data.Map as Map

import Expressions
import Error

data HNum = HInt Integer
          | HFloat Float
          deriving (Eq)

instance Ord HNum where
    (HInt x) `compare` (HInt y) = x `compare` y
    (HInt x) `compare` (HFloat y) = (fromIntegral x) `compare` y
    (HFloat x) `compare` (HInt y) = x `compare` (fromIntegral y)
    (HFloat x) `compare` (HFloat y) = x `compare` y

instance Num HNum where
    negate (HInt x) = HInt (negate x)
    negate (HFloat x) = HFloat (negate x)

    abs (HInt x) = HInt (abs x)
    abs (HFloat x) = HFloat (abs x)

    signum (HInt x) = HInt (signum x)
    signum (HFloat x) = HFloat (signum x)

    (HInt x) + (HInt y) = HInt (x + y)
    (HInt x) + (HFloat y) = HFloat ((fromIntegral x) + y)
    (HFloat x) + (HInt y) = HFloat (x + (fromIntegral y))
    (HFloat x) + (HFloat y) = HFloat (x + y)

    (HInt x) - (HInt y) = HInt (x - y)
    (HInt x) - (HFloat y) = HFloat ((fromIntegral x) - y)
    (HFloat x) - (HInt y) = HFloat (x - (fromIntegral y))
    (HFloat x) - (HFloat y) = HFloat (x - y)

    (HInt x) * (HInt y) = HInt (x * y)
    (HInt x) * (HFloat y) = HFloat ((fromIntegral x) * y)
    (HFloat x) * (HInt y) = HFloat (x * (fromIntegral y))
    (HFloat x) * (HFloat y) = HFloat (x * y)

    fromInteger = HInt

instance Show HNum where
    show (HInt x) = show x
    show (HFloat x) = show x


data HData = HN HNum
           | HBool Bool
           | HString String
           | HList [HData]
           | HQuote Expr
           | HFunc Env (Env -> [HData] -> ThrowsError HData)

instance Eq HData where
    (HFunc _ _) == _ = False
    _ == (HFunc _ _) = False

    (HN x) == (HN y) = x == y

    x == y = show x == show y -- Kinda hacky but good enough for now.


instance Show HData where
    show (HN num) = show num
    show (HBool bool) = show bool
    show (HString string) = show string
    show (HList list) = "(" ++ unwords (map show list) ++ ")"
    show (HQuote expr) = "'" ++ (show expr)
    show (HFunc _ _) = "procedure"

data Env = Env (Map.Map Identifier HData)

emptyEnv :: Env
emptyEnv = Env (Map.empty)

toMap :: Env -> Map.Map Identifier HData
toMap (Env m) = m
