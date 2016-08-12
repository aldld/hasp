-- Internal representation of data types in hasp. The types in this file differ
-- from those defined in Expressions.hs in that Expr represents syntactic forms
-- as they appear in hasp source code, whereas HData represnts hasp values, i.e.
-- results of computations.

module DataTypes
( HData(..)
, Env(..)
) where

import qualified Data.Map as Map

import Expressions
import Error

data HData = HInt Int
           | HFloat Float
           | HBool Bool
           | HString String
           | HList [HData]
           | HQuote Expr
           | HFunc Env Int ([HData] -> Either Error HData)

instance Show HData where
    show (HInt int) = show int
    show (HFloat float) = show float
    show (HBool bool) = show bool
    show (HString string) = show string
    show (HList list) = show list
    show (HQuote expr) = "'" ++ (show expr)
    show (HFunc e n f) = "procedure"


data Env = Env (Map.Map String HData) deriving (Show)

