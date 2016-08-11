module Expressions
( Literal(..)
, Symbol(..)
, Expr(..)
, VariableName
) where

data Literal = StringLiteral String
             | IntLiteral Int
             | FloatLiteral Float
             | BoolLiteral Bool
             deriving (Show)

type VariableName = String
data Symbol = Const Literal
            | Var VariableName
            deriving (Show)

data Expr = Atom Symbol
          | List [Expr]
          deriving (Show)
