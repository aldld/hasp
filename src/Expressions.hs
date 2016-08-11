module Expressions
( Symbol(..)
, Expr(..)
, VariableName
) where

type VariableName = String
data Symbol = StringLiteral String
            | IntLiteral Int
            | FloatLiteral Float
            | BoolLiteral Bool
            | Var VariableName
            deriving (Show)

data Expr = Atom Symbol
          | List [Expr]
          deriving (Show)
