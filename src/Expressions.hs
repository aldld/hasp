-- Data types representing structured syntactic expressions in hasp, used for
-- defining the abstract syntax tree of a hasp program.

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
          | Quote Expr
          deriving (Show)
