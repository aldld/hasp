-- |Data types representing structured syntactic expressions in hasp, used for
-- defining the abstract syntax tree of a hasp program.

module Expressions
( Atomic(..)
, Expr(..)
, Identifier
) where

type Identifier = String
data Atomic = StringLiteral String
            | IntLiteral Integer
            | FloatLiteral Float
            | BoolLiteral Bool
            | Id Identifier
            deriving (Show)

data Expr = Atom Atomic
          | List [Expr]
          | Quote Expr
          deriving (Show)
