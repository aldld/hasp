module Error
( Error(..)
) where

data Error = SyntaxError String deriving (Show)
