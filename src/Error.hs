-- |Data type for representing errors in the execution of hasp programs, as well
-- as common error messages.

module Error
( Error(..)
, errNotNum
, errTooFewArgs
, errNumArgs
, errEmptyList
, errWrongType
) where

data Error = Error String
           | SyntaxError String
           | NameError String
           | TypeError String

instance Show Error where
    show (Error msg) = "Error: " ++ msg
    show (SyntaxError msg) = "SyntaxError: " ++ msg
    show (NameError msg) = "NameError: " ++ msg
    show (TypeError msg) = "TypeError: " ++ msg

errNotNum :: String -> Error
errNotNum x = TypeError $ "Value of `" ++ x ++ "` is not numeric"

errTooFewArgs :: Error
errTooFewArgs = TypeError "Too few arguments"

errNumArgs :: Int -> Int -> Error
errNumArgs expected actual =
    TypeError $ "Invalid number of arguments.\n\tExpected " ++
        show expected ++ " arguments, got " ++ show actual

errEmptyList :: Error
errEmptyList = Error "Empty list"

errWrongType :: Error
errWrongType = Error "Wrong type"
