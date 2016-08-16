-- |Data type for representing errors in the execution of hasp programs, as well
-- as common error messages.

module Error
( HaspError(..)
, ThrowsError(..)
, throw
, unpackVal
, errNotNum
, errTooFewArgs
, errNumArgs
, errEmptyList
, errWrongType
) where

import System.Console.Haskeline
import Control.Monad

data HaspError = Error String
               | SyntaxError String
               | NameError String
               | TypeError String
               | BadFormError String
               | NotFunctionError String

instance Show HaspError where
    show (Error msg) = "Error: " ++ msg
    show (SyntaxError msg) = "SyntaxError: " ++ msg
    show (NameError msg) = "NameError: " ++ msg
    show (TypeError msg) = "TypeError: " ++ msg
    show (BadFormError msg) = "BadFormError: " ++ msg
    show (NotFunctionError msg) = "NotFunctionError: " ++ msg

newtype ThrowsError a = TE (Either HaspError a)

instance Functor ThrowsError where
    fmap _ (TE (Left err)) = TE $ Left err
    fmap f (TE (Right val)) = TE . Right $ f val

instance Applicative ThrowsError where
    pure = TE . Right
    (TE (Left err)) <*> _ = (TE (Left err))
    (TE (Right f)) <*> x = fmap f x

instance Monad ThrowsError where
    (TE te) >>= f =
        case te of
            Left err -> TE (Left err)
            Right val -> f val

    return val = TE $ Right val
    fail err = TE . Left $ Error err

throw :: HaspError -> ThrowsError a
throw err = TE $ Left err

unpackVal :: ThrowsError a -> Either HaspError a
unpackVal (TE val) = val

-- Some common error messages.

errNotNum :: String -> HaspError
errNotNum x = TypeError $ "Value of `" ++ x ++ "` is not numeric"

errTooFewArgs :: HaspError
errTooFewArgs = TypeError "Too few arguments"

errNumArgs :: Int -> Int -> HaspError
errNumArgs expected actual =
    TypeError $ "Invalid number of arguments.\n\tExpected " ++
        show expected ++ " arguments, got " ++ show actual

errEmptyList :: HaspError
errEmptyList = Error "Empty list"

errWrongType :: HaspError
errWrongType = Error "Wrong type"
