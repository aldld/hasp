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
, errFormSyntax
) where

data HaspError = Error String
               | SyntaxError String
               | NameError String
               | TypeError String
               | BadFormError String

instance Show HaspError where
    show (Error msg) = "Error: " ++ msg
    show (SyntaxError msg) = "SyntaxError: " ++ msg
    show (NameError msg) = "NameError: " ++ msg
    show (TypeError msg) = "TypeError: " ++ msg
    show (BadFormError msg) = "BadFormError: " ++ msg

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

errWrongType :: String -> String -> HaspError
errWrongType fname expected = TypeError $ "Wrong type in call to `" ++ fname ++
    "`.\n\tExpected argument of type " ++ expected

errTooFewArgs :: String -> HaspError
errTooFewArgs fname = TypeError $ "Too few arguments to `" ++ fname ++ "`"

errNumArgs :: String -> Int -> Int -> HaspError
errNumArgs fname expected actual =
    TypeError $ "Invalid number of arguments to `" ++ fname ++
        "`.\n\tExpected " ++ show expected ++ " arguments, got " ++ show actual

errEmptyList :: String -> HaspError
errEmptyList fname = Error $ "Attempted to call `" ++ fname ++ "` on Empty list"

errFormSyntax :: String -> String -> HaspError
errFormSyntax fname syntax =
    BadFormError $ "Syntax of `" ++ fname ++ "` expression must be of form " ++
        syntax
