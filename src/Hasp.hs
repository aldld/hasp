module Hasp
( repl
, initRepl
) where

import Error
import Tokenizer
import Parser
import Expressions
import Semantics
import DataTypes
import Builtins

import System.IO
import System.Console.Haskeline
import Control.Monad

printErrorAndContinue :: Env -> HaspError -> InputT IO ()
printErrorAndContinue env err = do
    outputStrLn $ show err
    repl env

printExprResults :: Env -> [Expr] -> InputT IO Env
printExprResults env [] = return env
printExprResults env (expr:exprs) = do
    let resultOrErr = unpackVal $ evalExpr env expr
    case resultOrErr of
        Left err -> do
            outputStrLn $ show err
            return env
        Right (result, newEnv) -> do
            outputStrLn $ show result
            printExprResults newEnv exprs

handleError :: (ThrowsError a) -> (HaspError -> InputT IO ()) -> (a -> InputT IO ()) -> InputT IO ()
handleError (TE (Left err)) alternative _ = alternative err
handleError (TE (Right val)) _ next = next val

-- |The main, interactive read-eval-print loop for hasp.
repl :: Env -> InputT IO ()
repl env = do
    maybeLine <- getInputLine "|λ〉"
    case maybeLine of
        Nothing   -> return () -- EOF / ctrl+d
        Just line -> do
            exprsOrErr <- return $ do
                tokens <- tokenize line
                parseExprs tokens
            handleError exprsOrErr (printErrorAndContinue env) $ \exprs -> do
                newEnv <- printExprResults env exprs
                repl newEnv

-- |Prints a welcome message and initializes the read-eval-print loop.
initRepl :: IO ()
initRepl = do
    putStrLn "Welome to hasp!"
    runInputT defaultSettings $ repl globalEnv

