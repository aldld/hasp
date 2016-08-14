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

printExprResults :: Env -> [Expr] -> InputT IO Env
printExprResults env [] = return env
printExprResults env (expr:exprs) = do
    case evalExpr env expr of
        Left err            -> do
            outputStrLn $ show err
            return env
        Right (result, newEnv) -> do
            outputStrLn $ show result
            printExprResults newEnv exprs


-- |The main, interactive read-eval-print loop for hasp.
repl :: Env -> InputT IO ()
repl env = do
    maybeLine <- getInputLine "λ> "
    case maybeLine of
        Nothing   -> return () -- EOF / ctrl+d
        Just line ->
            case tokenize line of
                Left err     -> do
                    outputStrLn $ show err
                    repl env
                Right tokens ->
                    case parseExprs tokens of
                        Left err    -> do
                            outputStrLn $ show err
                            repl env
                        Right exprs -> do
                            newEnv <- printExprResults env exprs
                            repl newEnv

-- |Prints a welcome message and initializes the read-eval-print loop.
initRepl :: IO ()
initRepl = do
    putStrLn "Welome to hasp!"
    runInputT defaultSettings $ repl globalEnv

