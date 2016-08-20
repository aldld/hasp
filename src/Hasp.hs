module Hasp
( repl
, initRepl
, haspVersion
, runProcessInputFiles
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
import System.Exit
import Control.Monad
import Control.Monad.Trans
import Control.Exception

haspVersion :: String
haspVersion = "0.1.0.0"

printErrorAndFail :: Env -> HaspError -> InputT IO Env
printErrorAndFail env err = do
    outputStrLn $ show err
    liftIO exitFailure

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
            when (result /= HNothing) $ outputStrLn $ show result
            printExprResults newEnv exprs

handleError :: (ThrowsError a) -> (HaspError -> InputT IO b) ->
    (a -> InputT IO b) -> InputT IO b
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

-- |Prints a welcome message and initializes the read-eval-print loop with the
-- given default environment.
initRepl :: Env -> IO ()
initRepl env = do
    putStrLn $ "Welome to hasp! Version " ++ haspVersion
    runInputT defaultSettings $ repl env

-- |Processes given input files from left to right, outputting the final
-- resulting environment.
processInputFiles :: Env -> [FilePath] -> InputT IO Env
processInputFiles env [] = return env
processInputFiles env (fname:fnames) = do
    sourceOrExc <- liftIO $ try $ readFile fname
    case (sourceOrExc :: Either IOError String) of
        Left except -> do
            outputStrLn $ show except
            liftIO exitFailure
        Right source -> do
            -- TODO: Refactor this, extract functionality from repl.
            exprsOrErr <- return $ do
                tokens <- tokenize source
                parseExprs tokens
            handleError exprsOrErr (printErrorAndFail env) $ \exprs -> do
                newEnv <- printExprResults env exprs
                processInputFiles newEnv fnames

-- |Processes given input files from left to right, outputting the final
-- resulting environment. This is a wrapper function for processInputFiles,
-- meant to be called from main.
runProcessInputFiles :: Env -> [FilePath] -> IO Env
runProcessInputFiles env fnames =
    runInputT defaultSettings $ processInputFiles env fnames
