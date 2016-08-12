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

import System.IO
import System.Console.Haskeline
import Control.Monad
import qualified Data.Map as Map

globalEnv :: Env
globalEnv = Env (Map.fromList [
    -- TODO: This is ugly. Make a nicer way of generating hasp functions.
    ("+", HFunc (Env Map.empty) 2 $ \args -> case args of
                       ((HInt x):(HInt y):[]) -> Right (HInt (x + y))
                       lst -> undefined)
    ])

printResult :: Either Error HData -> InputT IO ()
printResult (Left err) = outputStrLn $ show err
printResult (Right result) = outputStrLn $ show result

printExprResults :: Env -> [Expr] -> InputT IO ()
printExprResults env exprs = mapM_ printResult $ map (evalExpr env) exprs
-- TODO: Update environment after each expression is evaluated.


-- |The main, interactive read-eval-print loop for hasp.
repl :: Env -> InputT IO ()
repl env = do
    maybeLine <- getInputLine "λ> "
    case maybeLine of
        Nothing   -> return () -- EOF / ctrl+d
        Just line -> do
            case tokenize line of
                Left err     -> outputStrLn $ show err
                Right tokens ->
                    case parseExprs tokens of
                        Left err    -> outputStrLn $ show err
                        Right exprs -> printExprResults env exprs
            repl env -- TODO: Pass updated environment instead of original.

-- |Prints a welcome message and initializes the read-eval-print loop.
initRepl :: IO ()
initRepl = do
    putStrLn "Welome to hasp!"
    runInputT defaultSettings $ repl globalEnv

