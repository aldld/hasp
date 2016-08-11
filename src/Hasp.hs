module Hasp
( repl
, initRepl
) where

import Tokenizer
import Parser

import System.IO
import Control.Monad

-- |Reads a single line of input from the command line.
-- TODO: Support multiline input.
promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    hFlush stdout
    getLine

-- |The main, interactive read-eval-print loop for hasp.
repl :: IO ()
repl = forever $ do
    line <- promptLine "λ> "
    let tokensOrErr = tokenize line
    putStrLn $ "You typed " ++ line
    case tokensOrErr of
        Left err     -> print err
        Right tokens -> mapM_ putStrLn tokens

-- |Prints a welcome message and initializes the read-eval-print loop.
initRepl :: IO ()
initRepl = do
    putStrLn "Welome to hasp!" -- Print welcome message
    repl

