module Hasp
( repl
, initRepl
) where

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
    putStrLn $ "You typed " ++ line

-- |Prints a welcome message and initializes the read-eval-print loop.
initRepl :: IO ()
initRepl = do
    putStrLn "Welome to hasp!" -- Print welcome message
    repl

