module Main where

import System.Environment

import Hasp

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> initRepl  -- No arguments: initialize REPL.
        x:xs -> return ()  -- TODO: Read file input.
