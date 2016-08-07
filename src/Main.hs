module Main where

import System.Environment

import Hasp

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
        then initRepl  -- No arguments: initialize REPL.
        else return () -- TODO: Read source from file.
