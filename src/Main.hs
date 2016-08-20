module Main (main) where

import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO

import Hasp
import Builtins

data Flag = Interactive -- -i
          | Version     -- -v
          | Help        -- -h
          deriving (Eq, Ord, Enum, Show, Bounded)

-- |Option descriptions for command line flags.
flags :: [OptDescr (Options -> IO Options)]
flags =
    [ Option ['i'] ["interactive"]
        (NoArg $ \opt -> return opt { optInteractive = True }) $
        "Evaluate contents of supplied files, and initialize interactive "
            ++ "read-eval-print-loop"
    , Option ['v'] ["version"]
        (NoArg $ \_ -> do
            hPutStrLn stderr $ "Version " ++ haspVersion
            exitWith ExitSuccess)
        "Display hasp version info"
    , Option ['h'] ["help"]
        (NoArg $ \_ -> do
            prg <- getProgName
            hPutStrLn stderr $ usageInfo prg flags
            exitWith ExitSuccess)
        "Show this help text" ]

-- |Option values from command line.
data Options = Options
    { optInteractive :: Bool }

-- |Default command line options.
defaultOptions :: Options
defaultOptions = Options
    { optInteractive = False }

main :: IO ()
main = do
    args <- getArgs

    if args == []
        then initRepl globalEnv
        else do
            let (actions, fileNames, errors) =
                    getOpt RequireOrder flags args
            opts <- foldl (>>=) (return defaultOptions) actions
            let Options { optInteractive = interactive } = opts

            env <- runProcessInputFiles globalEnv fileNames

            if optInteractive opts
                then initRepl env
                else return ()
