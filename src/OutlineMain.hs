{- | The main module of the executable morgue-outline,
which is a simple outliner for markdown files.
-}
module Main where

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO (hPutStrLn, stderr)


import Data.Morgue.Outline
import Data.Morgue.Options
import Data.Morgue.Util

-- options registered with GetOpt
options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = writeFile arg })
            "FILE")
        "Output destination.\nDefault: stdout"

    , Option "f" ["format"]
        (ReqArg
            (\arg opt -> return opt { optFormat = readWrapper arg })
            "FORMAT")
        "Output format. Possible values are\n\
        \'Plaintext', 'ANSI' and 'Pango'. Default: ANSI"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                let header = prg ++ " version 0.4\nUSAGE: " ++
                        prg ++ " [OPTION..] file(s)\nOPTIONS:" 
                hPutStrLn stderr (usageInfo header options)
                exitSuccess))
        "Show this help."
    ]

-- do stuff
main :: IO ()
main = do
    -- handle args
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions

    -- look through the file(s) we are interested in
    concat <$> mapM readFile files >>= runOutline opts
