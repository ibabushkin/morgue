{- | The main module of the morgue-executable, allowing
to generate agendas from specially formatted markdown files.
-}
module Main where

import Data.List.Split (splitOn)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn, stderr)

import Data.Morgue.Agenda
import Data.Morgue.Options
import Data.Morgue.Util

-- | options to be registered with GetOpt
options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "m" ["mode"]
        (ReqArg
            (\arg opt -> return opt { optMode = readWrapper arg })
            "MODE")
        "Desired agenda mode. Possible values are\n\
        \'Timed', 'Todo' and 'Both'. Default: Both" 

    , Option "d" ["double-spaces"]
        (NoArg (\opt -> return opt { optDoubleSpaces = True }))
        "Double amount of leading spaces (in case you use 2)"

    , Option "t" ["tags"]
        (ReqArg
            (\arg opt -> return opt { optTags = Just $ splitOn ":" arg
                                    , optSkipTags = Nothing })
            "TAGS")
        "A colon-separated list of tags to include.\n\
        \If present, ignores all other tags."

    , Option "s" ["skip-tags"]
        (ReqArg
            (\arg opt -> return opt { optSkipTags = Just $ splitOn ":" arg
                                    , optTags = Nothing })
            "TAGS")
        "A colon-separated list of tags to ignore.\n\
        \If present, includes all other tags."

    , Option "n" ["days"]
        (ReqArg
            (\arg opt -> return opt { optNumDays = readWrapper arg })
            "NUM")
        "Number of days to include, not counting today.\nDefault: 6"
    
    , Option "o" ["output"]
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

-- | do stuff we need to do
main :: IO ()
main = do
    -- handle args
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    -- look through the file(s) we are interested in
    concat <$> mapM readFile files >>= runAgenda opts
