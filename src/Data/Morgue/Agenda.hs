module Data.Morgue.Agenda
    ( Options
    , runAgenda
    , main
    )
where

import System.IO
import System.Exit
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import Data.List.Split (splitOn)
import Data.Time
import Control.Exception (evaluate)

import Text.Pandoc

import Data.Morgue.Util
import Data.Morgue.AgendaGenerator
import Data.Morgue.Format

-- not many right now
data Options = Options { optMode         :: !AgendaMode
                       , optDoubleSpaces :: Bool
                       , optTags         :: Maybe [Tag]
                       , optSkipTags     :: Maybe [Tag]
                       , optNumDays      :: !Integer
                       , optOutput       :: String -> IO ()
                       , optFormat       :: !OutputFormat
                       }

-- default: 1 week agenda, output on stdout, ANSI coloring
defaultOptions :: Options
defaultOptions = Options { optMode    = Both
                         , optDoubleSpaces = False
                         , optTags = Nothing
                         , optSkipTags = Nothing
                         , optNumDays = 6
                         , optOutput  = putStrLn
                         , optFormat  = ANSI
                         }

-- options registered with GetOpt
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

-- perform computations
runAgenda :: Options -> String -> IO ()
runAgenda opts input = do
    let Options { optMode = m
                , optDoubleSpaces = ds
                , optTags = tags
                , optSkipTags = skipTags
                , optNumDays = n
                , optOutput = output
                , optFormat = format
                } = opts

    -- timezone + time hackery
    currentTimeZone <- getCurrentTimeZone
    currentUtcTime <- getCurrentTime
    let currentDay = utcToLocalTime currentTimeZone currentUtcTime

    -- process 
    let days = getFollowingDays currentDay n
        readerOpts = def { readerParseRaw = False }
        pandoc = readMarkdown readerOpts $ doubleSpaces ds input
    output $ writeAgenda m pandoc days format (tagFilter tags skipTags)

-- do stuff.
main :: IO ()
main = do
    -- handle args
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    -- look through the file(s) we are interested in
    concat <$> mapM readFile files >>= runAgenda opts
