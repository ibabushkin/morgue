import System.IO
import System.Exit
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import Data.Time
import Control.Exception (evaluate)

import Text.Pandoc

import Util
import AgendaGenerator
import Format

-- not many right now
data Options = Options { optMode       :: !AgendaMode
                       -- strict args (those that can fail)
                       , optNumDays    :: !Integer
                       , optOutput     :: String -> IO ()
                       , optFormat     :: !OutputFormat
                       }

-- default: 1 week agenda, output on stdout, ANSI coloring
defaultOptions :: Options
defaultOptions = Options { optMode    = Both
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

-- do stuff.
main :: IO ()
main = do
    -- handle args
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options { optMode = m
                , optNumDays = n
                , optOutput = output
                , optFormat = format
                } = opts

    -- look through the file(s) we are interested in
    inputs <- mapM readFile files

    -- timezone + time hackery
    currentTimeZone <- getCurrentTimeZone
    currentUtcTime <- getCurrentTime
    let currentDay = utcToLocalTime currentTimeZone currentUtcTime

    -- process 
    let days = getFollowingDays currentDay n
        readerOpts = def { readerParseRaw = False }
        pandoc = readMarkdown readerOpts $ concat inputs
        results = writeAgenda m pandoc days format
    
    -- output
    output results
