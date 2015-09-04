import System.IO
import System.Exit
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import Text.Pandoc

import Util
import OutlineGenerator
import Format

-- even less options than in Main.hs ;)
data Options = Options { optOutput :: String -> IO ()
                       , optFormat :: !OutputFormat
                       }

-- default: output on stdout, ANSI coloring
defaultOptions :: Options
defaultOptions = Options { optOutput = putStrLn
                         , optFormat = ANSI
                         }

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
    let Options { optOutput = output
                , optFormat = format
                } = opts

    -- look through the file(s) we are interested in
    inputs <- mapM readFile files

    -- process
    let readerOpts = def { readerParseRaw = False }
        pandoc = readMarkdown readerOpts $ concat inputs
        results = writeOutline pandoc format

    -- output
    output results
