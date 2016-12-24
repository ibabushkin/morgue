{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Morgue.Options ( run ) where

import Data.Foldable (foldl')
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Data.Monoid ((<>))
import Data.Morgue.Agenda.Time (getCurrentDay)
import Data.Morgue.Agenda.Types

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO (stderr)

-- | the format to be used when outputting a filtered tree
data OutputFormat
    = Plain -- ^ plain text. boring, but reliable (and machine-readable)
    | Pango -- ^ pango markup. useful for awesomewm or dunst notifications
    | ANSI  -- ^ colored plain text. not as boring
    deriving (Show, Eq)

-- | the options we parametrize our behaviour over
data Options
    = RunWith
        { optOutput :: Maybe FilePath -- ^ how to output the results
        , optFormat :: OutputFormat -- ^ the output format to use
        , optMode :: AgendaMode -- ^ the agenda mode to use
        , optTags :: Maybe ([Tag], Bool) -- ^ tags passed to filter the agenda tree
        , optFiles :: [FilePath] -- ^ the files to use as input
        }
    | Help
    | Version
    deriving (Show, Eq)

-- | get the default options (requires current day)
--
-- the defaults used are:
-- * output to stdout
-- * colored plaintext format
-- * a timed agenda for the week
-- * a todo tree
-- * no tag filter
defaultOptions :: IO Options
defaultOptions = constructOptions <$> getCurrentDay
    where constructOptions day = RunWith Nothing ANSI (Timed day 6 True) Nothing []

-- | get a string representation of the currently running version
-- TODO: use a quasi-quoter to read our cabal file at compile time.
version :: String
version = "1.0"

-- | build a help message
helpMessage :: IO Text
helpMessage = pack . flip usageInfo options . formatHeader <$> getProgName
    where formatHeader prg = prg <> " version " <> version <> "\nUSAGE: " <> prg
            <> " [OPTION..] file(s)\nOPTIONS:"

-- | build a version message
versionMessage :: IO Text
versionMessage = formatHeader <$> getProgName
    where formatHeader prg = pack prg <> " version " <> pack version

-- | options to be registered with GetOpt
options :: [OptDescr (Options -> Options)]
options =
    [ Option "h" ["help"] (NoArg (const Help))
        "Show this help."
    , Option "v" ["version"] (NoArg (const Version))
        "Show the morgue version you're using."
    ]

-- | run, parsing options
run :: IO ()
run = do
    -- handle args
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    opts <- foldl' (flip ($)) <$> defaultOptions <*> pure actions
    runOpts opts { optFiles = files }
    -- concat <$> mapM TIO.readFile files >>= runAgenda opts

-- | given some options, take the appropriate action
runOpts :: Options -> IO ()
runOpts Help = helpMessage >>= TIO.hPutStrLn stderr >> exitSuccess
runOpts Version = versionMessage >>= TIO.hPutStrLn stderr
runOpts opts@RunWith{..} = print opts
