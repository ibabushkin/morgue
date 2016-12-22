{-# LANGUAGE OverloadedStrings #-}
module Data.Morgue.Options where

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
data Options = Options
    { optOutput :: Text -> IO () -- ^ how to output the results
    , optFormat :: OutputFormat -- ^ the output format to use
    , optMode :: AgendaMode -- ^ the agenda mode to use
    , optTags :: Maybe ([Tag], Bool) -- ^ tags passed to filter the agenda tree
    }

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
    where constructOptions day = Options TIO.putStr ANSI (Timed day 6 True) Nothing

-- | build a help header
helpHeader :: IO String
helpHeader = formatHeader <$> getProgName
    where formatHeader prg = prg <> " version 1.0\nUSAGE: " <> prg
            <> " [OPTION..] file(s)\nOPTIONS:"

-- | options to be registered with GetOpt
options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "h" ["help"]
        (NoArg (\_ -> do
            helpHeader >>= TIO.hPutStrLn stderr . pack . flip usageInfo options
            exitSuccess
        ))
        "Show this help."
    ]

runWithOptions :: IO ()
runWithOptions = do
    -- handle args
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    opts <- foldl (>>=) defaultOptions actions
    -- TODO: concat <$> mapM TIO.readFile files >>= runAgenda opts
    return ()
