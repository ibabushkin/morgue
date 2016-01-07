-- | Interface to agenda generation, encapsulating options etc.
module Data.Morgue.Agenda
    ( defaultOptions
    , runAgenda
    , getAgenda
    )
where

import Data.Time

import Text.Pandoc

import Data.Morgue.AgendaGenerator
import Data.Morgue.Format
import Data.Morgue.Options
import Data.Morgue.Util

-- | default options: 1 week agenda, output on stdout, ANSI coloring
defaultOptions :: Options
defaultOptions = AgendaOptions
    { optMode    = Both
    , optDoubleSpaces = False
    , optTags = Nothing
    , optSkipTags = Nothing
    , optNumDays = 6
    , optOutput  = putStrLn
    , optFormat  = ANSI
    }

-- | perform computations based on options given
getAgenda :: Options -> TimeZone -> UTCTime -> String -> String
getAgenda opts tz time input =
    let AgendaOptions
         { optMode = m
         , optDoubleSpaces = ds
         , optTags = tags
         , optSkipTags = skipTags
         , optNumDays = n
         , optFormat = format
         } = opts
        currentDay = utcToLocalTime tz time
        days = getFollowingDays currentDay n
        readerOpts = def { readerParseRaw = False }
        pandoc = readMarkdown readerOpts $ doubleSpaces ds input
     in writeAgenda m pandoc days format (tagFilter tags skipTags)

-- | output an agenda based on options
runAgenda :: Options -> String -> IO ()
runAgenda opts input = (getAgenda opts <$> getCurrentTimeZone <*>
    getCurrentTime <*> pure input) >>= optOutput opts
