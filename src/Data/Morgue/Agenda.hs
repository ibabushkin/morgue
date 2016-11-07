{-# LANGUAGE RecordWildCards #-}
module Data.Morgue.Agenda
    ( defaultOptions
    , runAgenda
    , getAgenda
    ) where
-- Interface to agenda generation, encapsulating options etc.

import Data.Time

import Text.Pandoc

import Data.Morgue.AgendaGenerator
import Data.Morgue.Format
import Data.Morgue.Options
import Data.Morgue.Util

-- | default options: 1 week agenda, output on stdout, ANSI coloring
defaultOptions :: Options
defaultOptions = AgendaOptions
    { optMode = Both
    , optDoubleSpaces = False
    , optTags = Nothing
    , optSkipTags = Nothing
    , optNumDays = 6
    , optOutput = putStrLn
    , optFormat = ANSI
    }

-- | perform computations based on options given
getAgenda :: Options -> TimeZone -> UTCTime -> String -> String
getAgenda opts tz time input =
    writeAgenda optMode pandoc days optFormat (tagFilter optTags optSkipTags)
    where AgendaOptions{..} = opts
          currentDay = utcToLocalTime tz time
          days = getFollowingDays currentDay optNumDays
          readerOpts = def { readerParseRaw = False }
          pandoc = readMarkdown readerOpts $ doubleSpaces optDoubleSpaces input


-- | output an agenda based on options
runAgenda :: Options -> String -> IO ()
runAgenda opts input = (getAgenda opts <$> getCurrentTimeZone <*>
    getCurrentTime <*> pure input) >>= optOutput opts
