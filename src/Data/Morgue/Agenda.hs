-- | Interface to agenda generation, encapsulating options etc.
module Data.Morgue.Agenda
    ( Options(..)
    , defaultOptions
    , runAgenda
    , getAgenda
    )
where

import Data.Time

import Text.Pandoc

import Data.Morgue.AgendaGenerator
import Data.Morgue.Format
import Data.Morgue.Util

-- | options supported for agenda generation
data Options = Options { optMode         :: !AgendaMode
                       , optDoubleSpaces :: Bool
                       , optTags         :: Maybe [Tag]
                       , optSkipTags     :: Maybe [Tag]
                       , optNumDays      :: !Integer
                       , optOutput       :: String -> IO ()
                       , optFormat       :: !OutputFormat
                       }

-- | default options: 1 week agenda, output on stdout, ANSI coloring
defaultOptions :: Options
defaultOptions = Options { optMode    = Both
                         , optDoubleSpaces = False
                         , optTags = Nothing
                         , optSkipTags = Nothing
                         , optNumDays = 6
                         , optOutput  = putStrLn
                         , optFormat  = ANSI
                         }

-- | perform computations based on options given
getAgenda :: Options -> String -> IO String
getAgenda opts input = do
    let Options { optMode = m
                , optDoubleSpaces = ds
                , optTags = tags
                , optSkipTags = skipTags
                , optNumDays = n
                --, optOutput = output
                , optFormat = format
                } = opts
    currentTimeZone <- getCurrentTimeZone
    currentUtcTime <- getCurrentTime
    let currentDay = utcToLocalTime currentTimeZone currentUtcTime
        days = getFollowingDays currentDay n
        readerOpts = def { readerParseRaw = False }
        pandoc = readMarkdown readerOpts $ doubleSpaces ds input
    return $ writeAgenda m pandoc days format (tagFilter tags skipTags)

-- | output an agenda based on options
runAgenda :: Options -> String -> IO ()
runAgenda opts input = getAgenda opts input >>= optOutput opts
