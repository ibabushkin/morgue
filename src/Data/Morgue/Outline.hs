-- | interface to outline generation, encapsulating options etc.
module Data.Morgue.Outline
    ( runOutline,
      getOutline,
      Options(..),
      defaultOptions
    )
where

import Text.Pandoc

import Data.Morgue.Util
import Data.Morgue.OutlineGenerator
import Data.Morgue.Format

-- | options supported
data Options = Options { optOutput :: String -> IO ()
                       , optFormat :: !OutputFormat
                       }

-- | default options: output on stdout, ANSI coloring
defaultOptions :: Options
defaultOptions = Options { optOutput = putStrLn
                         , optFormat = ANSI
                         }

-- | perform computations to generate an outline
getOutline :: Options -> String -> IO String
getOutline opts input = do
    let Options { optOutput = output
                , optFormat = format
                } = opts
        readerOpts = def { readerParseRaw = False }
        pandoc = readMarkdown readerOpts input
    return $ writeOutline pandoc format

-- | output an outline based on options
runOutline :: Options -> String -> IO ()
runOutline opts input = getOutline opts input >>= optOutput opts
