-- | interface to outline generation, encapsulating options etc.
module Data.Morgue.Outline
    ( runOutline,
      getOutline,
      defaultOptions
    )
where

import Text.Pandoc

import Data.Morgue.Util
import Data.Morgue.Options
import Data.Morgue.OutlineGenerator
import Data.Morgue.Format

-- | default options: output on stdout, ANSI coloring
defaultOptions :: Options
defaultOptions = OutlineOptions
    { optOutput = putStrLn
    , optFormat = ANSI
    }

-- | perform computations to generate an outline
getOutline :: Options -> String -> String
getOutline OutlineOptions{optOutput = output , optFormat = format} input = 
    writeOutline pandoc format
    where readerOpts = def { readerParseRaw = False }
          pandoc = readMarkdown readerOpts input

-- | output an outline based on options
runOutline :: Options -> String -> IO ()
runOutline opts input = optOutput opts $ getOutline opts input
