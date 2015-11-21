module Data.Morgue.Outline
    ( runOutline,
      Options(..),
      defaultOptions
    )
where

import Text.Pandoc

import Data.Morgue.Util
import Data.Morgue.OutlineGenerator
import Data.Morgue.Format

-- even less options than in Main.hs ;)
data Options = Options { optOutput :: String -> IO ()
                       , optFormat :: !OutputFormat
                       }

-- default: output on stdout, ANSI coloring
defaultOptions :: Options
defaultOptions = Options { optOutput = putStrLn
                         , optFormat = ANSI
                         }

runOutline :: Options -> String -> IO ()
runOutline os input = do
    let Options { optOutput = output
                , optFormat = format
                } = os
        readerOpts = def { readerParseRaw = False }
        pandoc = readMarkdown readerOpts input
    output $ writeOutline pandoc format
