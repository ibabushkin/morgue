module Data.Morgue.Options where

import Data.Morgue.AgendaGenerator
import Data.Morgue.Format

data Options = AgendaOptions { optMode         :: !AgendaMode
                             , optDoubleSpaces :: Bool
                             , optTags         :: Maybe [Tag]
                             , optSkipTags     :: Maybe [Tag]
                             , optNumDays      :: !Integer
                             , optOutput       :: String -> IO ()
                             , optFormat       :: !OutputFormat
                             }
             | OutlineOptions { optOutput :: String -> IO ()
                              , optFormat :: !OutputFormat
                              }
