-- | Outline generation module
module Data.Morgue.OutlineGenerator where

import Data.List (intercalate)

import Text.Pandoc
import Text.Pandoc.Error

import Data.Morgue.Format

-- | return a string representing an outline
writeOutline :: Either PandocError Pandoc
             -> OutputFormat
             -> String
writeOutline (Right (Pandoc _ blocks)) outFormat =
    header ++ formatHeaders outFormat blocks 
    where header = format outFormat "Outline:\n"
writeOutline _ _ = error "Pandoc error occured!"

-- | format the headers for a given set of blocks
formatHeaders :: OutputFormat -> [Block] -> String
formatHeaders outFormat =
    intercalate "\n" . filter (/="") . map (formatHeader outFormat)

-- | format the header of a block, if present
formatHeader :: OutputFormat -> Block -> String
formatHeader outFormat (Header n _ is) =
    formatH outFormat n $ formatInlines is
formatHeader _ _ = ""
