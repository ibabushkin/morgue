module Format where

import System.Console.ANSI

import Data.List (isPrefixOf)

import Text.Pandoc

-- how do we want to display something?
data OutputFormat = Plaintext | Pango | ANSI deriving (Show, Read, Eq)

-- format a string according to args:
format :: OutputFormat -> String -> String
format Plaintext s = s
format Pango s
    | "Week agenda" `isPrefixOf` s = lightBlue
    | s == "OVERDUE" = red
    | s == "TODO" = red
    | s == "Time" = red
    | s == "Deadline" = red
    | s == "Scheduled" = yellow
    | otherwise = lightBlue
    where cS = "</span></b>"
          lightBlue = "<span color=\"lightblue\">" ++ s ++ "</span>"
          red = "<b><span color=\"red\">" ++ s ++ cS
          yellow = "<b><span color=\"yellow\">" ++ s ++ cS
format ANSI s
    | "Week agenda" `isPrefixOf` s = lightBlue
    | s == "OVERDUE" = red
    | s == "TODO" = red
    | s == "Time" = red
    | s == "Deadline" = red
    | s == "Scheduled" = yellow
    | otherwise = lightBlue
    where cS = setSGRCode []
          lightBlue = setSGRCode
              [SetColor Foreground Vivid Cyan] ++ s ++ cS
          red = setSGRCode
              [SetColor Foreground Vivid Red] ++ s ++ cS
          yellow = setSGRCode
              [SetColor Foreground Vivid Yellow] ++ s ++ cS

formatH :: OutputFormat -> Int -> String -> String
formatH f n s = (replicate ((n-1)*2) ' ') ++ formatted f
    where s' = "# " ++ s
          formatted Plaintext = s'
          formatted ANSI =
              setSGRCode [SetColor Foreground Vivid colorA] ++ s'
          formatted Pango = 
              "<span color=\"" ++ colorP ++ "\">" ++ s' ++ "</span>"
          colorA = enumFromTo Red White !! n'
          colorP = ["red", "green", "yellow", "blue", "magenta", "cyan", "white"] !! n'
          n' = n `mod` 7

-- format inlines
formatInlines :: [Inline] -> String
formatInlines = concatMap formatInline

-- format inline to a simplified markdown
formatInline :: Inline -> String
formatInline (Str s) = s
formatInline (Emph is) = wrapIn "*" $ formatInlines is
formatInline (Strong is) = wrapIn "**" $ formatInlines is
formatInline (Strikeout is) = wrapIn "~~" $ formatInlines is
formatInline (Quoted q is) = let q' = if q == DoubleQuote
                                      then "\""
                                      else "'"
                              in wrapIn q' $ formatInlines is 
formatInline (Code _ s) = wrapIn "`" s
formatInline Space = " "
formatInline (Link is t) = fst t -- links become urls.
formatInline (Image is t) = fst t -- same for images
formatInline _ = "" -- ignore the rest

-- wrap text in a tag-like structure
wrapIn :: String -> String -> String
wrapIn w c = w ++ c ++ w
