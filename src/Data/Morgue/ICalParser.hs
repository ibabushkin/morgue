{-# LANGUAGE FlexibleContexts #-}
module ICalParser where

import Data.List (groupBy, intercalate, sortOn)
import Data.List.Split (splitOn)
import Text.ParserCombinators.Parsec hiding (newline)

-- an event as saved in the iCal file
data Event = Event { summary :: String
                   , dtStart :: String
                   , location :: String
                   , categories :: String
                   } deriving (Show, Eq, Ord)
                   
-- show an event as a markdown bullet-point
eventToMd :: Event -> String
eventToMd (Event s t l c) = "* " ++ t ++ " "++ s ++ cat ++ ", " ++ l ++ "\n"
    where cat = case words c of
                  (r:_) -> r
                  [] -> ""

-- convert a iCal file to markdown
convertFile :: FilePath -> IO String
convertFile p = do result <- parse parseDocument p <$> readFile p
                   case result of
                     (Right es) -> return $ concatMap eventToMd es
                     (Left _) -> return "An error occured!"
    where gr e1 e2 = categories e1 == categories e2

-- parse an event
parseEvent :: Parser Event
parseEvent = begin "VEVENT" *> uselessLine *>
    (Event <$> (string "SUMMARY:" *> rest) <*> time <*>
        (uselessLine *> uselessLine *> uselessLine *> uselessLine *>
         string "LOCATION:" *> rest) <*>
         (string "CATEGORIES:" *> rest)) <*
         uselessLine <* uselessLine <* end "VEVENT"
    where uselessLine = many (noneOf "\r\n") <* newline
          rest = many (noneOf "\r\n") <* newline
          time = many (noneOf ":") *> char ':' *> timestamp <* newline
          timestamp = do year <- count 4 digit
                         month <- count 2 digit
                         day <- count 2 digit
                         char 'T'
                         hour <- count 2 digit
                         minute <- count 2 digit
                         count 2 digit
                         return $ "T[" ++ day ++ "." ++ month ++ "." ++ year ++
                             ":" ++ hour ++ ":" ++ minute ++ "]"

-- parse the whole document
parseDocument :: Parser [Event]
parseDocument =
    (begin "VCALENDAR" <* skipUntilTimezone) *>
        many (parseEvent <* newline)
        <* end "VCALENDAR"

-- skip timezone info
skipUntilTimezone :: Parser ()
skipUntilTimezone = section *> section *> pure ()
    where uselessLine = many (noneOf "\r\n") <* newline
          section = uselessLine `manyTill` newline

-- beginning of a section
begin :: String -> Parser String
begin s = string ("BEGIN:" ++ s) <* newline

-- end of a section
end :: String -> Parser String
end s = string ("END:" ++ s) <* newline

newline :: Parser String
newline = string "\n" <|> string "\r\n" <|> string "\r"

main :: IO ()
main = convertFile "/home/thewormkill/downloads/notes.ics" >>= putStrLn
