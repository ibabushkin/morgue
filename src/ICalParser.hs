{-# LANGUAGE FlexibleContexts #-}

import Data.List.Split (splitOn)
import Text.ParserCombinators.Parsec hiding (newline)

-- parsing helper
parse' rule text = parse rule "bla" text

-- the ADT type we map our parsed iCal entries to
data VEvent = VEvent [EventProperty]
    deriving (Show, Eq)

data EventProperty = UID String
                   | Summary String
                   | DtStart String String
                   | DtEnd String String
                   | Sequence Int
                   | XMozGeneration Int
                   | DtStamp String
                   | Location String
                   | Categories [String]
                   | Description String
                   | Class String
    deriving (Show, Eq)

-- parse the whole document
parseDocument :: Parser [VEvent]
parseDocument =
    (begin "VCALENDAR" <* skipUntilTimezone) *>
        many (parseVevent <* newline)
        <* end "VCALENDAR"

-- skip timezone info
skipUntilTimezone :: Parser ()
skipUntilTimezone = section *> section *> pure ()
    where uselessLine = many (noneOf "\r\n") <* newline
          section = (uselessLine `manyTill` newline)

-- a complete section describing an event
parseVevent :: Parser VEvent
parseVevent = VEvent <$> (begin "VEVENT" *> many line <* end "VEVENT")

-- beginning of a section
begin :: String -> Parser String
begin s = string ("BEGIN:" ++ s) <* newline

-- end of a section
end :: String -> Parser String
end s = string ("END:" ++ s) <* newline

-- parse a line in a VEVENT section
line :: Parser EventProperty
line = keyword <*> (char ':' *> many (noneOf "\r\n")) <* newline

newline :: Parser String
newline = string "\n" <|> string "\r\n" <|> string "\r"

-- parse a keyword and return a (partially applied) constructor
keyword :: Parser (String -> EventProperty)
keyword = choice [ string "UID" *> pure UID
                 , try (string "SUMMARY") *> pure Summary
                 , try (string "DTSTART;") *> (DtStart <$> (many $ noneOf ":")) 
                 , try (string "DTEND;") *> (DtEnd <$> (many $ noneOf ":"))
                 , try (string "SEQUENCE") *> pure (Sequence . read)
                 , try (string "X-MOZ-GENERATION") *> pure
                       (XMozGeneration . read)
                 , try (string "DTSTAMP") *> pure DtStamp
                 , try (string "LOCATION") *> pure Location
                 , try (string "CATEGORIES") *> pure (Categories . splitOn ",")
                 , try (string "DESCRIPTION") *> pure Description
                 , try (string "CLASS") *> pure Class
                 ]
