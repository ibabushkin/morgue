{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec
import Text.Megaparsec.Text

import System.Environment (getArgs)

iCalP :: Parser [Text]
iCalP = section "VCALENDAR" $ do
    string "VERSION:" >> manyTill anyChar crlf
    string "METHOD:" >> manyTill anyChar crlf
    string "PRODID:" >> manyTill anyChar crlf
    crlf
    -- skip the timezone info
    section "VTIMEZONE" (manyTill anyChar $ lookAhead (string "END:VTIMEZONE"))
    crlf
    (map renderEvent) <$> sepEndBy (section "VEVENT" eventP) crlf

data Event = Event
    { eventSummary :: Text
    , eventStart :: Text
    , eventLocation :: Text
    }

renderEvent :: Event -> Text
renderEvent Event{..} = "* " <> eventStart <> " " <> eventSummary <> ", " <> eventLocation

eventP :: Parser Event
eventP = do
    ignoreLine -- UID
    summary <- keyValueP "SUMMARY"
    dtstart <- timeValueP "DTSTART"
    ignoreLine -- DTEND
    ignoreLine -- SEQUENCE
    ignoreLine -- X-MOZ-GENERATION
    ignoreLine -- DTSTAMP
    location <- keyValueP "LOCATION"
    ignoreLine -- CATEGORIES
    ignoreLine -- DESCRIPTION
    ignoreLine -- CLASS
    pure $ Event summary dtstart location

ignoreLine :: Parser ()
ignoreLine = manyTill anyChar (lookAhead crlf) >> crlf >> pure ()

keyValueP :: String -> Parser Text
keyValueP needed = do
    string needed
    char ':'
    T.pack <$> manyTill anyChar crlf

timeValueP :: String -> Parser Text
timeValueP needed = do
    string needed
    char ';'
    manyTill anyChar (char ':')
    year <- count 4 digitChar
    month <- count 2 digitChar
    day <- count 2 digitChar
    char 'T'
    hours <- count 2 digitChar
    minutes<- count 2 digitChar
    _ <- count 2 digitChar -- seconds
    crlf
    pure . T.pack $
        "T[" <> year <> "-" <> month <> "-" <> day <> ":" <> hours <> ":" <> minutes <> "]"

section :: String -> Parser a -> Parser a
section sectionName inner = do
    string "BEGIN:"
    string sectionName
    crlf
    res <- inner
    string "END:"
    string sectionName
    crlf
    pure res

parseICal :: Text -> IO ()
parseICal input =
    case parse iCalP "" input of
      Right res -> mapM_ TIO.putStrLn res
      Left err -> print err

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> putStrLn "needs argument."
      (fName:_) -> TIO.readFile fName >>= parseICal
