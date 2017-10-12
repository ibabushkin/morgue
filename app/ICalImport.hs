{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Text.Megaparsec
import Text.Megaparsec.Text

import System.Environment (getArgs)

parseICal :: Text -> IO ()
parseICal input =
    case parse iCalP "" input of
      Right res -> print res
      Left err -> print err

iCalP :: Parser [()]
iCalP = section "VCALENDAR" $ do
    string "VERSION:" >> manyTill anyChar newline
    string "METHOD:" >> manyTill anyChar newline
    string "PRODID:" >> manyTill anyChar newline
    newline
    -- skip the timezone info
    section "VTIMEZONE" anyChar
    newline
    sepEndBy sectionP newline

sectionP :: Parser ()
sectionP = pure ()

section :: String -> Parser a -> Parser a
section sectionName inner = do
    string "BEGIN:"
    string sectionName
    newline
    res <- inner
    string "END:"
    string sectionName
    newline
    pure res

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> putStrLn "needs argument."
      (fName:_) -> TIO.readFile fName >>= parseICal
