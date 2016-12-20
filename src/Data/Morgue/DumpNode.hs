{-# LANGUAGE OverloadedStrings #-}
module Main where

import CMark (Node(..))

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Morgue.Agenda
import Data.Morgue.Agenda.Types
import Data.Text (Text, pack, stripSuffix)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Environment (getArgs)

-- | generate indent
indent :: Int -> Text
indent k = T.replicate k "  "

-- | dump a markdown AST
dump :: Int -> Node -> Text
dump k (Node _ nType ns) =
    indent k <> pack (show nType) <> "\n" <> mconcat (map (dump (k + 1)) ns)

-- | dump our own, agenda specific AST
dumpOwn :: Int -> AgendaTree -> Text
dumpOwn k (AgendaTree t []) = indent k <> "elem: " <> repr t <> "\n"
dumpOwn k (AgendaTree t ns) =
    indent k <> "elem: " <> repr t <> "\n" <> mconcat (map (dumpOwn (k + 1)) ns)

-- | render an `AgendaElement`
repr :: AgendaElement -> Text
repr (Elem d (Just tD) _ _) = reprT <> fromMaybe d (stripSuffix "\n" d)
    where reprT = if tD then "[ ] " else "[x] "
repr (Elem d Nothing _ _) = fromMaybe d (stripSuffix "\n" d)

-- | get a file name from the command line
getFileName :: IO FilePath
getFileName = do
    args <- getArgs
    case args of
      f : _ -> return f
      _ -> error "no file specified"

-- | glue everything together
main :: IO ()
main = getFileName >>= TIO.readFile >>=
    TIO.putStrLn . maybe "tree transform failed" (dumpOwn 0) . pipeline
    where pipeline = getAgendaTree . restoreHierarchy . parseMarkdown
