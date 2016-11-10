{-# LANGUAGE OverloadedStrings #-}
module Main where

import CMark (Node(..))

import Data.Monoid ((<>))
import Data.Morgue.Agenda
import Data.Morgue.AgendaTypes
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO

import System.Environment (getArgs)

-- | generate indent
indent :: Int -> Text
indent k = pack (replicate k ' ')

-- | dump a markdown AST
dump :: Int -> Node -> Text
dump k (Node _ nType ns) =
    indent k <> pack (show nType) <> "\n" <> mconcat (map (dump (k + 1)) ns)

-- | dump our own, agenda specific AST
dumpOwn :: Int -> AgendaTree Text -> Text
dumpOwn k (AgendaElement t ns) =
    indent k <> "elem: " <> t <> "\n" <> mconcat (map (dumpOwn (k + 1)) ns)
dumpOwn k (AgendaList ns) = 
    indent k <> "list: \n" <> mconcat (map (dumpOwn (k + 1)) ns)

-- | get a file name from the command line
getFileName :: IO FilePath
getFileName = do
    args <- getArgs
    case args of
      f:_ -> return f
      _ -> error "no file specified"

-- | glue everything together
main :: IO ()
main = getFileName >>= TIO.readFile >>=
    TIO.putStrLn . maybe "tree transform failed" (dumpOwn 0) . pipeline
    where pipeline = getAgendaTree . restoreHierarchy . parseMarkdown
