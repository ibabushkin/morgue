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
repr (Elem d (Just todo) _ _)
    | todo = "[ ] " <> (stripNewline . T.unlines) d
    | otherwise = "[x] " <> (stripNewline . T.unlines) d
repr (Elem d Nothing _ _) = (stripNewline . T.unlines) d

-- | strip the trailing newline from a `Text`, if there is any
stripNewline :: Text -> Text
stripNewline = fromMaybe <$> id <*> stripSuffix "\n"

-- | get a file name from the command line
getFileName :: IO FilePath
getFileName = do
    args <- getArgs
    case args of
      f : _ -> return f
      _ -> error "no file specified"

-- | glue everything together (we use the guarantee that our tree dump ends with a newline)
main :: IO ()
main = getFileName >>= TIO.readFile >>=
    mapM_ (TIO.putStr . dumpOwn 0) . getAgendaTree
