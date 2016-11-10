module Data.Morgue.Agenda where

import CMark

import Data.Maybe (mapMaybe)
import Data.Morgue.AgendaTypes
import Data.Text (Text)

commonmarkOptions :: [CMarkOption]
commonmarkOptions = [optSafe, optNormalize]

parseMarkdown :: Text -> Node
parseMarkdown = commonmarkToNode commonmarkOptions

formatMarkdown :: Node -> Text
formatMarkdown = nodeToCommonmark commonmarkOptions Nothing

splitByHeading :: Level -> [Node] -> [[Node]]
splitByHeading k = foldr go []
    where go new@(Node _ (HEADING j) _) (n : ns)
              | j == k = [] : (new : n) : ns
              | otherwise = (new : n) : ns
          go new@(Node _ (HEADING j) _) []
              | j == k = [[], [new]]
              | otherwise = [[new]]
          go new (n:ns) = (new : n) : ns
          go new [] = [[new]]

restoreHierarchy' :: Level -> Node -> Node
restoreHierarchy' k (Node p t ns) =
    Node p t (map (restoreHierarchy' (k+1)) (concatMap nest children))
    where children = splitByHeading k ns
          nest (Node p' (HEADING j) ns' : nss) = [Node p' (HEADING j) (ns' ++ nss)]
          nest nss = nss

restoreHierarchy :: Node -> Node
restoreHierarchy = restoreHierarchy' 1

getAgendaTree :: Node -> Maybe (AgendaTree Text)
getAgendaTree (Node _ DOCUMENT ns) = Just . AgendaList $ mapMaybe getAgendaListElem ns
getAgendaTree (Node _ (LIST _) ns) = Just . AgendaList $ mapMaybe getAgendaListElem ns
getAgendaTree (Node _ (HEADING _) (Node _ (TEXT t) [] : ns)) =
    Just . AgendaElement t $ mapMaybe getAgendaTree ns
getAgendaTree _ = Nothing

getAgendaListElem :: Node -> Maybe (AgendaTree Text)
getAgendaListElem (Node _ ITEM (Node _ PARAGRAPH ps : ns)) =
    Just $ AgendaElement (getParagraphText ps) (mapMaybe getAgendaTree ns)
getAgendaListElem _ = Nothing

getParagraphText :: [Node] -> Text
getParagraphText ns = mconcat (map formatMarkdown ns)

{-
    ( defaultOptions
    , runAgenda
    , getAgenda
    ) where
-- Interface to agenda generation, encapsulating options etc.

import Data.Time

import Text.Pandoc

import Data.Morgue.AgendaGenerator
import Data.Morgue.Format
import Data.Morgue.Options
import Data.Morgue.Util

-- | default options: 1 week agenda, output on stdout, ANSI coloring
defaultOptions :: Options
defaultOptions = AgendaOptions
    { optMode = Both
    , optDoubleSpaces = False
    , optTags = Nothing
    , optSkipTags = Nothing
    , optNumDays = 6
    , optOutput = putStrLn
    , optFormat = ANSI
    }

-- | perform computations based on options given
getAgenda :: Options -> TimeZone -> UTCTime -> String -> String
getAgenda opts tz time input =
    writeAgenda optMode pandoc days optFormat (tagFilter optTags optSkipTags)
    where AgendaOptions{..} = opts
          currentDay = utcToLocalTime tz time
          days = getFollowingDays currentDay optNumDays
          readerOpts = def { readerParseRaw = False }
          pandoc = readMarkdown readerOpts $ doubleSpaces optDoubleSpaces input


-- | output an agenda based on options
runAgenda :: Options -> String -> IO ()
runAgenda opts input = (getAgenda opts <$> getCurrentTimeZone <*>
    getCurrentTime <*> pure input) >>= optOutput opts
-}
