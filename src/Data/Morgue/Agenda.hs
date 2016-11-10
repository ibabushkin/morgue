{-# LANGUAGE OverloadedStrings #-}
module Data.Morgue.Agenda where

import CMark

import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Maybe (mapMaybe)
import Data.Morgue.AgendaTypes
import Data.Text (Text)

-- | the options we use while parsing markdown
commonmarkOptions :: [CMarkOption]
commonmarkOptions = [optSafe, optNormalize]

-- | get a markdown AST
parseMarkdown :: Text -> Node
parseMarkdown = commonmarkToNode commonmarkOptions

-- | get a textual representation of a markdown AST segment
formatMarkdown :: Node -> Text
formatMarkdown = nodeToCommonmark commonmarkOptions Nothing

-- | restructure the AST to make it... easier to process
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

-- | more AST restructuring
restoreHierarchy :: Node -> Node
restoreHierarchy = go 1
    where go k (Node p t ns) = Node p t (map (go (k + 1)) (children k ns))
          children k = concatMap nest . splitByHeading k
          nest (Node p' (HEADING j) ns' : nss) = [Node p' (HEADING j) (ns' ++ nss)]
          nest nss = nss

-- | get a specialized agenda-focused AST from the markdown AST
getAgendaTree :: Node -> Maybe (AgendaTree Text)
getAgendaTree (Node _ DOCUMENT ns) = Just . AgendaList $ mapMaybe getAgendaTree ns
getAgendaTree (Node _ (LIST _) ns) = Just . AgendaList $ mapMaybe getAgendaListElem ns
getAgendaTree (Node _ (HEADING _) (Node _ (TEXT t) [] : ns)) =
    Just . AgendaElement t $ mapMaybe getAgendaTree ns
getAgendaTree _ = Nothing

-- | get a list of elements from a markdown AST node
getAgendaListElem :: Node -> Maybe (AgendaTree Text)
getAgendaListElem (Node _ ITEM (Node _ PARAGRAPH ps : ns)) =
    Just $ AgendaElement (getParagraphText ps) (mapMaybe getAgendaTree ns)
getAgendaListElem _ = Nothing

-- | get the text from a paragraph
getParagraphText :: [Node] -> Text
getParagraphText ns = mconcat (map formatMarkdown ns)

-- | parse a checkbox from an agenda entry description
checkboxP :: Parser Bool
checkboxP = (== "[x]") <$> (string "[ ]" <|> string "[x]")

-- | parse a set of tags from an agenda entry description
tagsP :: Parser [Tag]
tagsP = colon *> sepBy (Tag <$> takeTill (== ':')) colon <* colon
    where colon = char ':'

-- | parse a repetition interval from a timestamp
repeatP :: Parser RepeatInterval
repeatP = string "/+" *> (Interval <$> decimal <*> timestepP)
    where timestepP = choice
              [ char 'd' *> pure Day
              , char 'w' *> pure Week
              , char 'm' *> pure Month
              , char 'y' *> pure Year
              ]

modeP :: Parser TimeMode
modeP = choice
    [ char 'D' *> pure Deadline
    , char 'T' *> pure Time
    , char 'S' *> pure Scheduled
    ]
