{-# LANGUAGE OverloadedStrings #-}
module Data.Morgue.Agenda where

import CMark

import Control.Applicative ((<|>), optional)
import Control.Monad (mzero)

import Data.Attoparsec.Text
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Morgue.AgendaTypes
import Data.Text (Text)
import Data.Time (LocalTime, defaultTimeLocale)
import Data.Time.Format (parseTimeM)

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
getAgendaTree :: Node -> Maybe (AgendaTree AgendaElement)
getAgendaTree (Node _ DOCUMENT ns) = Just . AgendaList $ mapMaybe getAgendaTree ns
getAgendaTree (Node _ (LIST _) ns) = Just . AgendaList $ mapMaybe getAgendaListElem ns
getAgendaTree (Node _ (HEADING _) (Node _ (TEXT t) [] : ns)) =
    AgendaElement <$> parseElement t <*> pure (mapMaybe getAgendaTree ns)
getAgendaTree _ = Nothing

-- | get a list of elements from a markdown AST node
getAgendaListElem :: Node -> Maybe (AgendaTree AgendaElement)
getAgendaListElem (Node _ ITEM (Node _ PARAGRAPH ps : ns)) = AgendaElement <$>
    parseElement (getParagraphText ps) <*> pure (mapMaybe getAgendaTree ns)
getAgendaListElem _ = Nothing

-- | get the text from a paragraph
getParagraphText :: [Node] -> Text
getParagraphText ns = mconcat (map formatMarkdown ns)

-- | wrap the agenda element description parser
parseElement :: Text -> Maybe AgendaElement
parseElement input =
    case parseOnly elementP input of
      Right res -> Just res
      Left _ -> Nothing

-- | parse an an agenda entry description
elementP :: Parser AgendaElement
elementP = do
    td <- optional (checkboxP <* space)
    ts <- optional (timestampP <* space)
    tg <- fromMaybe [] <$> optional (tagsP <* space)
    de <- takeText
    return $ Elem de td ts tg

-- | parse a checkbox from an agenda entry description
checkboxP :: Parser Bool
checkboxP = (/= "[x]") <$> (string "[ ]" <|> string "[x]")

-- | parse a timing mode from an agenda entry description
modeP :: Parser TimeMode
modeP = choice
    [ char 'D' *> pure Deadline
    , char 'T' *> pure Time
    , char 'S' *> pure Scheduled
    ]

-- | parse a timestamp from an agenda entry description
timestampP :: Parser Timestamp
timestampP = do
    m <- modeP
    _ <- char '['
    timestr <- many1 . satisfy $ notInClass "/]"
    r <- optional repeatP
    _ <- char ']'
    case parseTime timestr of
      Just (t, tp) -> return $ Timestamp t m r tp
      Nothing -> mzero

-- | parse a textual date/time representation and check what format it has in the process
parseTime :: String -> Maybe (LocalTime, Bool)
-- we use strings because... we have to and attoparsec makes it easy to get them
parseTime str =
    case parseTime' "%d.%m.%Y:%H:%M" str of
      Nothing -> (,) <$> parseTime' "%d.%m.%Y" str <*> pure False
      res -> (,) <$> res <*> pure True
    where parseTime' = parseTimeM False defaultTimeLocale

-- | parse a repetition interval from a timestamp
repeatP :: Parser RepeatInterval
repeatP = string "/+" *> (Interval <$> decimal <*> timestepP)
    where timestepP = choice
              [ char 'd' *> pure Day
              , char 'w' *> pure Week
              , char 'm' *> pure Month
              , char 'y' *> pure Year
              ]

-- | parse a set of tags from an agenda entry description
tagsP :: Parser [Tag]
tagsP = colon *> sepBy (Tag <$> takeWhile1 (`notElem` [':', ' '])) colon <* colon
    where colon = char ':'
