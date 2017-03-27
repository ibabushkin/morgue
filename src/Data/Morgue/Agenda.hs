{-# LANGUAGE OverloadedStrings #-}
module Data.Morgue.Agenda (getAgendaTree) where

import CMark

import Control.Applicative ((<|>), optional)
import Control.Monad (mzero)

--import Data.Attoparsec.Text
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Morgue.Agenda.Types
import Data.Text (Text, pack, stripSuffix)
import qualified Data.Text as T
import Data.Time (LocalTime, defaultTimeLocale)
import Data.Time.Format (parseTimeM)

import Text.Megaparsec
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.Text

-- | the options we use while parsing markdown
commonmarkOptions :: [CMarkOption]
commonmarkOptions = [optSafe, optNormalize]

-- | get a markdown AST
parseMarkdown :: Text -> Node
parseMarkdown = commonmarkToNode commonmarkOptions

-- | get a textual representation of a markdown AST segment
formatMarkdown :: Node -> Text
formatMarkdown = T.filter (/= '\\') . nodeToCommonmark commonmarkOptions Nothing

-- | restructure the AST to make it... easier to process
splitByHeading :: Level -> [Node] -> [[Node]]
splitByHeading k = foldr go [[]]
    where go new@(Node _ (HEADING j) _) (n : ns)
              | j == k = [] : (new : n) : ns
              | otherwise = (new : n) : ns
          go new (n:ns) = (new : n) : ns

-- | more AST restructuring
restoreHierarchy :: Node -> Node
restoreHierarchy = go 1
    where go k (Node p t ns) = Node p t (map (go (k + 1)) (children k ns))
          children k = concatMap nest . splitByHeading k
          nest (Node p' h@(HEADING _) [] : nss) = [Node p' h nss]
          nest nss = nss

-- | get a specialized agenda-focused AST from the cleaned CMark AST
getAgendaTreeFromNode :: Node -> Maybe AgendaTree
getAgendaTreeFromNode n@(Node _ DOCUMENT _) =
    Just $ AgendaTree rootNode (getChildren n)
    where rootNode = Elem "root" Nothing Nothing []
getAgendaTreeFromNode (Node _ ITEM (Node _ PARAGRAPH ps : ns)) =
    AgendaTree <$> parseElement (getParagraphText ps) <*> getGrandchildren ns
getAgendaTreeFromNode (Node _ (HEADING _) (Node _ (TEXT t) [] : ns)) =
    AgendaTree <$> parseElement t <*> getGrandchildren ns
getAgendaTreeFromNode _ = Nothing

-- | get a specialized agenda-focused AST from a `Text`
getAgendaTree :: Text -> Maybe AgendaTree
getAgendaTree = getAgendaTreeFromNode . restoreHierarchy . parseMarkdown

-- | get the children of a node as `AgendaTree`s
getChildren :: Node -> [AgendaTree]
getChildren (Node _ (LIST _) ns) = mapMaybe getAgendaTreeFromNode ns
getChildren (Node _ _ ns) = foldr go [] ns
    where go n ts = case getAgendaTreeFromNode n of
                      Just t -> t : ts
                      Nothing -> getChildren n ++ ts

-- | get the grandchildren as `AgendaTree`s, given the list of child nodes
getGrandchildren :: Applicative f => [Node] -> f [AgendaTree]
getGrandchildren = pure . concatMap getChildren

-- | get the text from a paragraph
getParagraphText :: [Node] -> Text
getParagraphText =
    (fromMaybe <$> id <*> stripSuffix "\n") . mconcat . map formatMarkdown

-- | wrap the agenda element description parser
parseElement :: Text -> Maybe AgendaElement
parseElement input =
    case parse elementP "" input of
      Right res -> Just res
      Left _ -> Nothing

-- | parse an an agenda entry description
elementP :: Parser AgendaElement
elementP = do
    td <- optional (checkboxP <* space)
    ts <- optional (timestampP <* space)
    tg <- fromMaybe [] <$> optional (tagsP <* space)
    de <- pack <$> some anyChar -- TODO: no pack
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
    timestr <- some (noneOf ['/',']'])
    r <- optional repeatP
    _ <- char ']'
    case parseTime timestr of
      Just (t, tp) -> return $ Timestamp t m r tp
      Nothing -> mzero

-- | parse a textual date/time representation and check what format it has in the process
parseTime :: String -> Maybe (LocalTime, Bool)
parseTime str =
    case parseTime' "%d.%m.%Y:%H:%M" str of
      Nothing -> (,) <$> parseTime' "%d.%m.%Y" str <*> pure False
      res -> (,) <$> res <*> pure True
    where parseTime' = parseTimeM False defaultTimeLocale

-- | parse a repetition interval from a timestamp
repeatP :: Parser RepeatInterval
repeatP = string "/+" *> (Interval <$> integer <*> timestepP)
    where timestepP = choice
              [ char 'd' *> pure Day
              , char 'w' *> pure Week
              , char 'm' *> pure Month
              , char 'y' *> pure Year
              ]

-- | parse a set of tags from an agenda entry description
tagsP :: Parser [Tag]
tagsP = colon *> sepEndBy1 (Tag . pack <$> some alphaNumChar) colon -- TODO: no pack
    where colon = char ':'
