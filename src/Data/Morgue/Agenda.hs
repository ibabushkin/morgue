{-# LANGUAGE OverloadedStrings #-}
module Data.Morgue.Agenda (getAgendaTree) where

import CMark

import Control.Applicative ((<|>), optional)
import Control.Monad (mzero)

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Morgue.Agenda.Types
import Data.Text (Text, pack, stripSuffix)
import qualified Data.Text as T
import Data.Time (LocalTime, defaultTimeLocale)
import Data.Time.Format (parseTimeM)

import Text.Megaparsec
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.Text

-- | get a specialized agenda-focused AST from a `Text`
getAgendaTree :: Text -> [AgendaTree]
getAgendaTree = getAgendaTreeFromDocument . restoreHierarchy . parseMarkdown
    where getAgendaTreeFromDocument n@(Node _ DOCUMENT _) = getChildren n
          getAgendaTreeFromDocument _ = []

-- | the options we use while parsing markdown
commonmarkOptions :: [CMarkOption]
commonmarkOptions = [optSafe, optNormalize]

-- | get a markdown AST
parseMarkdown :: Text -> Node
parseMarkdown = commonmarkToNode commonmarkOptions

-- | get a textual representation of a markdown AST segment
formatMarkdown :: Node -> Text
formatMarkdown = stripNewline . clean . nodeToCommonmark commonmarkOptions Nothing
    where stripNewline "\n" = "\n"
          stripNewline s = fromMaybe s (stripSuffix "\n" s)
          clean = T.filter (/= '\\')

-- | restructure the AST to make it... easier to process
splitByHeading :: Level -> [Node] -> [[Node]]
splitByHeading k = foldr go [[]]
    where go new@(Node _ (HEADING j) _) (n : ns)
              | j == k = [] : (new : n) : ns
              | otherwise = (new : n) : ns
          go new (n:ns) = (new : n) : ns
          go _ [] = [[]] -- unreachable

-- | more AST restructuring
restoreHierarchy :: Node -> Node
restoreHierarchy = go 1
    where go k (Node p t ns) = Node p t (map (go (k + 1)) (children k ns))
          children k = concatMap nest . splitByHeading k
          nest (Node p' h@(HEADING _) cs : nss) = [Node p' h (cs ++ nss)]
          nest nss = nss

-- | get a specialized agenda-focused AST from the cleaned CMark AST
getAgendaTreeFromNode :: Node -> Maybe AgendaTree
getAgendaTreeFromNode (Node _ ITEM (Node _ PARAGRAPH ps : ns)) =
    AgendaTree <$> parseElement (getParagraphText ps) <*> getSubtree ns
getAgendaTreeFromNode (Node _ (HEADING _) (Node _ (TEXT t) [] : ns)) =
    AgendaTree <$> parseElement t <*> getSubtree ns
getAgendaTreeFromNode _ = Nothing

-- | get the children of a node as `AgendaTree`s
getChildren :: Node -> [AgendaTree]
getChildren (Node _ (LIST _) ns) = mapMaybe getAgendaTreeFromNode ns
getChildren (Node _ _ ns) = foldr go [] ns
    where go n ts = case getAgendaTreeFromNode n of
                      Just t -> t : ts
                      Nothing -> getChildren n ++ ts

-- | get subtrees as `AgendaTree`s, given the list of child nodes
getSubtree :: Applicative f => [Node] -> f [AgendaTree]
getSubtree = pure . concatMap buildTree
    where buildTree n@(Node _ (LIST _) _) = getChildren n
          buildTree n = fromMaybe [] $ pure <$> getAgendaTreeFromNode n

-- | get the text from a paragraph
getParagraphText :: [Node] -> Text
getParagraphText = mconcat . map formatMarkdown

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
    ts <- optional (try timestampP <* space)
    de <- pack <$> manyTill anyChar endP
    tg <- try tagsP <|> pure []
    return $ Elem (map T.strip $ T.lines de) td ts tg

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
parseTime :: String -> Maybe (LocalTime, TimestampDisplay)
parseTime str =
    case parseTime' "%F:%R" str of
      Nothing -> (,) <$> parseTime' "%F" str <*> pure FullWithoutTime
      res -> (,) <$> res <*> pure FullWithTime
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
tagsP = string " :" *> sepEndBy (Tag . pack <$> some alphaNumChar) colon <* colon
    where colon = char ':'

-- | parse the end of a regular entry text
endP :: Parser ()
endP = (lookAhead tagsP *> pure ()) <|> eof
