{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Morgue.Agenda.Render where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Morgue.Agenda.Types
import Data.Text (stripSuffix)
import Data.Time.Calendar (Day)

import GHC.Generics

import Text.Mustache (Template(..), Node(..))
import Text.Mustache.Compile.TH

newtype TimedAgendaResult = TimedAgendaResult [(Day, AgendaTree)]

instance ToJSON TimedAgendaResult where
    toJSON (TimedAgendaResult days) = object [ "days" .= map pairToJSON days ]
        where pairToJSON (day, tree) = object
                  [ "day" .= toJSON day
                  , "tree" .= toJSON tree
                  ]

newtype TodoAgendaResult = TodoAgendaResult AgendaTree

instance ToJSON TodoAgendaResult where
    toJSON (TodoAgendaResult tree) = object [ "tree" .= toJSON tree ]

data BothAgendaResult = BothAgendaResult
    { timed :: TimedAgendaResult
    , todo :: TodoAgendaResult
    } deriving Generic

instance ToJSON BothAgendaResult

newtype TreeAgendaResult = TreeAgendaResult AgendaTree

instance ToJSON TreeAgendaResult where
    toJSON (TreeAgendaResult tree) = object [ "tree" .= toJSON tree ]

-- | the template to render a timed agenda
timedTemplate :: Template
timedTemplate = cleanTemplate $(compileMustacheDir "timed" "templates/")

-- | the template to render a todo agenda
todoTemplate :: Template
todoTemplate = timedTemplate { templateActual = "todo" }

-- | the template to render a timed and todo agenda
bothTemplate :: Template
bothTemplate = timedTemplate { templateActual = "todo" }

-- | the template to render a tree agenda
treeTemplate :: Template
treeTemplate = timedTemplate { templateActual = "tree" }

-- | "clean" a template - that is, remove trailing newlines that are around because we
-- read from a file created by a human
cleanTemplate :: Template -> Template
cleanTemplate (Template a c) = Template a (clean <$> c)
    where clean = foldr go []
          go (TextBlock t) [] = [TextBlock $ fromMaybe t (stripSuffix "\n" t)]
          go n ns = n:ns

-- | the format to be used when outputting a filtered tree
data OutputFormat
    = Plain -- ^ plain text. boring, but reliable (and machine-readable)
    | Pango -- ^ pango markup. useful for awesomewm or dunst notifications
    | ANSI  -- ^ colored plain text. not as boring
    | Custom FilePath -- ^ custom mustache template passed
    deriving (Show, Eq)

-- | get a template according to output format and agenda mode

-- TODO: make this return an IO Template (since we could need to get one from the path in
-- a `Custom`
dispatchTemplate :: OutputFormat -> AgendaMode -> Template
dispatchTemplate _ (Timed _ both)
    | both = bothTemplate
    | otherwise = todoTemplate
dispatchTemplate _ Todo = todoTemplate
dispatchTemplate _ Tree = treeTemplate
