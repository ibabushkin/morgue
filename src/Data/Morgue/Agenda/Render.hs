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

timedTemplate :: Template
timedTemplate = cleanTemplate $(compileMustacheDir "timed" "templates/")

todoTemplate :: Template
todoTemplate = cleanTemplate $(compileMustacheDir "todo" "templates/")

bothTemplate :: Template
bothTemplate = cleanTemplate $(compileMustacheDir "both" "templates/")

treeTemplate :: Template
treeTemplate = cleanTemplate $(compileMustacheDir "tree" "templates/")

cleanTemplate :: Template -> Template
cleanTemplate (Template a c) = Template a (clean <$> c)
    where clean = foldr go []
          go (TextBlock t) [] = [TextBlock $ fromMaybe t (stripSuffix "\n" t)]
          go n ns = n:ns
