{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Morgue.Agenda.Render where

import Data.Aeson
import Data.Morgue.Agenda.Types
import Data.Time.Calendar (Day)

import GHC.Generics

import Text.Mustache (Template)
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
timedTemplate = $(compileMustacheDir "timed" "templates/")

todoTemplate :: Template
todoTemplate = $(compileMustacheDir "todo" "templates/")

bothTemplate :: Template
bothTemplate = $(compileMustacheDir "both" "templates/")

treeTemplate :: Template
treeTemplate = $(compileMustacheDir "tree" "templates/")
