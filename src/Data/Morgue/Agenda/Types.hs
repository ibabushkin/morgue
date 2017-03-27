{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Morgue.Agenda.Types where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime (LocalTime)

import GHC.Generics

-- | tags are just text values "attached" to agenda elements
newtype Tag = Tag Text
    deriving (Generic, Show, Eq)

instance ToJSON Tag

-- | the kind of timing selected for an agenda element
data TimeMode
    = Time -- ^ an event
    | Deadline -- ^ a task's deadline
    | Scheduled -- ^ a scheduled task
    deriving (Generic, Show, Eq)

instance ToJSON TimeMode

-- | a time step for reoccuring events
data TimeStep
    = Day -- ^ a day
    | Week -- ^ a week (that is, 7 days for those who're unaware)
    | Month -- ^ a month in the gregorian calendar
    | Year -- ^ a year in the gregorian calendar
    deriving (Generic, Show, Eq)

instance ToJSON TimeStep

-- | a repetition interval for reoccuring events, expressed in terms of a time step
data RepeatInterval = Interval
    { numSteps :: Integer -- ^ the number of time steps
    , lenSteps :: TimeStep -- ^ the length of the step
    } deriving (Generic, Show, Eq)

instance ToJSON RepeatInterval

-- | a timestamp as defined in a markdown file
data Timestamp = Timestamp
    { timeValue :: LocalTime -- ^ the actual time of the timestamp
    , mode :: TimeMode -- ^ the mode of the timestamp
    , repeat :: Maybe RepeatInterval -- ^ an optional repetition specifier
    , toPrint :: Bool -- ^ whether the *time* is set explicitly
    } deriving (Generic, Show, Eq)

instance Ord Timestamp where
    (<=) (Timestamp a _ _ _) (Timestamp b _ _ _) = a <= b

instance ToJSON Timestamp

-- | the data type used to hold an element of an agenda  
data AgendaElement = Elem
    { description :: Text -- ^ the textual content of the agenda element
    , toDo :: Maybe Bool -- ^ an optional todo status
    , time :: Maybe Timestamp -- ^ an optional timestamp
    , tags :: [Tag] -- ^ an optional set of tags
    } deriving (Generic, Show, Eq)

instance Ord AgendaElement where
    (<=) (Elem _ _ a _) (Elem _ _ b _) = a <= b

instance ToJSON AgendaElement where
    toJSON Elem{..} = object
        [ "description" .= description
        , "toDo" .= toDoToJSON toDo
        , "time" .= time
        , "tags" .= tags
        ]

-- | render todo markers to a nice view known from orgmode
toDoToJSON :: Maybe Bool -> Value
toDoToJSON Nothing = Null
toDoToJSON (Just val)
    | val = String "TODO"
    | otherwise = String "DONE"

-- | a tree of hierarchically ordered agenda elements of any type
data AgendaTree = AgendaTree AgendaElement [AgendaTree]
    deriving (Show, Eq)

instance ToJSON AgendaTree where
    toJSON = agendaTreeToJSON 0

-- | store explicit information on the indent level in the JSON representing an
-- `AgendaTree`
agendaTreeToJSON :: Integer -> AgendaTree -> Value
agendaTreeToJSON n (AgendaTree e cs) = object
    [ "element" .= e
    , "indent" .= T.replicate (fromIntegral n) " "
    , "children" .= map (agendaTreeToJSON (n + 1)) cs
    ]

-- | the kinds of agenda we can get
data AgendaMode
    = Timed Integer Bool -- ^ a timed agenda
    | Todo -- ^ a simple tree of todo items
    | Tree -- ^ a simple tree of all items
    deriving (Show, Eq)

-- | the result type represnting an action to take when walking an `AgendaTree`
data AgendaTreeFilter
    = KeepTree -- ^ keep the complete current tree, with no further checks
    | DropTree -- ^ drop the complete current tree, with no further checks
    | KeepTreeAndWalk -- ^ keep the current node, but check subtrees
    | DropTreeAndWalk -- ^ drop the current node, but check subtrees
    deriving (Show, Eq)
