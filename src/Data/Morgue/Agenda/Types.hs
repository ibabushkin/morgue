{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Morgue.Agenda.Types where

import Data.Aeson
import Data.Maybe (mapMaybe)
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import GHC.Generics

-- | tags are just text values "attached" to agenda elements
newtype Tag = Tag Text
    deriving (Eq, Generic, Show)

instance ToJSON Tag

-- | the kind of timing selected for an agenda element
data TimeMode
    = Time -- ^ an event
    | Deadline -- ^ a task's deadline
    | Scheduled -- ^ a scheduled task
    deriving (Eq, Generic, Show)

instance ToJSON TimeMode

-- | a time step for reoccuring events
data TimeStep
    = Day -- ^ a day
    | Week -- ^ a week (that is, 7 days for those who're unaware)
    | Month -- ^ a month in the gregorian calendar
    | Year -- ^ a year in the gregorian calendar
    deriving (Eq, Generic, Show)

instance ToJSON TimeStep

-- | a repetition interval for reoccuring events, expressed in terms of a time step
data RepeatInterval = Interval
    { numSteps :: Integer -- ^ the number of time steps
    , lenSteps :: TimeStep -- ^ the length of the step
    } deriving (Eq, Generic, Show)

instance ToJSON RepeatInterval

-- | a timestamp as defined in a markdown file
data Timestamp = Timestamp
    { timeValue :: LocalTime -- ^ the actual time of the timestamp
    , mode :: TimeMode -- ^ the mode of the timestamp
    , repeatInt :: Maybe RepeatInterval -- ^ an optional repetition specifier
    , toPrint :: Bool -- ^ whether the *time* is set explicitly
    } deriving (Eq, Generic, Show)

instance Ord Timestamp where
    (<=) (Timestamp a _ _ _) (Timestamp b _ _ _) = a <= b

instance ToJSON Timestamp where
    toJSON Timestamp{..} = object
        [ "timeValue" .= formatTime defaultTimeLocale "%R" timeValue
        , "mode" .= mode
        , "toPrint" .= toPrint
        ]

-- | a data block describing the timespan covered by a timed agenda, in week numbers
data WeekInfo
    = OneWeek Int -- ^ the timespan fits into the given week
    | MultipleWeeks Int Int -- ^ the timespan overlaps with these two weeks
    | NoWeeks -- ^ edge case: the timespan spans no days at all
    deriving (Eq, Generic, Show)

instance ToJSON WeekInfo where
    toJSON (OneWeek w) = object [ "week" .= w ]
    toJSON (MultipleWeeks w1 w2) = object [ "week" .= w1, "week2" .= w2 ]
    toJSON NoWeeks = Null

instance Semigroup WeekInfo where
    NoWeeks <> r = r
    r <> NoWeeks = r
    (OneWeek l) <> (OneWeek r)
        | l < r = MultipleWeeks l r
        | l > r = MultipleWeeks r l
        | otherwise = OneWeek l
    (MultipleWeeks l1 l2) <> (MultipleWeeks r1 r2)
        | l1 <= r1 && l2 <= r2 = MultipleWeeks l1 r2
        | l1 <= r1 && l2 >  r2 = MultipleWeeks l1 l2
        | l1 >  r1 && l2 <= r2 = MultipleWeeks r1 r2
        | l1 >  r1 && l2 >  r2 = MultipleWeeks r1 l2
    (MultipleWeeks l1 l2) <> (OneWeek r)
        | r < l1 = MultipleWeeks r l2
        | r > l2 = MultipleWeeks l1 r
        | otherwise = MultipleWeeks l1 l2
    (OneWeek l) <> (MultipleWeeks r1 r2)
        | l < r1 = MultipleWeeks l r2
        | l > r2 = MultipleWeeks r1 l
        | otherwise = MultipleWeeks r1 r2

instance Monoid WeekInfo where
    mempty = NoWeeks
    mappend = (<>)

-- | the data type used to hold an element of an agenda  
data AgendaElement = Elem
    { description :: [Text] -- ^ the textual content of the agenda element, split by lines
    , toDo :: Maybe Bool -- ^ an optional todo status
    , time :: Maybe Timestamp -- ^ an optional timestamp
    , tags :: [Tag] -- ^ an optional set of tags
    } deriving (Eq, Generic, Show)

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
    deriving (Eq, Show)

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

-- | a file containing a number of agenda trees
data AgendaFile = AgendaFile
    { fName :: Text -- ^ the name of the file
    , trees :: [AgendaTree] -- ^ a list of agenda trees from the file
    } deriving (Eq, Generic, Show)

instance ToJSON AgendaFile

liftFile :: (AgendaTree -> Maybe AgendaTree) -> AgendaFile -> Maybe AgendaFile
liftFile f (AgendaFile name trees) =
    case mapMaybe f trees of
      [] -> Nothing
      ts -> Just $ AgendaFile name ts

-- | the kinds of agenda we can get
data AgendaMode
    = Timed Integer Bool -- ^ a timed agenda
    | Todo -- ^ a simple tree of todo items
    | Tree -- ^ a simple tree of all items
    deriving (Eq, Show)

-- | the result type represnting an action to take when walking an `AgendaTree`
data AgendaTreeFilter
    = KeepTree -- ^ keep the complete current tree, with no further checks
    | DropTree -- ^ drop the complete current tree, with no further checks
    | KeepTreeAndWalk -- ^ keep the current node, but check subtrees
    | DropTreeAndWalk -- ^ drop the current node, but check subtrees
    deriving (Eq, Show)
