module Data.Morgue.Agenda.Types where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime)

-- | tags are just text values "attached" to agenda elements
newtype Tag = Tag Text
    deriving (Show, Eq)

-- | the kind of timing selected for an agenda element
data TimeMode
    = Time -- ^ an event
    | Deadline -- ^ a task's deadline
    | Scheduled -- ^ a scheduled task
    deriving (Show, Eq)

-- | a time step for reoccuring events
data TimeStep
    = Day -- ^ a day
    | Week -- ^ a week (that is, 7 days for those who're unaware)
    | Month -- ^ a month in the gregorian calendar
    | Year -- ^ a year in the gregorian calendar
    deriving (Show, Eq)

-- | a repetition interval for reoccuring events, expressed in terms of a time step
data RepeatInterval = Interval
    { numSteps :: Integer -- ^ the number of time steps
    , lenSteps :: TimeStep -- ^ the length of the step
    } deriving (Show, Eq)

-- | a timestamp as defined in a markdown file
data Timestamp = Timestamp
    { timeValue :: LocalTime -- ^ the actual time of the timestamp
    , mode :: TimeMode -- ^ the mode of the timestamp
    , repeat :: Maybe RepeatInterval -- ^ an optional repetition specifier
    , toPrint :: Bool -- ^ whether the *time* is set explicitly
    } deriving (Show, Eq)

instance Ord Timestamp where
    (<=) (Timestamp a _ _ _) (Timestamp b _ _ _) = a <= b

-- | the data type used to hold an element of an agenda  
data AgendaElement = Elem
    { description :: Text -- ^ the textual content of the agenda element
    , toDo :: Maybe Bool -- ^ an optional todo status
    , time :: Maybe Timestamp -- ^ an optional timestamp
    , tags :: [Tag] -- ^ an optional set of tags
    } deriving (Show, Eq)

instance Ord AgendaElement where
    (<=) (Elem _ _ a _) (Elem _ _ b _) = a <= b

-- | the kinds of agenda we can get
data AgendaMode
    = Timed Day Integer Bool -- ^ a timed agenda
    | Todo -- ^ a simple tree of todo items
    | Tree -- ^ a simple tree of all items
    deriving (Show, Eq)

-- | a tree of hierarchically ordered agenda elements of any type
data AgendaTree = AgendaTree AgendaElement [AgendaTree]
    deriving (Show, Eq)

-- | the result type represnting an action to take when walking an `AgendaTree`
data AgendaTreeFilter
    = KeepTree -- ^ keep the complete current tree, with no further checks
    | DropTree -- ^ drop the complete current tree, with no further checks
    | KeepTreeAndWalk -- ^ keep the current node, but check subtrees
    | DropTreeAndWalk -- ^ drop the current node, but check subtrees
    deriving (Show, Eq)
