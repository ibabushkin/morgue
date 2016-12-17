module Data.Morgue.Agenda.Types where

import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)

-- | tags are just text values "attached" to agenda elements
newtype Tag = Tag Text
    deriving (Show, Eq)

-- | the kind of timing selected for an agenda element
data TimeMode
    = Time
    | Deadline
    | Scheduled
    deriving (Show, Eq)

-- | a time step for reoccuring events
data TimeStep
    = Day
    | Week
    | Month
    | Year
    deriving (Show, Eq)

-- | a repetition interval for reoccuring events, expressed in terms of a time step
data RepeatInterval = Interval
    { numSteps :: Integer
    , lenSteps :: TimeStep
    } deriving (Show, Eq)

-- | a timestamp as defined in a markdown file
data Timestamp = Timestamp
    { timeValue :: LocalTime
    , mode :: TimeMode
    , repeat :: Maybe RepeatInterval
    , toPrint :: Bool
    } deriving (Show, Eq)

instance Ord Timestamp where
    (<=) (Timestamp a _ _ _) (Timestamp b _ _ _) = a <= b

-- | the data type used to hold an element of an agenda  
data AgendaElement = Elem
    { description :: Text
    , toDo :: Maybe Bool
    , time :: Maybe Timestamp
    , tags :: [Tag]
    } deriving (Show, Eq)

instance Ord AgendaElement where
    (<=) (Elem _ _ a _) (Elem _ _ b _) = a <= b

-- | the kinds of agenda we can get
data AgendaMode
    = Timed
    | Todo
    | Both
    deriving (Show, Eq)

-- | a tree of hierarchically ordered agenda elements of any type
data AgendaTree
    = AgendaElement AgendaElement [AgendaTree]
    | AgendaList [AgendaTree]
