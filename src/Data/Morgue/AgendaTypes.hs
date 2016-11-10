{-# LANGUAGE DeriveFunctor #-}
module Data.Morgue.AgendaTypes where

import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)

-- | tags are just text values "attached" to agenda elements
newtype Tag = Tag Text
    deriving Eq

-- | the kind of timing selected for an agenda element
data TimeMode
    = Time
    | Deadline
    | Scheduled
    deriving Eq

-- | a time step for reoccuring events
data TimeStep
    = Day
    | Week
    | Month
    | Year
    deriving Eq

-- | a repetition interval for reoccuring events, expressed in terms of a time step
data RepeatInterval = Interval
    { numSteps :: Int
    , lenSteps :: TimeStep
    } deriving Eq

-- | a timestamp as defined in a markdown file
data Timestamp = Timestamp
    { timeValue :: LocalTime
    , mode :: TimeMode
    , repeat :: Maybe RepeatInterval
    , toPrint :: Bool
    } deriving Eq

instance Ord Timestamp where
    (<=) (Timestamp a _ _ _) (Timestamp b _ _ _) = a <= b

-- | the data type used to hold an element of an agenda  
data AgendaElement = Elem
    { description :: Text
    , toDo :: Maybe Bool
    , time :: Maybe Timestamp
    , tags :: [Tag]
    } deriving Eq

instance Ord AgendaElement where
    (<=) (Elem _ _ a _) (Elem _ _ b _) = a <= b

-- | the kinds of agenda we can get
data AgendaMode
    = Timed
    | Todo
    | Both
    deriving Eq

-- | a filter used for tags
type Filter = [AgendaElement] -> [AgendaElement]

-- | a tree of hierarchically ordered agenda elements of any type
data AgendaTree a
    = AgendaElement a [AgendaTree a]
    | AgendaList [AgendaTree a]
    deriving Functor
