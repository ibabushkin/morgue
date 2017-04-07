{-# LANGUAGE OverloadedStrings #-}
module Data.Morgue.Agenda.Time
    ( getCurrentDay
    , consecutiveDays
    , getNext
    , getDay
    , isOverdue
    , isRelevant
    , formatDay
    , toWeekInfo
    , module Calendar
    , module LocalTime
    , module OrdinalDate
    ) where

import Data.Monoid ((<>))
import Data.Morgue.Agenda.Types
import Data.Text (Text, pack)
import Data.Time.Calendar as Calendar
import Data.Time.Calendar.OrdinalDate as OrdinalDate
import Data.Time.LocalTime as LocalTime

-- | get the current Day
getCurrentDay :: IO Day
getCurrentDay = extractDay <$> getZonedTime
    where extractDay (ZonedTime (LocalTime day _) _) = day

-- | get a list of n consecutive points in time, each one day apart,
-- starting from the given day
consecutiveDays :: Day -> Integer -> [Day]
consecutiveDays day n = map (`addDays` day) [0..n-1]

-- | get the next matching timestamp for a timestamp, if it specifies
-- a repetition interval
getNext :: Timestamp -> Maybe Timestamp
getNext (Timestamp _ _ Nothing _) = Nothing
getNext ts@(Timestamp (LocalTime day tod) _ (Just (Interval n l)) _) =
    Just ts { timeValue = LocalTime (addSteps l day) tod }
    where addSteps Day = addDays n 
          addSteps Week = addDays (7 * n)
          addSteps Month = addGregorianMonthsRollOver n
          addSteps Year = addGregorianYearsRollOver n

-- | extract the day from a timestamp
getDay :: Timestamp -> Day
getDay (Timestamp (LocalTime day _) _ _ _) = day

-- | decide whether a timed element is overdue at a certain day
isOverdue :: Day -> AgendaElement -> Bool 
isOverdue day (Elem _ (Just True) (Just ts) _) = getDay ts < day
isOverdue _ _ = False

-- | decide whether an element is to be included for a certain day
isRelevant :: Day -> AgendaElement -> Bool
isRelevant day (Elem _ _ (Just ts) _) = repeatValid ts
    where repeatValid current
              | getDay current < day = maybe False repeatValid (getNext current)
              | getDay current > day = False
              | otherwise = True
isRelevant _ _ = False

-- | format a day
formatDay :: Day -> Text
formatDay = combine <$> toText . snd . OrdinalDate.mondayStartWeek <*> pack . show
    where combine first second = first <> ", " <> second
          -- TODO: this should be cleaner
          toText 1 = "Monday"
          toText 2 = "Tuesday"
          toText 3 = "Wednesday"
          toText 4 = "Thursday"
          toText 5 = "Friday"
          toText 6 = "Saturday"
          toText 7 = "Sunday"
          toText _ = "Boomtime!! (this is a bug)"

-- | construct a week information record from two days
toWeekInfo :: Maybe (Day, Day) -> WeekInfo
toWeekInfo (Just (f, l))
    | week1 < week2 = MultipleWeeks week1 week2
    | week1 > week2 = MultipleWeeks week2 week1
    | otherwise = OneWeek week1
    where (week1, _) = mondayStartWeek f
          (week2, _) = mondayStartWeek l
toWeekInfo Nothing = NoWeeks
