module AgendaGenerator where

import Data.List (intercalate, isPrefixOf, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, isNothing)
import Data.Time
import Data.Time.Calendar
import Data.Time.Format

import Text.Pandoc
import Text.Pandoc.Error

import Format

-- pretty self-explanatory, huh?
data TimeMode = Time | Deadline | Scheduled
    deriving (Show, Read, Eq)

-- a time step: week, day, month, year
data TimeStep = Day | Week | Month | Year
    deriving (Show, Read, Eq)

-- a repetition interval
data RepeatInterval = Interval { numSteps :: Int
                               , lenSteps :: TimeStep
                               } deriving (Show, Read, Eq)

-- a timestamp
data Timestamp = Timestamp { timeValue :: UTCTime
                           , mode :: TimeMode
                           , repeat :: Maybe RepeatInterval
                           , toPrint :: Bool
                           } deriving (Show, Read, Eq)

-- make ordering possible
instance Ord Timestamp where
    (<=) (Timestamp a _ _ _) (Timestamp b _ _ _) = a <= b

-- get the next date from a Timestamp with a repetition
getNextTime :: Timestamp -> Maybe Timestamp
getNextTime (Timestamp _ _ Nothing _) = Nothing
getNextTime ts@(Timestamp t _ (Just (Interval n l)) _)
    | l == Day = Just $ ts{
        timeValue = (timeValue ts){
            utctDay = addDays step day}}
    | l == Week = Just $ ts{
        timeValue = (timeValue ts){
            utctDay = addDays (7*step) day}}
    | l == Month = Just $ ts{
        timeValue = (timeValue ts){
            utctDay = addGregorianMonthsRollOver step day}}
    | l == Year = Just $ ts{
        timeValue = (timeValue ts){
            utctDay = addGregorianYearsRollOver step day}}
    where day = utctDay $ timeValue ts
          step = toInteger n

-- the data type used to hold an element of an agenda  
data AgendaElement = Elem { description :: [Inline]
                          , toDo :: Maybe Bool
                          , time :: Maybe Timestamp
                          } deriving (Read, Eq)

-- make ordering possible
instance Ord AgendaElement where
    (<=) (Elem _ _ a) (Elem _ _ b) = a <= b

-- what kind of agenda do we want?
data AgendaMode = Timed | Todo | Both deriving (Show, Read, Eq)

-- format agenda elements
show' :: OutputFormat -> AgendaElement -> String
show' outFormat e = getToDo ++ getTimeMode ++ formatInlines (description e)
    where getToDo
             | toDo e == Just True = format outFormat "TODO" ++ ":\t"
             | toDo e == Just False = format outFormat "DONE" ++ ":\t"
             | otherwise = ""
          getTimeMode = case time e of
                          Just (Timestamp t m _ p)
                              | p -> format outFormat (show m) ++ ":\t"
                                 ++ formatTime defaultTimeLocale "%H:%M " t
                              | otherwise -> format outFormat (show m) ++ ":\t"
                          Nothing -> ""

-- decide whether an element is to be included for a certain day  
isRelevant :: UTCTime -> AgendaElement -> Bool
isRelevant (UTCTime d' _) (Elem _ _ (Just ts@(Timestamp (UTCTime d _) _ r _))) =
    d == d' || repeatValid d' ts
        where repeatValid day start = case getNextTime start of
                     (Just ts'@(Timestamp (UTCTime next _) _ _ _))
                         | next > day -> False
                         | next < day -> repeatValid day ts'
                         | otherwise  -> True
                     Nothing -> False  
isRelevant _ _ = False

-- decide whether a timed element is overdue at a certain day
isOverdue :: UTCTime -> AgendaElement -> Bool 
isOverdue (UTCTime d' _)
          (Elem _ (Just True)
          (Just (Timestamp (UTCTime d _) _ _ _))) =
    d < d'
isOverdue _ _ = False

-- return a string representing our agenda
writeAgenda :: AgendaMode
            -> Either PandocError Pandoc
            -> [UTCTime]
            -> OutputFormat
            -> String
writeAgenda mode (Right (Pandoc _ blocks)) days outFormat
    | mode == Timed = writeAgendaTimed blocks days outFormat
    | mode == Todo  = writeAgendaTodo blocks outFormat
    | otherwise     = writeAgendaTimed blocks days outFormat
                         ++ "\n\n" ++ writeAgendaTodo blocks outFormat
writeAgenda _ _ _ _ = error "Pandoc error occured!"

-- write an agenda for a number of days
writeAgendaTimed :: [Block] -> [UTCTime] -> OutputFormat -> String
writeAgendaTimed blocks days outFormat =
    header ++ formatOverdue outFormat overdueElements ++ "\n" ++
        intercalate "\n" (map (formatDay outFormat) weekdayElements)
    where elements = processBlocks blocks
          overdueElements = filter (isOverdue $ head days) elements
          weekdayElements = map (\d -> (d, filter (isRelevant d) elements)) days
          header = format outFormat $ "Week agenda (" ++ weeks ++ "):\n"
          week1 = formatTime defaultTimeLocale "%V" $ head days
          week2 = formatTime defaultTimeLocale "%V" $ last days 
          weeks
              | week1 == week2 = 'W':week1
              | otherwise = 'W':week1 ++ "-" ++ 'W':week2

-- write an agenda with all TODO's that don't have a date assigned
writeAgendaTodo :: [Block] -> OutputFormat -> String
writeAgendaTodo blocks outFormat = 
    header ++ (intercalate "\n" . map (show' outFormat)) (filter helper elements)
    where elements = processBlocks blocks
          helper e = isNothing (time e) && isJust (toDo e)
          header = format outFormat "Global list of TODO's:\n" 

-- format overdue elements
formatOverdue :: OutputFormat -> [AgendaElement] -> String 
formatOverdue outFormat es
    | agenda /= "" = format outFormat "OVERDUE" ++ ":\n" ++ agenda
    | otherwise = ""
    where agenda = intercalate "\n" . sortBy (flip compare) $
             map (('\t':) . show' outFormat) es

-- format a day's agenda 
formatDay :: OutputFormat -> (UTCTime, [AgendaElement]) -> String
formatDay outFormat (t, es) =
    format outFormat (formatTime defaultTimeLocale "%A, %d.%m.%Y:\n" t) ++ agenda
    where agenda = (intercalate "\n" . sort) $
             map (('\t':) . show' outFormat) es

-- process blocks
processBlocks :: [Block] -> [AgendaElement]
processBlocks = concatMap processBlock

-- process a block
-- lists are interpreted as lists of tasks, apointments etc.
processBlock :: Block -> [AgendaElement]
processBlock (BulletList bs) = processList bs
processBlock (OrderedList _ bs) = processList bs
processBlock _ = []

-- process a bullet list
-- a list consists of a list of elements
processList :: [[Block]] -> [AgendaElement]
processList = map processElement

-- process list element
-- a list element consists of a list of blocks
processElement :: [Block] -> AgendaElement
processElement (b:_) = processElementBlock b
processElement [] = error "Empty block." 

-- process a block inside a list element
processElementBlock :: Block -> AgendaElement
processElementBlock (Plain is) = processElementBlock' is
processElementBlock (Para is)  = processElementBlock' is 

-- helper because sometimes where clauses suck :/ 
processElementBlock' :: [Inline] -> AgendaElement
processElementBlock' is = Elem is'' todo time
    where (todo, is' ) = getCheckboxFromElementBlock is
          (time, is'') = getTimeFromElementBlock is'

-- get a checkbox state and the rest of the block without any trailing spaces
getCheckboxFromElementBlock :: [Inline] -> (Maybe Bool, [Inline])
getCheckboxFromElementBlock is
    | td `isPrefixOf` is = (Just True, drop 4 is) -- drop the space
    | dn `isPrefixOf` is = (Just False, drop 2 is)  -- same here
    | otherwise = (Nothing, is)
    where td = [Str "[", Space, Str "]"]
          dn = [Str "[x]"]

-- get a time and the rest of the block 
getTimeFromElementBlock :: [Inline] -> (Maybe Timestamp, [Inline])
getTimeFromElementBlock fis@(Str (m:s):is) 
    | (head s == '[') && (last s == ']') =
        (Just (Timestamp timeval mode rep print), tail is)
    | otherwise = (Nothing, fis) -- "full" is
    where (ftoken:tokens) = splitOn "/" . tail $ init s
          (timeval, print)
              | ':' `elem` ftoken = (parseTimeOrError True defaultTimeLocale
                                        "%d.%m.%Y:%H:%M" ftoken :: UTCTime, True)
              | otherwise = (parseTimeOrError True defaultTimeLocale
                                "%d.%m.%Y" ftoken :: UTCTime, False)
          rep = case tokens of
                     ([r]) -> Just (readR r :: RepeatInterval)
                     _      -> Nothing
          readR ('+':s) = Interval num ts
              where ts = case last s of
                           'd' -> Day
                           'w' -> Week
                           'm' -> Month
                           'y' -> Year
                    num = read (init s) :: Int 
          readR _ = error "Cannot read repeat interval: Wrong format!"
          mode = case m of
                   'T' -> Time
                   'D' -> Deadline
                   'S' -> Scheduled
getTimeFromElementBlock is = (Nothing, is)

-- get the next 7 days beginning with a given date
getFollowingDays :: UTCTime -> Integer -> [UTCTime]
getFollowingDays d n = map (\i -> d{ utctDay = addDays i (utctDay d)}) [0..n]

