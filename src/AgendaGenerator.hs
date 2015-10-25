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

-- Tags
type Tag = String

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
data Timestamp = Timestamp { timeValue :: LocalTime
                           , mode :: TimeMode
                           , repeat :: Maybe RepeatInterval
                           , toPrint :: Bool
                           } deriving (Show, Read, Eq)

-- make ordering possible
instance Ord Timestamp where
    (<=) (Timestamp a _ _ _) (Timestamp b _ _ _) = a <= b

-- get the next 7 days beginning with a given date
getFollowingDays :: LocalTime -> Integer -> [LocalTime]
getFollowingDays d n = map (\i -> d{localDay = addDays i (localDay d)}) [0..n]

-- get the next date from a Timestamp with a repetition
getNextTime :: Timestamp -> Maybe Timestamp
getNextTime (Timestamp _ _ Nothing _) = Nothing
getNextTime ts@(Timestamp t _ (Just (Interval n l)) _)
    | l == Day = Just $ ts{
        timeValue = (timeValue ts){
            localDay = addDays step day}}
    | l == Week = Just $ ts{
        timeValue = (timeValue ts){
            localDay = addDays (7*step) day}}
    | l == Month = Just $ ts{
        timeValue = (timeValue ts){
            localDay = addGregorianMonthsRollOver step day}}
    | l == Year = Just $ ts{
        timeValue = (timeValue ts){
            localDay = addGregorianYearsRollOver step day}}
    where day = localDay $ timeValue ts
          step = toInteger n

-- the data type used to hold an element of an agenda  
data AgendaElement = Elem { description :: [Inline]
                          , toDo :: Maybe Bool
                          , time :: Maybe Timestamp
                          , tags :: [Tag]
                          } deriving (Show, Read, Eq)

-- make ordering possible
instance Ord AgendaElement where
    (<=) (Elem _ _ a _) (Elem _ _ b _) = a <= b

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
isRelevant :: LocalTime -> AgendaElement -> Bool
isRelevant (LocalTime d' _) (Elem _ _ (Just ts@(Timestamp (LocalTime d _) _ r _)) _) =
    d == d' || repeatValid d' ts
        where repeatValid day start = case getNextTime start of
                     (Just ts'@(Timestamp (LocalTime next _) _ _ _))
                         | next > day -> False
                         | next < day -> repeatValid day ts'
                         | otherwise  -> True
                     Nothing -> False  
isRelevant _ _ = False

-- decide whether a timed element is overdue at a certain day
isOverdue :: LocalTime -> AgendaElement -> Bool 
isOverdue (LocalTime d' _)
          (Elem _ (Just True)
          (Just (Timestamp (LocalTime d _) _ _ _)) _) =
    d < d'
isOverdue _ _ = False

-- a filter used for tags
type Filter = [AgendaElement] -> [AgendaElement]

-- return a string representing our agenda
writeAgenda :: AgendaMode
            -> Either PandocError Pandoc
            -> [LocalTime]
            -> OutputFormat
            -> Filter
            -> String
writeAgenda mode (Right (Pandoc _ blocks)) days outFormat tagFilter
    | mode == Timed = writeAgendaTimed blocks days outFormat tagFilter
    | mode == Todo  = writeAgendaTodo blocks outFormat tagFilter
    | otherwise     = writeAgendaTimed blocks days outFormat tagFilter
                         ++ "\n\n" ++ writeAgendaTodo blocks outFormat tagFilter
writeAgenda _ _ _ _ _ = error "Pandoc error occured!"

-- write an agenda for a number of days
writeAgendaTimed :: [Block] -> [LocalTime] -> OutputFormat -> Filter -> String
writeAgendaTimed blocks days outFormat tagFilter =
    header ++ formatOverdue outFormat overdueElements ++ "\n" ++
        intercalate "\n" (map (formatDay outFormat) weekdayElements)
    where elements = tagFilter $ processBlocks [] blocks
          overdueElements = filter (isOverdue $ head days) elements
          weekdayElements = map (\d -> (d, filter (isRelevant d) elements)) days
          header = format outFormat $ "Week agenda (" ++ weeks ++ "):\n"
          week1 = formatTime defaultTimeLocale "%V" $ head days
          week2 = formatTime defaultTimeLocale "%V" $ last days 
          weeks | week1 == week2 = 'W':week1
                | otherwise = 'W':week1 ++ "-" ++ 'W':week2

-- write an agenda with all TODO's that don't have a date assigned
writeAgendaTodo :: [Block] -> OutputFormat -> Filter -> String
writeAgendaTodo blocks outFormat tagFilter = 
    header ++ (intercalate "\n" . map (show' outFormat)) (filter helper elements)
    where elements = tagFilter $ processBlocks [] blocks
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
formatDay :: OutputFormat -> (LocalTime, [AgendaElement]) -> String
formatDay outFormat (t, es) =
    format outFormat (formatTime defaultTimeLocale "%A, %d.%m.%Y:\n" t) ++ agenda
    where agenda = (intercalate "\n" . sort) $
             map (('\t':) . show' outFormat) es

-- process blocks
processBlocks :: [Tag] -> [Block] -> [AgendaElement]
processBlocks ts = concat . map (processBlock ts)

-- process a block
-- lists are interpreted as lists of tasks, apointments etc.
processBlock :: [Tag] -> Block -> [AgendaElement]
processBlock ts (BulletList bs) = processList ts bs
processBlock ts (OrderedList _ bs) = processList ts bs
processBlock _ _ = []

-- process a bullet list
-- a list consists of a list of elements
processList :: [Tag] -> [[Block]] -> [AgendaElement]
processList ts = concat . map (processElement ts)

-- process list element
-- a list element consists of a list of blocks
processElement :: [Tag] -> [Block] -> [AgendaElement]
processElement ts (b:bs) = e:es
    where e = processElementBlock ts b
          es = processBlocks (tags e) bs
processElement _ [] = error "Empty block." 

-- process a block inside a list element
processElementBlock :: [Tag] -> Block -> AgendaElement
processElementBlock ts (Plain is) = processElementBlock' ts is
processElementBlock ts (Para is)  = processElementBlock' ts is 

-- helper
processElementBlock' :: [Tag] -> [Inline] -> AgendaElement
processElementBlock' ts is = Elem is''' todo time (ts ++ tags)
    where (todo, is'  ) = getCheckboxFromElementBlock is
          (time, is'' ) = getTimeFromElementBlock is'
          (tags, is''') = getTagsFromElementBlock is''

-- get a checkbox state and the rest of the block without any trailing spaces
getCheckboxFromElementBlock :: [Inline] -> (Maybe Bool, [Inline])
getCheckboxFromElementBlock is
    | td `isPrefixOf` is = (Just True, drop 4 is) -- drop the space
    | dn `isPrefixOf` is = (Just False, drop 2 is)  -- same here
    | otherwise = (Nothing, is)
    where td = [Str "[", Space, Str "]"]
          dn = [Str "[x]"]

getTagsFromElementBlock :: [Inline] -> ([Tag], [Inline])
getTagsFromElementBlock is = 
    case reverse is of
      (Str s:is') -> process s is'
      _ -> ([], is)
    where process (':':s') is' =
              case last s' of
                ':' -> (splitOn ":" (init s'), (reverse is'))
                _ -> ([], is)
          process _ _ = ([], is)

-- get a time and the rest of the block 
getTimeFromElementBlock :: [Inline] -> (Maybe Timestamp, [Inline])
getTimeFromElementBlock fis@(Str (m:s):is) 
    | (head s == '[') && (last s == ']') =
        (Just (Timestamp timeval mode rep print), tail is)
    | otherwise = (Nothing, fis) -- "full" is
    where (ftoken:tokens) = splitOn "/" . tail $ init s
          (timeval, print)
              | ':' `elem` ftoken = (parseTimeOrError True defaultTimeLocale
                                        "%d.%m.%Y:%H:%M" ftoken :: LocalTime, True)
              | otherwise = (parseTimeOrError True defaultTimeLocale
                                "%d.%m.%Y" ftoken :: LocalTime, False)
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
