{-# LANGUAGE FlexibleContexts #-}

module Data.Morgue.AgendaGenerator where

import Data.List (intercalate, isPrefixOf, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, isNothing)
import Data.Time
import Data.Time.Calendar
import Data.Time.Format

import Text.ParserCombinators.Parsec
import Text.Pandoc
import Text.Pandoc.Error

import Data.Morgue.Format

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
data AgendaElement = Elem { description :: String
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
show' outFormat e = getToDo ++ getTimeMode ++ (description e)
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

-- barebones string-based element parsing
processElementBlock' :: [Tag] -> [Inline] -> AgendaElement
processElementBlock' ts is = addTags ts $ parseElement is

-- parse a list of Inlines as a String
parseElement :: [Inline] -> AgendaElement
parseElement is = case parse elementP "source" (formatInlines is) of
                    Left e -> error $ show e ++ '\n': formatInlines is
                    Right a -> a

-- AgendaElement parser
elementP :: Parser AgendaElement
elementP = do td <- optionMaybe $ checkboxP <* space
              ts <- optionMaybe $ timestampP <* space
              de <- intercalate " " <$> many (try $ word <* option ' ' space)
              tg <- tagsP
              return $ Elem de td ts tg
    where word = (:) <$> noneOf ": " <*> (many $ noneOf " ")

-- checkbox parser
checkboxP :: Parser Bool
checkboxP = (try $ string "[ ]" *> pure True) <|>
    (try $ string "[x]" *> pure False)

-- tags parser
tagsP :: Parser [Tag]
tagsP = option [] . try $ (char ':') *> many1 (many1 (noneOf ":") <* char ':')

timestampP :: Parser Timestamp
timestampP = try $ do
    mode <- modeP
    char '['
    inp <- getInput
    let (format, tp) = getFormat inp
    time <- parseTime format <$>
        (many $ noneOf "/]")
    repeat <- optionMaybe repeatP
    char ']'
    return $ Timestamp time mode repeat tp
    where getFormat i | length i >= 11 && i !! 10 == ':' =
                             ("%d.%m.%Y:%H:%M", True)
                      | otherwise = ("%d.%m.%Y", True)
          parseTime :: String -> String -> LocalTime
          parseTime = parseTimeOrError True defaultTimeLocale

repeatP :: Parser RepeatInterval
repeatP = try $ char '/' *> char '+' *>
    (Interval <$> read <$> (many digit) <*> (getInterval <$> oneOf "dwmy"))
    where getInterval 'd' = Day
          getInterval 'w' = Week
          getInterval 'm' = Month
          getInterval 'y' = Year

modeP :: Parser TimeMode
modeP = getMode <$> oneOf "DTS"
    where getMode 'D' = Deadline
          getMode 'T' = Time
          getMOde 'S' = Scheduled

-- add tags to an AgendaElement
addTags :: [Tag] -> AgendaElement -> AgendaElement
addTags ts a = a {tags = ts ++ ts'}
    where ts' = tags a
