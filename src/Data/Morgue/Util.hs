-- | universal utilities
module Data.Morgue.Util where

import Data.List (intersect)
import Data.Maybe (fromMaybe)

import Text.Read (readMaybe)
import Text.Pandoc

import Data.Morgue.AgendaGenerator (Tag, AgendaElement(..))

-- | reading ADTs for Options
readWrapper :: (Read a) => String -> a
readWrapper s = fromMaybe
    (error "You passed a wrong value, check your args!") (readMaybe s)

{- | Double the amount of leading spaces in a String if necessary.
Pandoc assumes 4-space indentation for nested lists, whereas some people
(like myself) use 2.
-}
doubleSpaces :: Bool -> String -> String
doubleSpaces True = unlines . map doubleSpaces' . lines
    where doubleSpaces' = flip (++) <*> takeWhile (==' ')
doubleSpaces False = id

-- | create a tag filter
tagFilter :: Maybe [Tag] -> Maybe [Tag] -> [AgendaElement] -> [AgendaElement]
tagFilter (Just ts) _ = filter (\(Elem _ _ _ ts') -> ts `intersect` ts' /= [])
tagFilter _ (Just ts) = filter (\(Elem _ _ _ ts') -> ts `intersect` ts' == [])
tagFilter _ _ = id
