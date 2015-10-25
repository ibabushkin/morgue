module Util where

import Data.List (intersect)
import Data.Maybe (fromMaybe)

import Text.Read (readMaybe)

import AgendaGenerator (Tag, AgendaElement(..))

readWrapper :: (Read a) => String -> a
readWrapper s = fromMaybe
    (error "You passed a wrong value, check your args!") (readMaybe s)

doubleSpaces :: Bool -> String -> String
doubleSpaces True = unlines . map doubleSpaces' . lines
    where doubleSpaces' = flip (++) <*> takeWhile (==' ')
doubleSpaces False = id

tagFilter :: Maybe [Tag] -> Maybe [Tag] -> [AgendaElement] -> [AgendaElement]
tagFilter (Just ts) _ = filter (\(Elem _ _ _ ts') -> ts `intersect` ts' /= [])
tagFilter _ (Just ts) = filter (\(Elem _ _ _ ts') -> ts `intersect` ts' == [])
