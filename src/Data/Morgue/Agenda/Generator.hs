{-# LANGUAGE OverloadedStrings #-}
module Data.Morgue.Agenda.Generator where

import Data.List (intersect)
-- import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.Morgue.Agenda.Time
import Data.Morgue.Agenda.Types

-- | filter an agenda tree by dropping nodes not matched by the passed function
filterAgendaTree :: (AgendaElement -> AgendaTreeFilter) -> AgendaTree -> Maybe AgendaTree
filterAgendaTree func tree@(AgendaTree e subTrees) =
    case func e of
      KeepTree -> Just tree
      DropTree -> Nothing
      KeepTreeAndWalk -> Just $ AgendaTree e (mapMaybe (filterAgendaTree func) subTrees)
      DropTreeAndWalk -> case mapMaybe (filterAgendaTree func) subTrees of
                           [] -> Nothing
                           children -> Just (AgendaTree e children)

-- | a filter to be used to filter for subtrees tagged with certain tags
agendaTreeFilterTagged :: [Tag] -> AgendaElement -> AgendaTreeFilter
agendaTreeFilterTagged allowed (Elem _ _ _ ts)
    | not. null $ allowed `intersect` ts = KeepTree
    | otherwise = DropTreeAndWalk

-- | a filter to be used to filter for subtrees not tagged with certain tags
agendaTreeFilterNotTagged :: [Tag] -> AgendaElement -> AgendaTreeFilter
agendaTreeFilterNotTagged forbidden (Elem _ _ _ ts)
    | not . null $ forbidden `intersect` ts = DropTree
    | otherwise = KeepTreeAndWalk

-- | a filter to be used to filter for subtrees denoting todo elements
agendaTreeFilterTodo :: Bool -> AgendaElement -> AgendaTreeFilter
agendaTreeFilterTodo _ (Elem _ Nothing _ _) = DropTreeAndWalk
agendaTreeFilterTodo showDone (Elem _ (Just todo) _ _)
    | todo || showDone = KeepTree
    | otherwise = DropTreeAndWalk

-- | a filter to be used to filter for subtrees denoting elements relevant on a given day
agendaTreeFilterTimed :: Bool -> Day -> AgendaElement -> AgendaTreeFilter
agendaTreeFilterTimed showOverdue day element
    | isRelevant day element || showOverdue = KeepTree
    | otherwise = DropTreeAndWalk
