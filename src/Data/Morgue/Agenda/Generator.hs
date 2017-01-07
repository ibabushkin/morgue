{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Morgue.Agenda.Generator where

import Data.Aeson
import Data.List (intersect)
import Data.Maybe (mapMaybe)
import Data.Morgue.Agenda.Time
import Data.Morgue.Agenda.Types

import GHC.Generics

newtype TimedAgendaResult = TimedAgendaResult [(Day, Maybe AgendaTree)]

instance ToJSON TimedAgendaResult where
    toJSON (TimedAgendaResult days) = object [ "days" .= map pairToJSON days ]
        where pairToJSON (day, tree) = object
                  [ "day" .= toJSON day
                  , "tree" .= toJSON tree
                  ]

timedAgendaResult :: Day -> Integer -> AgendaTree -> TimedAgendaResult
timedAgendaResult day n t =
    TimedAgendaResult $ map ((,) <$> id <*> computeTree t) (consecutiveDays day n)
    where computeTree tree d = filterAgendaTree (agendaTreeFilterTimed False d) tree

newtype TodoAgendaResult = TodoAgendaResult (Maybe AgendaTree)

instance ToJSON TodoAgendaResult where
    toJSON (TodoAgendaResult tree) = object [ "tree" .= toJSON tree ]

todoAgendaResult :: Bool -> AgendaTree -> TodoAgendaResult
todoAgendaResult showDone =
    TodoAgendaResult . filterAgendaTree (agendaTreeFilterTodo showDone)

data BothAgendaResult = BothAgendaResult
    { timed :: TimedAgendaResult
    , todo :: TodoAgendaResult
    } deriving Generic

instance ToJSON BothAgendaResult

bothAgendaResult :: Day -> Integer -> Bool -> AgendaTree -> BothAgendaResult
bothAgendaResult day n showDone =
    BothAgendaResult <$> timedAgendaResult day n <*> todoAgendaResult showDone

newtype TreeAgendaResult = TreeAgendaResult (Maybe AgendaTree)

instance ToJSON TreeAgendaResult where
    toJSON (TreeAgendaResult tree) = object [ "tree" .= toJSON tree ]

treeAgendaResult :: [Tag] -> Bool -> AgendaTree -> TreeAgendaResult
treeAgendaResult tags invert = TreeAgendaResult . filterAgendaTree (getFilter invert tags)
    where getFilter True = agendaTreeFilterNotTagged
          getFilter False = agendaTreeFilterTagged

-- | filter an agenda tree by dropping nodes not matched by the passed function
filterAgendaTree :: (AgendaElement -> AgendaTreeFilter) -> AgendaTree -> Maybe AgendaTree
filterAgendaTree func tree@(AgendaTree e subTrees) =
    case func e of
      KeepTree -> Just tree
      DropTree -> Nothing
      KeepTreeAndWalk ->
          Just $ AgendaTree e (mapMaybe (filterAgendaTree func) subTrees)
      DropTreeAndWalk ->
          case mapMaybe (filterAgendaTree func) subTrees of
            [] -> Nothing
            children -> Just (AgendaTree e children)

-- | a filter for subtrees tagged with certain tags
agendaTreeFilterTagged :: [Tag] -> AgendaElement -> AgendaTreeFilter
agendaTreeFilterTagged allowed (Elem _ _ _ ts)
    | not. null $ allowed `intersect` ts = KeepTree
    | otherwise = DropTreeAndWalk

-- | a filter for subtrees not tagged with certain tags
agendaTreeFilterNotTagged :: [Tag] -> AgendaElement -> AgendaTreeFilter
agendaTreeFilterNotTagged forbidden (Elem _ _ _ ts)
    | not . null $ forbidden `intersect` ts = DropTree
    | otherwise = KeepTreeAndWalk

-- | a filter for subtrees denoting todo elements
agendaTreeFilterTodo :: Bool -> AgendaElement -> AgendaTreeFilter
agendaTreeFilterTodo _ (Elem _ Nothing _ _) = DropTreeAndWalk
agendaTreeFilterTodo showDone (Elem _ (Just tD) _ _)
    | tD || showDone = KeepTree
    | otherwise = DropTreeAndWalk

-- | a filter to be used to filter for subtrees denoting elements relevant on a given day
agendaTreeFilterTimed :: Bool -> Day -> AgendaElement -> AgendaTreeFilter
agendaTreeFilterTimed showOverdue day element
    | isRelevant day element || showOverdue = KeepTree
    | otherwise = DropTreeAndWalk
