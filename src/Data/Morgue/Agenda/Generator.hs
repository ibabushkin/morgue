{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Morgue.Agenda.Generator 
    ( TimedParams(..)
    , TimedResult(..)
    , timedResult
    , TodoParams(..)
    , TodoResult(..)
    , todoResult
    , BothParams(..)
    , BothResult(..)
    , bothResult
    , TreeParams(..)
    , TreeResult(..)
    , treeResult
    ) where

import Data.Aeson
import Data.List (intersect)
import Data.Maybe (mapMaybe)
import Data.Morgue.Agenda.Time
import Data.Morgue.Agenda.Types

import GHC.Generics

-- | the parameters passed to a timed agenda
data TimedParams = TimedParams Day Integer (Maybe TreeParams)

-- | the result of a timed agenda
newtype TimedResult = TimedResult [(Day, Maybe AgendaTree)]

instance ToJSON TimedResult where
    toJSON (TimedResult days) = object [ "days" .= map pairToJSON days ]
        where pairToJSON (day, tree) = object
                  [ "day" .= toJSON day
                  , "tree" .= toJSON tree
                  ]

-- | compute a timed agenda
timedResult :: TimedParams -> AgendaTree -> TimedResult
timedResult (TimedParams day n tP) t =
    TimedResult $ map ((,) <$> id <*> computeTree t tP) (consecutiveDays day n)
    where computeTree tree (Just treeParams) d = treeAgenda treeParams tree >>=
              filterAgendaTree (agendaTreeFilterTimed False d)
          computeTree tree Nothing d =
              filterAgendaTree (agendaTreeFilterTimed False d) tree

-- | a filter to be used to filter for subtrees denoting elements relevant on a given day
agendaTreeFilterTimed :: Bool -> Day -> AgendaElement -> AgendaTreeFilter
agendaTreeFilterTimed showOverdue day element
    | isRelevant day element || showOverdue = KeepTree
    | otherwise = DropTreeAndWalk

-- | the parameters passed to a todo agenda
data TodoParams = TodoParams Bool (Maybe TreeParams)

-- | the result of a todo agenda
newtype TodoResult = TodoResult (Maybe AgendaTree)

instance ToJSON TodoResult where
    toJSON (TodoResult tree) = object [ "tree" .= toJSON tree ]

-- | compute a todo agenda
todoResult :: TodoParams -> AgendaTree -> TodoResult
todoResult (TodoParams showDone (Just tP)) tree =
    TodoResult $ treeAgenda tP tree >>= filterAgendaTree (agendaTreeFilterTodo showDone)
todoResult (TodoParams showDone Nothing) tree =
    TodoResult $ filterAgendaTree (agendaTreeFilterTodo showDone) tree

-- | a filter for subtrees denoting todo elements
agendaTreeFilterTodo :: Bool -> AgendaElement -> AgendaTreeFilter
agendaTreeFilterTodo _ (Elem _ Nothing _ _) = DropTreeAndWalk
agendaTreeFilterTodo showDone (Elem _ (Just tD) _ _)
    | tD || showDone = KeepTree
    | otherwise = DropTreeAndWalk

-- | the parameters passed to a both agenda
data BothParams = BothParams Day Integer Bool (Maybe TreeParams)

-- | the result of a both agenda
data BothResult = BothResult
    { timed :: TimedResult
    , todo :: TodoResult
    } deriving Generic

instance ToJSON BothResult

-- | compute a both agenda
bothResult :: BothParams -> AgendaTree -> BothResult
bothResult (BothParams day n sD tP) =
    BothResult <$> timedResult (TimedParams day n tP) <*> todoResult (TodoParams sD tP)

-- | the parameters passed to a tree agenda
data TreeParams = TreeParams [Tag] Bool

-- | the result of a tree agenda
newtype TreeResult = TreeResult (Maybe AgendaTree)

instance ToJSON TreeResult where
    toJSON (TreeResult tree) = object [ "tree" .= toJSON tree ]

-- | compute a tree agenda
treeResult :: TreeParams -> AgendaTree -> TreeResult
treeResult tP = TreeResult . treeAgenda tP

-- | filter an AgendaTree by tags
treeAgenda :: TreeParams -> AgendaTree -> Maybe AgendaTree
treeAgenda (TreeParams ts invert) = filterAgendaTree (getFilter invert ts)
    where getFilter True = agendaTreeFilterNotTagged
          getFilter False = agendaTreeFilterTagged

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
