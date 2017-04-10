{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Control.Monad ((>=>))

import Data.Aeson
import Data.List (intersect)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, maybeToList)
import Data.Morgue.Agenda.Time
import Data.Morgue.Agenda.Types
import Data.Semigroup

import GHC.Generics

-- | the parameters passed to a timed agenda
data TimedParams = TimedParams Day Integer (Maybe TreeParams)

-- | the result of a timed agenda
newtype TimedResult = TimedResult (M.Map Day [AgendaFile])
    deriving (Eq, Show)

instance Semigroup TimedResult where
    (TimedResult m1) <> (TimedResult m2) = TimedResult $ M.unionWith (<>) m1 m2

instance Monoid TimedResult where
    mappend = (<>)
    mempty = TimedResult mempty

instance ToJSON TimedResult where
    toJSON (TimedResult days) = object
        [ "days" .= M.foldrWithKey go [] days
        , "week" .= toWeekInfo ((,) <$> safeGet M.findMin <*> safeGet M.findMax)
        ]
        where go day tree res = pairToJSON day tree : res
              pairToJSON day tree = object
                  [ "day" .= formatDay day
                  , "trees" .= tree
                  ]
              safeGet fun
                  | null days = Nothing
                  | otherwise = Just . fst $ fun days

-- | compute a timed agenda
timedResult :: TimedParams -> AgendaFile -> TimedResult
timedResult (TimedParams day n treeParams) file =
    TimedResult . M.map (maybeToList . computeTrees treeParams) .  M.fromDistinctAscList $
        zip days days
    where days = consecutiveDays day n
          timeFilter = filterAgendaTree . agendaTreeFilterTimed False
          computeTrees (Just tP) d = liftFile (treeAgenda tP >=> timeFilter d) file
          computeTrees Nothing d = liftFile (timeFilter d) file

-- | a filter to be used to filter for subtrees denoting elements relevant on a given day
agendaTreeFilterTimed :: Bool -> Day -> AgendaElement -> AgendaTreeFilter
agendaTreeFilterTimed showOverdue day element
    | isRelevant day element || showOverdue = KeepTree
    | otherwise = DropTreeAndWalk

-- | the parameters passed to a todo agenda
data TodoParams = TodoParams Bool (Maybe TreeParams)

-- | the result of a todo agenda
newtype TodoResult = TodoResult [AgendaFile]
    deriving (Eq, Semigroup, Show, Monoid)

instance ToJSON TodoResult where
    toJSON (TodoResult tree) = object [ "trees" .= toJSON tree ]

-- | compute a todo agenda
todoResult :: TodoParams -> AgendaFile -> TodoResult
todoResult (TodoParams showDone (Just tP)) file = TodoResult . maybeToList $
    liftFile (filterAgendaTree (agendaTreeFilterTodo showDone) >=> treeAgenda tP) file
todoResult (TodoParams showDone Nothing) tree = TodoResult . maybeToList $
    liftFile (filterAgendaTree (agendaTreeFilterTodo showDone)) tree

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
    } deriving (Eq, Generic, Show)

instance Semigroup BothResult where
    (BothResult timed1 todo1) <> (BothResult timed2 todo2) =
        BothResult (timed1 <> timed2) (todo1 <> todo2)

instance Monoid BothResult where
    mappend = (<>)
    mempty = BothResult mempty mempty

instance ToJSON BothResult

-- | compute a both agenda
bothResult :: BothParams -> AgendaFile -> BothResult
bothResult (BothParams day n sD tP) =
    BothResult <$> timedResult (TimedParams day n tP) <*> todoResult (TodoParams sD tP)

-- | the parameters passed to a tree agenda
data TreeParams = TreeParams [Tag] Bool

-- | the result of a tree agenda
newtype TreeResult = TreeResult [AgendaFile]
    deriving (Eq, Semigroup, Show, Monoid)

instance ToJSON TreeResult where
    toJSON (TreeResult tree) = object [ "trees" .= toJSON tree ]

-- | compute a tree agenda
treeResult :: TreeParams -> AgendaFile -> TreeResult
treeResult tP = TreeResult . maybeToList . liftFile (treeAgenda tP)

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
