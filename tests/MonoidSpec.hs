{-# LANGUAGE OverloadedStrings #-}
module MonoidSpec where

import Data.Monoid ((<>))
import Data.Morgue.Agenda.Generator
import Data.Morgue.Agenda.Types

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

instance Arbitrary Tag where
    arbitrary = Tag <$> arbitrary

instance Arbitrary WeekInfo where
    arbitrary = oneof
        [ OneWeek <$> arbitrary
        , MultipleWeeks <$> elements [0..52] <*> elements [0..52]
        , pure NoWeeks
        ]

instance Arbitrary AgendaElement where
    arbitrary = Elem <$>
        ((:[]) <$> elements ["abc", "def", "ghi", "jkl"]) <*>
            pure Nothing <*> pure Nothing <*> pure []

instance Arbitrary AgendaTree where
    arbitrary = AgendaTree <$> arbitrary <*> pure []

instance Arbitrary AgendaFile where
    arbitrary = AgendaFile <$> arbitrary <*> arbitrary

instance Arbitrary TimedParams where
    arbitrary = TimedParams <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TimedResult where
    arbitrary = TimedResult <$> arbitrary <*> arbitrary

instance Arbitrary TreeParams where
    arbitrary = TreeParams <$> arbitrary <*> arbitrary

instance Arbitrary TreeResult where
    arbitrary = TreeResult <$> arbitrary

instance Arbitrary TodoParams where
    arbitrary = TodoParams <$> arbitrary <*> arbitrary

instance Arbitrary TodoResult where
    arbitrary = TodoResult <$> arbitrary

spec :: Spec
spec = describe "Data.Morgue.Agenda Monoid instances" $ do
    describe "Data.Morgue.Agenda.Types" weekInfoSpec
    describe "Data.Morgue.Agenda.Generator" $ do
        treeResultSpec
        todoResultSpec
        timedResultSpec
        -- bothResultSpec

neutrality :: (Eq a, Monoid a) => a -> Bool
neutrality a = a <> mempty == a && mempty <> a == a

weekNeutrality :: WeekInfo -> Bool
weekNeutrality = neutrality

associativity :: (Eq a, Monoid a) => a -> a -> a -> Bool
associativity a b c = a <> (b <> c) == (a <> b) <> c

weekAssociativity :: WeekInfo -> WeekInfo -> WeekInfo -> Bool
weekAssociativity = associativity -- TODO: find a more elegant of doing this

weekInfoSpec :: Spec
weekInfoSpec = describe "Data.Morgue.Agenda.Types.WeekInfo" $ do
    it "has a proper neutral element" $ property weekNeutrality
    it "respects associativity" $ property weekAssociativity

treeResultSpec :: Spec
treeResultSpec = describe "Data.Morgue.Agenda.Generator.TreeResult" $ do
    it "has a proper neutral element" $ property $
        \r@TreeResult{} -> neutrality r
    it "respects associativity" $ property $
        \r1@TreeResult{} r2@TreeResult{} r3@TreeResult{} -> associativity r1 r2 r3

todoResultSpec :: Spec
todoResultSpec = describe "Data.Morgue.Agenda.Generator.TodoResult" $ do
    it "has a proper neutral element" $ property $
        \r@TodoResult{} -> neutrality r
    it "respects associativity" $ property $
        \r1@TodoResult{} r2@TodoResult{} r3@TodoResult{} -> associativity r1 r2 r3

timedResultSpec :: Spec
timedResultSpec = describe "Data.Morgue.Agenda.Generator.TimedResult" $ do
    it "has a proper neutral element" $ property $
        \r@TimedResult{} -> neutrality r
    it "respects associativity" $ property $
        \r1@TimedResult{} r2@TimedResult{} r3@TimedResult{} -> associativity r1 r2 r3
