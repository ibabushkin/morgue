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

instance Arbitrary AgendaElement where
    arbitrary = Elem <$>
        ((:[]) <$> elements ["abc", "def", "ghi", "jkl"]) <*>
            pure Nothing <*> pure Nothing <*> pure []

instance Arbitrary AgendaTree where
    arbitrary = AgendaTree <$> arbitrary <*> pure []

instance Arbitrary AgendaFile where
    arbitrary = AgendaFile <$> arbitrary <*> arbitrary

instance Arbitrary TimedResult where
    arbitrary = TimedResult <$> arbitrary

instance Arbitrary TreeResult where
    arbitrary = TreeResult <$> arbitrary

instance Arbitrary TodoResult where
    arbitrary = TodoResult <$> arbitrary

instance Arbitrary BothResult where
    arbitrary = BothResult <$> arbitrary <*> arbitrary

spec :: Spec
spec = describe "Data.Morgue.Agenda Monoid instances" $ do
    describe "Data.Morgue.Agenda.Generator" $ do
        treeResultSpec
        todoResultSpec
        timedResultSpec
        bothResultSpec

neutrality :: (Eq a, Monoid a) => a -> Bool
neutrality a = a <> mempty == a && mempty <> a == a

associativity :: (Eq a, Monoid a) => a -> a -> a -> Bool
associativity a b c = a <> (b <> c) == (a <> b) <> c

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

bothResultSpec :: Spec
bothResultSpec = describe "Data.Morgue.Agenda.Generator.BothResult" $ do
    it "has a proper neutral element" $ property $
        \r@BothResult{} -> neutrality r
    it "respects associativity" $ property $
        \r1@BothResult{} r2@BothResult{} r3@BothResult{} -> associativity r1 r2 r3
