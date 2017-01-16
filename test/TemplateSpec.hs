{-# LANGUAGE OverloadedStrings #-}
module TemplateSpec where

import Data.Morgue.Agenda.Render
import Data.Morgue.Agenda.Types

import Test.Hspec

import Text.Mustache

spec :: Spec
spec = describe "Data.Morgue.Agenda.Render" $ do
    elementTemplateSpec
    treeTemplateSpec

elementTemplate :: Template
elementTemplate = timedTemplate { templateActual = "element" }

elementTemplateSpec :: Spec
elementTemplateSpec = describe "elementTemplate" $ do
    it "displays simple values correctly" $
        render elementTemplate (Elem "test" (Just True) Nothing [])
            `shouldBe` "todo indicator here \ttest"
    it "copes with random input" True -- TODO: quickcheck

treeTemplateSpec :: Spec
treeTemplateSpec = describe "treeTemplate" $ do
    it "displays simple values correctly" $
        render treeTemplate (AgendaTree (Elem "root" Nothing Nothing []) [])
            `shouldBe` "root\n"
    it "handles indentation correctly" True -- TODO

{- The template is unusable for obvious reasons
Template
    { templateActual = PName {unPName = "tree"}
    , templateCache = fromList
        [ (PName {unPName = "both"},[])
        , ( PName {unPName = "element"}
          , [ Section (Key {unKey = ["toDo"]}) [TextBlock "todo indicator here \t"]
            , Section (Key {unKey = ["time"]}) [TextBlock "time format here \t"]
            , UnescapedVar (Key {unKey = ["description"]})
            , Section (Key {unKey = ["tags"]}) [TextBlock " \ttags here"]
            , TextBlock ""
            ]
          )
        , ( PName {unPName = "main"}
          , [Partial (PName {unPName = "tree"}) (Just (Pos 1))]
          )
        , ( PName {unPName = "timed"}
          , [ TextBlock "Week agenda ("
            , UnescapedVar (Key {unKey = ["weeks"]})
            , TextBlock "):\n"
            , Section (Key {unKey = ["days"]})
                [ Partial (PName {unPName = "date"}) Nothing
                , TextBlock ":\n"
                , Partial (PName {unPName = "tree"}) (Just (Pos 1))
                , InvertedSection (Key {unKey = ["tree"]}) [TextBlock "\n"]
                ]
            ]
          )
        , ( PName {unPName = "todo"}
          , [ TextBlock "Global list of TODO entries:\n"
            , Partial (PName {unPName = "tree"}) (Just (Pos 1))
            ]
          )
        , ( PName {unPName = "tree"}
          , [ Section (Key {unKey = ["element"]})
                [Partial (PName {unPName = "element"}) Nothing]
            , TextBlock "\n",Section (Key {unKey = ["children"]})
                [ Section (Key {unKey = ["element"]})
                    [ UnescapedVar (Key {unKey = ["indent"]})
                    , TextBlock ": "
                    , Partial (PName {unPName = "element"}) Nothing
                    ]
                ]
            , TextBlock "\n"
            ]
          )
        ]
    }
    -}
