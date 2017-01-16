{-# LANGUAGE OverloadedStrings #-}
module ToJSONSpec where

import Data.Morgue.Agenda.Render
import Data.Morgue.Agenda.Types

import Test.Hspec

spec :: Spec
spec = describe "Data.Morgue.Agenda/ToJSON"
    agendaTreeSpec

agendaTreeSpec :: Spec
agendaTreeSpec = describe "AgendaTree" $
    it "renders to proper JSON" $
        renderJSON (AgendaTree (Elem "root" Nothing Nothing []) []) `shouldBe`
            "{\"children\":[]\
            \,\"indent\":0\
            \,\"element\":\
                \{\"time\":null,\"toDo\":null,\"description\":\"root\",\"tags\":[]}\
            \}"
