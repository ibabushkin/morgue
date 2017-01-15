{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Morgue.Agenda.Render where

import Control.Exception (try)

import Data.Aeson (ToJSON(..), encode)
import Data.Maybe (fromMaybe)
import Data.Morgue.Agenda.Types
import Data.Text (Text, stripSuffix)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

import System.IO.Error (tryIOError)

import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH

-- | the template to render a timed agenda
timedTemplate :: Template
timedTemplate = cleanTemplate $(TH.compileMustacheDir "timed" "templates/")

-- | the template to render a todo agenda
todoTemplate :: Template
todoTemplate = timedTemplate { templateActual = "todo" }

-- | the template to render a timed and todo agenda
bothTemplate :: Template
bothTemplate = timedTemplate { templateActual = "todo" }

-- | the template to render a tree agenda
treeTemplate :: Template
treeTemplate = timedTemplate { templateActual = "tree" }

-- | "clean" a template - that is, remove trailing newlines that are around because we
-- read from a file created by a human
cleanTemplate :: Template -> Template
cleanTemplate (Template a c) = Template a (clean <$> c)
    where clean = foldr go []
          go (TextBlock t) [] = [TextBlock $ fromMaybe t (stripSuffix "\n" t)]
          go n ns = n:ns

-- | the format to be used when outputting a filtered tree
data OutputFormat a
    = Plain -- ^ plain text. boring, but reliable (and machine-readable)
    | Pango -- ^ pango markup. useful for awesomewm or dunst notifications
    | ANSI  -- ^ colored plain text. not as boring
    | Custom a -- ^ custom mustache template passed, represented by some type
    deriving (Show, Eq, Foldable, Functor, Traversable)

-- | compile a template from a path, catching exceptions.
compileTemplate :: FilePath
                -> IO (Either IOError (Either MustacheException Template))
compileTemplate = tryIOError . try . compileMustacheDir "main"

-- | get a template according to output format and agenda mode
-- TODO: set the custom template entry point to use here!
dispatchTemplate :: OutputFormat Template -> AgendaMode -> Template
dispatchTemplate (Custom template) _ = template
dispatchTemplate _ (Timed _ both)
    | both = bothTemplate
    | otherwise = todoTemplate
dispatchTemplate _ Todo = todoTemplate
dispatchTemplate _ Tree = treeTemplate

render :: ToJSON a => Template -> a -> Text
render template = toStrict . renderMustache template . toJSON

renderJSON :: ToJSON a => a -> Text
renderJSON = toStrict . decodeUtf8 . encode . toJSON
