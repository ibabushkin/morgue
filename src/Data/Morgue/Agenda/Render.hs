{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Morgue.Agenda.Render
    ( OutputFormat(..)
    , someTemplate
    , compileTemplate
    , dispatchTemplate
    , render
    , renderJSON
    ) where

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

-- | the format to be used when outputting a filtered tree
data OutputFormat a
    = Plain -- ^ plain text. boring, but reliable (and machine-readable)
    | Colored  -- ^ colored plain text. not as boring
    | Pango -- ^ pango markup. useful for awesomewm or dunst notifications
    | Custom a -- ^ custom mustache template passed, represented by some type
    deriving (Show, Eq, Foldable, Functor, Traversable)

-- | the template to render a timed agenda
timedTemplate :: OutputFormat Template -> Template
timedTemplate Plain = cleanTemplate $(TH.compileMustacheDir "timed" "templates/plain")
timedTemplate Colored = cleanTemplate $(TH.compileMustacheDir "timed" "templates/colored")
timedTemplate Pango = cleanTemplate $(TH.compileMustacheDir "timed" "templates/pango")
timedTemplate (Custom template) = cleanTemplate template

-- | the template to render a todo agenda
todoTemplate :: OutputFormat Template -> Template
todoTemplate format = (timedTemplate format) { templateActual = "todo" }

-- | the template to render a timed and todo agenda
bothTemplate :: OutputFormat Template -> Template
bothTemplate format = (timedTemplate format) { templateActual = "both" }

-- | the template to render a tree agenda
treeTemplate :: OutputFormat Template -> Template
treeTemplate format = (timedTemplate format) { templateActual = "tree" }

-- | some template
someTemplate :: OutputFormat Template -> Template
someTemplate = timedTemplate

-- | "clean" a template - that is, remove trailing newlines that are around because we
-- read from a file created by a human
cleanTemplate :: Template -> Template
cleanTemplate (Template a c) = Template a (clean <$> c)
    where clean = foldr go []
          go (TextBlock t) [] = [TextBlock $ fromMaybe t (stripSuffix "\n" t)]
          go n ns = n:ns

-- | compile a template from a path, catching exceptions.
compileTemplate :: FilePath
                -> IO (Either IOError (Either MustacheException Template))
compileTemplate = tryIOError . try . compileMustacheDir "timed"

-- | get a template according to output format and agenda mode
dispatchTemplate :: OutputFormat Template -> AgendaMode -> Template
dispatchTemplate format (Timed _ both)
    | both = bothTemplate format
    | otherwise = timedTemplate format
dispatchTemplate format Todo = todoTemplate format
dispatchTemplate format Tree = treeTemplate format

-- | render a template to text
render :: ToJSON a => Template -> a -> Text
render template = toStrict . renderMustache template . toJSON

-- | render a template to JSON
renderJSON :: ToJSON a => a -> Text
renderJSON = toStrict . decodeUtf8 . encode . toJSON
