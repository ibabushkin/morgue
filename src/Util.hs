module Util where

import Data.Maybe (fromMaybe)

import Text.Read (readMaybe)

readWrapper :: (Read a) => String -> a
readWrapper s = fromMaybe
    (error "You passed a wrong value, check your args!") (readMaybe s)
