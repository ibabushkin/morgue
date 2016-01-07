module Data.Morgue.Options where

import Data.Morgue.AgendaGenerator
import Data.Morgue.Format

data SimpleOptions = SAgendaOptions
    { sMode :: AgendaMode
    , sDoubleSpaces :: Bool
    , sTags :: Maybe [Tag]
    , sSkipTags :: Maybe [Tag]
    , sNumDays :: Integer
    , sFormat :: OutputFormat
    } | SOutlineOptions { sFormat :: OutputFormat }

-- | convert between the datatypes
convertOptions :: SimpleOptions -> Options
convertOptions s@SAgendaOptions{} =
    AgendaOptions <$> sMode <*> sDoubleSpaces <*> sTags <*>
        sSkipTags <*> sNumDays <*> pure putStrLn <*> sFormat $ s
convertOptions (SOutlineOptions format) =
    OutlineOptions putStrLn format

data Options = AgendaOptions
    { optMode         :: !AgendaMode
    , optDoubleSpaces :: Bool
    , optTags         :: Maybe [Tag]
    , optSkipTags     :: Maybe [Tag]
    , optNumDays      :: !Integer
    , optOutput       :: String -> IO ()
    , optFormat       :: !OutputFormat
    } | OutlineOptions
    { optOutput :: String -> IO ()
    , optFormat :: !OutputFormat
    }

instance Show Options where
    show (OutlineOptions _ f) = "OutlineOptions " ++ show f
    show (AgendaOptions m d _ _ n _ f) = "AgendaOptions " ++
        show m ++ " " ++ show d ++ " " ++ show n ++ " " ++ show f

instance Eq Options where
    (==) (AgendaOptions m d t s n _ f) (AgendaOptions m' d' t' s' n' _ f') =
        m == m' && d == d' && t == t' && s == s' && n == n' && f == f'
