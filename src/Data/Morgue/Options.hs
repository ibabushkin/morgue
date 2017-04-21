{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Morgue.Options (run) where

import Control.Exception (displayException, Exception(..))
import Control.Monad (when)

import Data.Aeson (ToJSON(..))
import Data.Either (partitionEithers)
import Data.Foldable (foldl')
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Morgue.Agenda
import Data.Morgue.Agenda.Generator
import Data.Morgue.Agenda.Render
import Data.Morgue.Agenda.Time (getCurrentDay, Day)
import Data.Morgue.Agenda.Types

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr)
import System.IO.Error (tryIOError)

import Text.Mustache (Template)
import Text.Read (readMaybe)

-- | the options we parametrize our behaviour over
data Options
    = RunWith
        { optOutput :: Maybe FilePath -- ^ how to output the results
        , optFormat :: OutputFormat FilePath -- ^ the output format to use
        , optMode :: AgendaMode -- ^ the agenda mode to use
        , optDay :: Day -- ^ the starting day of the agenda
        , optTags :: Maybe ([Tag], Bool) -- ^ tags passed to filter the agenda tree
        , optTimeDisplay  :: Bool -- ^ the time settings outside of a timed agenda
        , optFiles :: [FilePath] -- ^ the files to use as input
        }
    | Help
    | Version
    deriving (Show, Eq)

-- | get the default options (requires current day)
--
-- the defaults used are:
-- * output to stdout
-- * colored plaintext format
-- * a timed agenda for the week
-- * a todo tree
-- * no tag filter
defaultOptions :: IO Options
defaultOptions = constructOptions <$> getCurrentDay
    where constructOptions day = RunWith Nothing Colored (Timed 7 True) day Nothing False []

-- | get a string representation of the currently running version
version :: String
version = "1.1.0.0"

-- | build a help message
helpMessage :: IO Text
helpMessage = pack . flip usageInfo options . formatHeader <$> getProgName
    where formatHeader prg = prg <> " " <> version <> "\nUSAGE: " <> prg
            <> " [OPTION..] file(s)\nOPTIONS:"

-- | build a version message
versionMessage :: IO Text
versionMessage = formatHeader <$> getProgName
    where formatHeader prg = pack prg <> " version " <> pack version

-- | options to be registered with GetOpt
options :: [OptDescr (Options -> Options)]
options =
    [ Option "h" ["help"] (NoArg (const Help))
        "Show this help."
    , Option "v" ["version"] (NoArg (const Version))
        "Show the morgue version you're using."
    , Option "m" ["mode"] (ReqArg setMode "MODE")
        "Desired agenda mode. Possible values:\n\
        \  'timed':\tAn agenda, sorted by days.\n\
        \  'todo':\tA set of todo entries.\n\
        \  'both':\tBoth of the above.\n\
        \  'tree':\tThe complete document.\n\
        \Defaults to 'both'.\n"
    , Option "n" ["num-days"] (ReqArg setNumDays "NUM")
        "Number of days to include in the agenda, including\n\
        \today. Only makes sense if you use 'timed' or 'both'\n\
        \mode, ignored otherwise.\n\
        \Defaults to 7."
    , Option "t" ["tags"] (ReqArg setTags "TAGS")
        "Tags to filter elements on. This affects every mode."
    , Option "i" ["ignore"] (NoArg setIgnore)
        "Invert the used tag filter, that is, make positive\n\
        \filters negative and vice-versa.\n\
        \Defaults to a positive filter."
    , Option "d" ["display-time"] (NoArg setTimeDisplay)
        "Toggle whether timestamps should be rendered outside\n\
        \of a 'timed' agenda or the 'timed' part of a 'both'\n\
        \agenda. Defaults to false."
    , Option "o" ["output"] (ReqArg setOutput "FILE")
        "File to redirect output to, using stdout if not set."
    , Option "f" ["format"] (ReqArg setFormat "FORMAT")
        "Output format to use. Possible values:\n\
        \  'plain':\tPlaintext formatting.\n\
        \  'colored':\tPlaintext formatting, with ANSI colors.\n\
        \  'pango':\tPango markup formatting.\n\
        \Defaults to 'colored'. This overrides any custom\n\
        \templates specified using -F."
    , Option "F" ["custom-format"] (ReqArg setCustomFormat "DIR")
        "A directory with mustache templates to be used to\n\
        \render the agenda. This overrides any format\n\
        \specified using -f."
    ]

-- | set the mode on a set of options
setMode :: String -> Options -> Options
setMode "timed" opts@RunWith{..} = opts { optMode = updateMode optMode }
    where updateMode (Timed n _) = Timed n False
          updateMode _ = Timed 7 False
setMode "todo" opts@RunWith{} = opts { optMode = Todo }
setMode "both" opts@RunWith{..} = opts { optMode = updateMode optMode }
    where updateMode (Timed n _) = Timed n True
          updateMode _ = Timed 7 True
setMode "tree" opts@RunWith{} = opts { optMode = Tree }
setMode _ opts = opts

-- | set the number of days on a set of options
setNumDays :: String -> Options -> Options
setNumDays num opts@RunWith{ optMode = Timed n b } =
    opts { optMode = Timed (fromMaybe n (readMaybe num)) b }
setNumDays _ opts = opts

-- | set the tag filter on a set of options
setTags :: String -> Options -> Options
setTags tags opts@RunWith{..} = opts { optTags = Just (newTags, bool) }
    where newTags = map Tag . filter (not . T.null) . T.split (== ':') $ pack tags
          bool = fromMaybe False (snd <$> optTags)
setTags _ opts = opts

-- | toggle the tag filter status on a set of options
setIgnore :: Options -> Options
setIgnore opts@RunWith{..} = opts { optTags = Just (tags, bool) }
    where tags = fromMaybe [] (fst <$> optTags)
          bool = maybe True not (snd <$> optTags)
setIgnore opts = opts

setTimeDisplay :: Options -> Options
setTimeDisplay opts@RunWith{..} = opts { optTimeDisplay = not optTimeDisplay }
setTimeDisplay opts = opts

-- | set the output file on a set of options
setOutput :: FilePath -> Options -> Options
setOutput path opts@RunWith{ optOutput = Nothing } = opts { optOutput = Just path }
setOutput _ opts = opts

-- | set the output format on a set of options
setFormat :: String -> Options -> Options
setFormat "plain" opts@RunWith{} = opts { optFormat = Plain }
setFormat "colored" opts@RunWith{} = opts { optFormat = Colored }
setFormat "pango" opts@RunWith{} = opts { optFormat = Pango }
setFormat _ opts = opts

-- | set the output format to a custom template path on a set of options
setCustomFormat :: FilePath -> Options -> Options
setCustomFormat path opts@RunWith{} = opts { optFormat = Custom path }
setCustomFormat _ opts = opts

-- | get the file contents to work with, including a list of I/O errors
-- that occured and the data, if any of the sources returned some
getFileContents :: [FilePath] -> IO ([IOError], [AgendaFile])
getFileContents files = partitionEithers <$> mapM get files
    where get fName = fmap (AgendaFile (pack fName) . getAgendaTree) <$> getText fName
          getText "-" = tryIOError TIO.getContents
          getText fName = tryIOError (TIO.readFile fName)

-- | handle errors from two layers of exceptions caught with `try` and treat
-- them as fatal.
handleNestedErrors :: (Exception e1, Exception e2)
                   => Either e1 (Either e2 b)
                   -> IO b
handleNestedErrors (Left e) =
    TIO.hPutStrLn stderr (pack $ displayException e) >> exitFailure
handleNestedErrors (Right (Left e)) =
    TIO.hPutStrLn stderr (pack $ displayException e) >> exitFailure
handleNestedErrors (Right (Right r)) = return r

-- | run, parsing options
run :: IO ()
run = do
    -- handle args
    args <- getArgs
    let (actions, files, _) = getOpt RequireOrder options args
    opts <- foldl' (flip ($)) <$> defaultOptions <*> pure actions
    runOpts opts files

-- | given some files and options, take the appropriate action
runOpts :: Options -> [FilePath] -> IO ()
runOpts Help _ = helpMessage >>= TIO.hPutStr stderr >> exitSuccess
runOpts Version _ = versionMessage >>= TIO.hPutStrLn stderr
runOpts opts@RunWith{..} files = do
    (errs, contents) <- getFileContents files
    mapM_ (TIO.hPutStrLn stderr . pack . displayException) errs
    when (not (null errs) && null contents) exitFailure
    format <- mapM compileTemplate optFormat >>= mapM handleNestedErrors
    output optOutput $ runWith opts (dispatchTemplate format optMode) contents

-- | handle options, computing actual output
runWith :: Options -> Template -> [AgendaFile] -> Text
runWith RunWith{..} template files
    | Timed num True <- optMode = toText $ map (bothRes num) files
    | Timed num False <- optMode = toText $ map (timedRes num) files
    | Todo <- optMode = toText $ map todoRes files
    | Tree <- optMode, Just tP <- treeParams = toText $ map (treeResult tP) files
    | otherwise = render template $ TreeResult files
    where treeParams = uncurry TreeParams <$> optTags <*> pure optTimeDisplay
          bothRes num = bothResult (BothParams optDay num True treeParams)
          timedRes num = timedResult (TimedParams optDay num treeParams)
          todoRes = todoResult (TodoParams True treeParams)
          toText :: (ToJSON a, Monoid a) => [a] -> Text
          toText = render template . mconcat
runWith _ _ _ = mempty

-- | dispatch the output to an appropriate place
output :: Maybe FilePath -> Text -> IO ()
output (Just file) = TIO.writeFile file
output Nothing = TIO.putStr
