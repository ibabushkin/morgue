{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Morgue.Options ( run ) where

import Control.Exception (displayException, Exception(..))
import Control.Monad (when)

import Data.Foldable (foldl')
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Morgue.Agenda.Render
import Data.Morgue.Agenda.Time (getCurrentDay, Day)
import Data.Morgue.Agenda.Types

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr)
import System.IO.Error (tryIOError)

import Text.Read (readMaybe)

-- | the options we parametrize our behaviour over
data Options
    = RunWith
        { optOutput :: Maybe FilePath -- ^ how to output the results
        , optFormat :: OutputFormat FilePath -- ^ the output format to use
        , optMode :: AgendaMode -- ^ the agenda mode to use
        , optDay :: Day -- ^ the starting day of the agenda
        , optTags :: Maybe ([Tag], Bool) -- ^ tags passed to filter the agenda tree
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
    where constructOptions day = RunWith Nothing ANSI (Timed 7 True) day Nothing []

-- | get a string representation of the currently running version
version :: String
version = "1.0"

-- | build a help message
helpMessage :: IO Text
helpMessage = pack . flip usageInfo options . formatHeader <$> getProgName
    where formatHeader prg = prg <> " version " <> version <> "\nUSAGE: " <> prg
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
        "Desired agenda mode. Possible values are: 'timed', 'todo', 'both',\n\
        \and 'tree'. Defaults to 'both'.\n\
        \'timed': An agenda, sorted by days.\n\
        \'todo': A set of todo entries.\n\
        \'both': Both of the above.\n\
        \'tree': The complete document."
    , Option "n" ["num-days"] (ReqArg setNumDays "NUM")
        "Number of days to include in the agenda, including today.\n\
        \Only makes sense to use if you use 'timed' or 'both' mode,\n\
        \ignored otherwise. Defaults to 7."
    , Option "t" ["tags"] (ReqArg setTags "TAGS")
        "Tags to filter elements on. This affects every mode."
    , Option "i" ["ignore"] (NoArg setIgnore)
        "Invert the used tag filter, that is, make positive filters\n\
        \negative and vice-versa. Defaults to a positive filter."
    , Option "o" ["output"] (ReqArg setOutput "FILE")
        "File to redirect output to, using stdout if not set."
    , Option "f" ["format"] (ReqArg setFormat "FORMAT")
        "Output format to use. Possible values are 'plain', 'colored',\n\
        \and 'pango'. Defaults to 'colored'. This overrides any custom\n\
        \templates specified using -F."
    , Option "F" ["custom-format"] (ReqArg setCustomFormat "DIRECTORY")
        "A directory with mustache templates to be used to render the agenda.\n\
        \This overrides any format specified using -f."
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

-- | set the output file on a set of options
setOutput :: FilePath -> Options -> Options
setOutput path opts@RunWith{ optOutput = Nothing } = opts { optOutput = Just path }
setOutput _ opts = opts

-- | set the output format on a set of options
setFormat :: String -> Options -> Options
setFormat "plain" opts@RunWith{} = opts { optFormat = Plain }
setFormat "colored" opts@RunWith{} = opts { optFormat = ANSI }
setFormat "pango" opts@RunWith{} = opts { optFormat = Pango }
setFormat _ opts = opts

-- | set the output format to a custom template path on a set of options
setCustomFormat :: FilePath -> Options -> Options
setCustomFormat path opts@RunWith{} = opts { optFormat = Custom path }
setCustomFormat _ opts = opts

-- | get the file contents to work with, including a list of I/O errors
-- that occured and a flag indicating whether the operation can be considered
-- sucessful (that is, at least one of the given sources returned data).
getFileContents :: [FilePath] -> IO ([IOError], Text, Bool)
getFileContents files = foldr go (mempty, mempty, False) <$> mapM get files
    where get "-" = tryIOError TIO.getContents
          get fName = tryIOError (TIO.readFile fName)
          go (Left e) (es, res, s) = (e:es, res, s)
          go (Right r) (es, res, _) = (es, r <> res, True)

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
    runOpts files opts

-- | given some files and options, take the appropriate action
runOpts :: [FilePath] -> Options -> IO ()
runOpts _ Help = helpMessage >>= TIO.hPutStr stderr >> exitSuccess
runOpts _ Version = versionMessage >>= TIO.hPutStrLn stderr
runOpts files opts@RunWith{..} = do
    (errs, contents, status) <- getFileContents files
    mapM_ (TIO.hPutStrLn stderr . pack . displayException) errs
    when (not (null errs) && not status) exitFailure
    format <- mapM compileTemplate optFormat >>= mapM handleNestedErrors
    let template = dispatchTemplate format optMode
    return ()
