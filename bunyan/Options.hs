module Options (getOpts) where

import           Control.Monad (when)
import qualified System.Console.GetOpt as Opt
import qualified System.Directory as Sys
import qualified System.Environment as Sys
import qualified System.Exit as Sys

import qualified Bunyan

data Options = Options
  { optShowVersion :: Bool
  , optShowHelp    :: Bool
  , optEditorCmd   :: Maybe String
  , optRepoPath    :: FilePath
  } deriving (Eq, Show)

options :: [Opt.OptDescr (Options -> Options)]
options =
  [ Opt.Option ['v'] ["version"]
    (Opt.NoArg (\ o -> o { optShowVersion = True }))
    "Show version number"
  , Opt.Option ['h'] ["help"]
    (Opt.NoArg (\ o -> o { optShowHelp = True }))
    "Show this help screen"
  , Opt.Option ['e'] ["editor"]
    (Opt.ReqArg (\ e o -> o { optEditorCmd = Just e }) "CMD")
    "desired editor command (defaults to $EDITOR)"
  , Opt.Option ['r'] ["repository"]
    (Opt.ReqArg (\ p o -> o { optRepoPath = p }) "PATH")
    "git repository location (defaults to $CWD)"
  ]

usageInfo :: String
usageInfo = Opt.usageInfo header options
  where header = "Usage: bunyan [OPTIONS]..."

getOpts :: IO Bunyan.Config
getOpts = do
  args <- Sys.getArgs
  defaultEditor <- Sys.lookupEnv "EDITOR"
  defaultPath   <- Sys.getCurrentDirectory
  let defOpts = Options
        { optShowVersion = False
        , optShowHelp    = False
        , optEditorCmd   = defaultEditor
        , optRepoPath    = defaultPath
        }
  case Opt.getOpt Opt.Permute options args of
    (o, [], [])  -> do
      let opts = foldl (flip id) defOpts o
      when (optShowVersion opts) $ do
        putStrLn "bunyan, version 0.1.0.0"
        Sys.exitSuccess
      when (optShowHelp opts) $ do
        putStrLn usageInfo
        Sys.exitSuccess
      editor <- case optEditorCmd opts of
        Just e -> return e
        Nothing ->
          Sys.die "No $EDITOR set and no editor command supplied!"
      return $ Bunyan.Config
        { Bunyan.cfgEditorCommand = editor
        , Bunyan.cfgGitRepo       = optRepoPath opts
        }
    (_, _, errs) -> do
      Sys.die (concat errs ++ usageInfo)
