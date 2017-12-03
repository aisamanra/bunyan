module Bunyan where

import qualified Data.Text.IO as T
import qualified System.Process as Sys

import qualified Bunyan.Log as Log
import qualified Bunyan.App as App
import qualified Bunyan.Pretty as Pretty

data Config = Config
  { cfgEditorCommand :: String
  , cfgGitRepo       :: FilePath
  } deriving (Eq, Show)


main :: Config -> IO ()
main cfg = do
  let pr = (Sys.proc "git" ["log"]) { Sys.cwd = Just (cfgGitRepo cfg)
                                    , Sys.std_out = Sys.CreatePipe
                                    }
  rs <- Sys.withCreateProcess pr $ \ _ (Just stdin) _ _ -> do
    T.hGetContents stdin
  let entries = Log.parseLogEntry rs
  cats <- App.runApp entries
  T.putStrLn (Pretty.pretty cats)
