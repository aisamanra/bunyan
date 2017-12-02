module Bunyan where

import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Process as Sys

import           Bunyan.Log
import           Bunyan.App
import           Bunyan.Pretty

data Config = Config
  { cfgEditorCommand :: String
  , cfgGitRepo       :: FilePath
  } deriving (Eq, Show)


main :: Config -> IO ()
main cfg = do
  let pr = (Sys.proc "git" ["log"]) { Sys.cwd = Just (cfgGitRepo cfg)
                                    , Sys.std_out = Sys.CreatePipe
                                    }
  rs <- Sys.withCreateProcess pr $ \ _ (Just stdin) _ ph -> do
    T.hGetContents stdin
  let entries = parseLogEntry rs
  cats <- runApp entries
  T.putStrLn (pretty cats)
