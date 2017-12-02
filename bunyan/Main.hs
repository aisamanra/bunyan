module Main where

import qualified Bunyan
import qualified Options

main :: IO ()
main = do
  bunyan <- Options.getOpts
  Bunyan.main bunyan
