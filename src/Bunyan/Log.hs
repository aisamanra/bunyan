{-# LANGUAGE OverloadedStrings #-}

module Bunyan.Log where

import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Sequence as S


data Entry = Entry
  { logCommit      :: T.Text
  , logAuthor      :: T.Text
  , logDate        :: T.Text
  , logMessage     :: S.Seq T.Text
  } deriving (Eq, Show)


emptyLogEntry :: T.Text -> Entry
emptyLogEntry commit = Entry
  { logCommit      = commit
  , logAuthor      = ""
  , logDate        = ""
  , logMessage     = mempty
  }


parseLogEntry :: T.Text -> S.Seq Entry
parseLogEntry = getNextCommit . T.lines
  where getNextCommit [] = S.empty
        getNextCommit (x:xs)
          | Just cmt <- T.stripPrefix "commit " x =
            parseCommit (emptyLogEntry cmt) xs
          | otherwise = getNextCommit xs

        parseCommit entry [] = S.singleton entry
        parseCommit entry (x:xs)
          | Just cmt <- T.stripPrefix "commit " x =
              entry S.<| parseCommit (emptyLogEntry cmt) xs
          | Just author <- T.stripPrefix "Author:" x =
              parseCommit (entry { logAuthor = T.strip author }) xs
          | Just date <- T.stripPrefix "Date:" x =
              parseCommit (entry { logDate = T.strip date }) xs
          | Just line   <- T.stripPrefix "    " x =
              parseCommit (entry { logMessage = logMessage entry <> S.singleton line }) xs
          | otherwise =
            parseCommit entry xs
