{-# LANGUAGE OverloadedStrings #-}

module Bunyan.Pretty (pretty) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Sequence as S
import qualified Data.Text as T

pretty :: M.Map T.Text (S.Seq (S.Seq T.Text)) -> T.Text
pretty messages = T.unlines $ concat
  [ section annot fields
  | (annot, fields) <- M.toList messages
  , not (S.null fields)
  ]

section :: T.Text -> S.Seq (S.Seq T.Text) -> [T.Text]
section annot fields =
  let bullet [] = []
      bullet (x:xs) = ("* " <> x) : map ("  " <>) xs
  in "" : (annot <> ":") : F.foldMap bullet (fmap F.toList fields)
