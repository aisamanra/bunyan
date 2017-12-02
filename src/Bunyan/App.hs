{-# LANGUAGE OverloadedStrings #-}

module Bunyan.App (runApp) where

import qualified Brick
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Input.Events as Vty

import qualified Bunyan.Log as Log


data Modal
  = AddSectionModal ()
  | ConfirmQuitModal
  deriving (Eq, Show)

data Annot
  = Annot T.Text Log.Entry
  | Skip Log.Entry
  deriving (Eq, Show)

data State = State
  { stateSections :: M.Map T.Text (S.Seq (S.Seq T.Text))
  , stateCommits  :: S.Seq Log.Entry
  , stateFinished :: S.Seq Annot
  , stateKeys     :: M.Map Char T.Text
  , stateModal    :: Maybe Modal
  , stateStatus   :: T.Text
  } deriving (Eq, Show)


defaultSections :: [(Char, T.Text)]
defaultSections =
  [ ('f', "New features")
  , ('b', "Bug fixes")
  , ('p', "Package changes")
  , ('d', "Documentation changes")
  , ('i', "Performance improvements")
  ]

newState :: S.Seq Log.Entry -> State
newState commits = State
  { stateSections = M.fromList
    [ (name, mempty) | (_, name) <- defaultSections ]
  , stateKeys = M.fromList defaultSections
  , stateCommits = commits
  , stateFinished = S.empty
  , stateModal = Nothing
  , stateStatus = ""
  }



runApp :: S.Seq Log.Entry -> IO (M.Map T.Text (S.Seq (S.Seq T.Text)))
runApp entries = do
  let state = newState entries
      vty = Vty.mkVty Vty.defaultConfig
  final <- Brick.customMain vty Nothing app state
  return (stateSections final)


app :: Brick.App State () ()
app = Brick.App
  { Brick.appDraw         = draw
  , Brick.appChooseCursor = Brick.showFirstCursor
  , Brick.appHandleEvent  = event
  , Brick.appStartEvent   = return
  , Brick.appAttrMap      = \ _ -> Brick.forceAttrMap mempty
  }


draw :: State -> [Brick.Widget ()]
draw st
  | Just ConfirmQuitModal <- stateModal st =
      [ Brick.txt "Are you sure you want to quit? (y/n)" ]
  | otherwise =
      [ Brick.vBox
        [ Brick.str (show (stateModal st))
        , Brick.txt (stateStatus st)
        ]
      ]


type EventHandler =
  State -> Vty.Key -> [Vty.Modifier] -> Brick.EventM () (Brick.Next State)

event :: State -> Brick.BrickEvent () () -> Brick.EventM () (Brick.Next State)
event st (Brick.VtyEvent (Vty.EvKey key mod)) =
  case stateModal st of
    Nothing -> mainEvent st key mod
    Just ConfirmQuitModal -> confirmQuitEvent st key mod
    Just (AddSectionModal ()) -> addSectionEvent st key mod
event st _ = Brick.continue st


mainEvent :: EventHandler
mainEvent st (Vty.KChar ' ') [] = do
  let car S.:< cdr = S.viewl (stateCommits st)
  Brick.continue st
    { stateFinished = Skip car S.<| stateFinished st
    , stateCommits = cdr
    , stateStatus = "skipped"
    }

mainEvent st (Vty.KChar 'q') [] =
  Brick.continue st { stateModal = Just ConfirmQuitModal }

mainEvent st (Vty.KChar 'a') [] =
  Brick.continue st { stateModal = Just (AddSectionModal ()) }

mainEvent st (Vty.KChar c) []
  | Just annot <- M.lookup c (stateKeys st) = do
      let car S.:< cdr = S.viewl (stateCommits st)
      Brick.continue st
        { stateFinished = Annot annot car S.<| stateFinished st
        , stateCommits  = cdr
        , stateSections =
          M.adjust ((Log.logMessage car) S.<|) annot (stateSections st)
        , stateStatus =
          "Added " <> Log.logCommit car <> " to section " <> annot
        }
  | otherwise =
    Brick.continue st { stateStatus = "Unknown keybindings: " <> T.pack (show c) }

mainEvent st _ _ = Brick.continue st


confirmQuitEvent :: EventHandler
confirmQuitEvent st (Vty.KChar 'y') _ = Brick.halt st
confirmQuitEvent st _ _  = Brick.continue st { stateModal = Nothing }


addSectionEvent :: EventHandler
addSectionEvent st _ _ = Brick.continue st { stateModal = Nothing }
