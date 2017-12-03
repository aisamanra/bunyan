{-# LANGUAGE OverloadedStrings #-}

module Bunyan.App (runApp) where

import qualified Brick
import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import qualified Bunyan.Log as Log


data Modal
  = AddSectionModal ()
  | ConfirmQuitModal
  deriving (Eq, Show)

data AnnotEntry = AnnotEntry
  { aeAnnot :: Maybe Annotation
  , aeEntry :: Log.Entry
  } deriving (Eq, Show)

data Annotation
  = Annotation T.Text
  | Skip
  deriving (Eq, Show)

data EntryZipper = EntryZipper
  { ezBefore  :: S.Seq AnnotEntry
  , ezAfter   :: S.Seq AnnotEntry
  , ezCurrent :: AnnotEntry
  } deriving (Eq, Show)

toZipper :: S.Seq Log.Entry -> EntryZipper
toZipper sq
  | car S.:< cdr <- S.viewl sq = EntryZipper
    { ezBefore  = S.empty
    , ezCurrent = AnnotEntry Nothing car
    , ezAfter   = fmap (AnnotEntry Nothing) cdr
    }
  | otherwise = error "empty history"

getCurrentCommit :: EntryZipper -> T.Text
getCurrentCommit
  = Log.logCommit . aeEntry . ezCurrent

zipperPrev :: EntryZipper -> EntryZipper
zipperPrev ez = case S.viewr (ezBefore ez) of
  S.EmptyR -> ez
  cdr S.:> car -> ez
    { ezBefore  = cdr
    , ezAfter   = ezCurrent ez S.<| ezAfter ez
    , ezCurrent = car
    }

zipperNext :: EntryZipper -> EntryZipper
zipperNext ez = case S.viewl (ezAfter ez) of
  S.EmptyL -> ez
  car S.:< cdr -> ez
    { ezBefore  = ezBefore ez S.|> ezCurrent ez
    , ezAfter   = cdr
    , ezCurrent = car
    }

annotCurrent :: T.Text -> EntryZipper -> EntryZipper
annotCurrent annot ez = ez
  { ezCurrent = (ezCurrent ez) { aeAnnot = Just (Annotation annot) } }

skipCurrent :: EntryZipper -> EntryZipper
skipCurrent ez = ez
  { ezCurrent = (ezCurrent ez) { aeAnnot = Just Skip } }

data State = State
  { stateCommits  :: EntryZipper
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
  { stateKeys = M.fromList defaultSections
  , stateCommits = toZipper commits
  , stateModal = Nothing
  , stateStatus = ""
  }

zipperToSeq :: EntryZipper -> S.Seq AnnotEntry
zipperToSeq ez =
  ezBefore ez <> S.singleton (ezCurrent ez) <> ezAfter ez

mkSections :: EntryZipper -> M.Map T.Text (S.Seq (S.Seq T.Text))
mkSections ez =
  let sq = zipperToSeq ez
  in M.unionsWith (<>) [ M.singleton annot (S.singleton (Log.logMessage entry))
                       | AnnotEntry { aeAnnot = mbAnnot
                                    , aeEntry = entry
                                    } <- F.toList sq
                       , Just (Annotation annot) <- [mbAnnot]
                       ]


runApp :: S.Seq Log.Entry -> IO (M.Map T.Text (S.Seq (S.Seq T.Text)))
runApp entries = do
  let state = newState entries
      vty = Vty.mkVty Vty.defaultConfig
  final <- Brick.customMain vty Nothing app state
  return (mkSections (stateCommits final))


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
      let cmts = stateCommits st
      in [ Brick.viewport () Brick.Vertical $ Brick.vBox $
           (map (renderEntry False) (F.toList (ezBefore cmts)) ++
            [ renderEntry True (ezCurrent cmts) ] ++
            map (renderEntry False) (F.toList (ezAfter cmts)))
      ]
  where renderEntry isFocus AnnotEntry
          { aeAnnot = annot
          , aeEntry = Log.Entry
                        { Log.logMessage = msg
                        , Log.logCommit  = cmt
                        }
          } = (if isFocus then Brick.visible else id) $ Brick.hBox
                [ if isFocus
                    then Brick.txt "> "
                    else Brick.txt "  "
                , case annot of
                    Nothing -> Brick.txt "[]"
                    Just Skip -> Brick.txt "skip"
                    Just (Annotation a) -> Brick.txt a
                , Brick.txt " | "
                , Brick.txt cmt
                , Brick.txt ": "
                , Brick.vBox (map Brick.txt (F.toList msg))
                ]


type EventHandler =
  State -> Vty.Key -> [Vty.Modifier] -> Brick.EventM () (Brick.Next State)

event :: State -> Brick.BrickEvent () () -> Brick.EventM () (Brick.Next State)
event st (Brick.VtyEvent (Vty.EvKey key md)) =
  case stateModal st of
    Nothing -> mainEvent st key md
    Just ConfirmQuitModal -> confirmQuitEvent st key md
    Just (AddSectionModal ()) -> addSectionEvent st key md
event st _ = Brick.continue st


mainEvent :: EventHandler
mainEvent st (Vty.KChar ' ') [] = do
  let commits = stateCommits st
      current = getCurrentCommit commits
  Brick.continue st
    { stateCommits = zipperNext (skipCurrent commits)
    , stateStatus = ("skipped " <> current)
    }

mainEvent st (Vty.KChar 'q') [] =
  Brick.continue st { stateModal = Just ConfirmQuitModal }

mainEvent st (Vty.KChar 'a') [] =
  Brick.continue st { stateModal = Just (AddSectionModal ()) }

mainEvent st (Vty.KChar 'j') [] =
  Brick.continue st { stateCommits = zipperNext (stateCommits st) }

mainEvent st (Vty.KChar 'k') [] =
  Brick.continue st { stateCommits = zipperPrev (stateCommits st) }

mainEvent st (Vty.KChar c) []
  | Just annot <- M.lookup c (stateKeys st) = do
      let commits = stateCommits st
          current = getCurrentCommit commits
      Brick.continue st
        { stateCommits = zipperNext (annotCurrent annot commits)
        , stateStatus = "Added " <> current <> " to " <> annot
        }
  | otherwise =
    Brick.continue st { stateStatus = "Unknown keybindings: " <> T.pack (show c) }

mainEvent st _ _ = Brick.continue st


confirmQuitEvent :: EventHandler
confirmQuitEvent st (Vty.KChar 'y') _ = Brick.halt st
confirmQuitEvent st _ _  = Brick.continue st { stateModal = Nothing }


addSectionEvent :: EventHandler
addSectionEvent st _ _ = Brick.continue st { stateModal = Nothing }
