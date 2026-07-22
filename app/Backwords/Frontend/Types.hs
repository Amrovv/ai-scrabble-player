{- |
  This module contains the code for the terminal user interface (TUI) for
  Backwords. You do NOT need to worry about the contents of this file, but it
  might be of interest to those of you looking to go deeper into advanced
  Haskell programming.

  Your code should go in src/CourseworkOne.hs.
-}
module Backwords.Frontend.Types where

import Brick
import Backwords.Types hiding (PlayWord)
import Backwords.BasicGame

import Data.Set qualified as Set
import Data.Set (Set)
import qualified CourseworkOne as CW
import Backwords.Frontend.Utils

--------------------------------------------------------------------------------
-- TYPES

data TUIState = TUIState
  -- See "tick" for what the modes do
  { currentMode :: TUIMode
  , -- How long we are pausing before the next animation
    waiting :: Int
  , -- The selected multiplier and chip count, if any
    selectedScore :: Maybe Int
  ,  -- The list of words that have been played before now
    wordList :: [String]
  , -- Which cards are currently selected in the UI
    currentWord :: String
   -- The position of the animation
  , animPos :: Int
    -- Which indices are considered used
  , usedTiles :: Set Int
  -- Which indices we have "discarded" (for discard animation)
  , discardedTiles :: Set Int
  , -- True if the mouse button is currently down (avoids double clicking)
    clicked :: Bool
  , -- Whether the game should play itself using the provided AI
    isAIGame :: Bool
  , -- What the most recent AI move was
    aiMove :: Maybe String
  , -- The current state of the game
    gameState :: GameState
    -- Whether the bag is currently being shown
  , showingBag :: Bool
  }

{- | The current mode of the TUI tells the program what it should do next.
See "tick" below for how the modes are used.
-}
data TUIMode
  = PlayWord
  | WaitCorrect
  | WaitWrong
  | WaitScore
  | WaitDiscard
  | ChooseTakeTile
  | Initial
  | GameOver GOReason
  deriving (Eq, Show)

data GOReason = BagEmpty | NoWords deriving (Eq, Show)

-- A simple unit type which the terminal receives 10 times per second, to force an update
data Tick = Tick

makeInitialTUIState :: Bool -> IO TUIState
makeInitialTUIState aiModeRequested = do
  gs <- initialGameState
  ws <- tryGetOrIO [""] (CW.possibleWords (rack gs))
  let
    startState = if null ws
      then GameOver NoWords
      else PlayWord
  pure $
    TUIState
      { clicked = False
      , currentMode = startState
      , waiting = 0
      , isAIGame = aiModeRequested
      , aiMove = Nothing
      , usedTiles = Set.empty
      , discardedTiles = Set.empty
      , wordList = []
      , currentWord = []
      , animPos = 0
      , gameState = gs
      , selectedScore = Nothing
      , showingBag = False
      }

--------------------------------------------------------------------------------

-- A Name is a unique identifier for each interactable widget in the UI
data Name
  = LogViewport
  | RackTile Int
  | PlayArea
  | PlayAreaTile Int
  | TakeVowelBtn
  | TakeConsonantBtn
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Helpers

type GameUpdate = EventM Name TUIState ()
