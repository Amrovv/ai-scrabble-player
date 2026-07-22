{-# OPTIONS_GHC -Wno-x-partial #-}

-- |
--     This module contains the code for running Backwords.
--     You do NOT need to worry about the contents of this file, but it
--     might be of interest to those of you looking to go deeper into advanced
--     Haskell programming.
--
--     Your code should go in src/CourseworkOne.hs.
module Backwords.BasicGame where

import CourseworkOne
import Data.List qualified as List
import Data.Ord (Down (..), comparing)
import Backwords.Types
import System.Random (randomRIO)
import qualified CourseworkOne as CW

-- |
--   Perform a Fisher-Yates shuffle on the bag
--   https://en.wikipedia.org/wiki/Fisher-Yates_shuffle
--   N.B. NOT very efficient because of (++), but fine on 52 elements
--
--   "But Alex, Haskell functions are pure, and you said we can't do random
--   number generation" We'll get there! Don't worry about it for now :)
shuffleBag :: [Char] -> IO [Char]
shuffleBag [] = pure []
shuffleBag [x] = pure [x]
shuffleBag bag = do
  i <- randomRIO (0, length bag - 1)
  let (before, after) = splitAt i bag
  shuffled <- shuffleBag (before ++ tail after)
  pure $ head after : shuffled

-- The current state of the game.
data GameState = GameState
  { rack :: [Char], 
    bag :: [Char],
    roundNum :: Int,
    totalScore :: Int,
    pastWords :: [String]
  }
  deriving (Show)

-- The initial state of the game.
initialGameState :: IO GameState
initialGameState = do
  b <- shuffleBag initialBag
  pure
    GameState
      -- { rack = take numRack b,
        -- bag = drop numRack b,
        { 
        rack = take numRack b,
        bag = b List.\\ take numRack b,
        roundNum = 1,
        totalScore = 0,
        pastWords = []
      }

-- | How many tiles the player can hold at once.
numRack :: Int
numRack = 9

--------------------------------------------------------------------------------
-- Running the AI Directly

type AI = [Char] -> [Char] -> Move

runAIDirectly :: IO Int
runAIDirectly = runSomeAI aiMove

runSomeAI :: AI -> IO Int
runSomeAI ai =
  do
    gs <- initialGameState
    finalGS <- runLoopAI ai gs
    return $ totalScore finalGS

-- The game should end if there are no tiles left in the bag when the AI goes to draw, OR if the AI cannot make any valid words.
runLoopAI :: AI -> GameState -> IO GameState
runLoopAI ai gs
  | null (bag gs) && length (rack gs) < numRack = do
      return gs
  | null (CW.possibleWords (rack gs)) && length (rack gs) == numRack = do
      return gs
  | otherwise = do
    let
      move = ai (bag gs) (rack gs)
      newGS = applyMove move gs
    runLoopAI ai newGS

applyMove :: Move -> GameState -> GameState
applyMove (PlayWord w) gs@GameState{..} = gs
  { rack = [c | Unused c <- useTiles rack w],
    pastWords = w : pastWords,
    totalScore = totalScore + scoreWord w
  }
applyMove TakeVowel gs = takeVowel gs
applyMove TakeConsonant gs = takeConsonant gs

takeConsonant :: GameState -> GameState
takeConsonant gs@GameState {..} = do
  case List.find (`elem` consonants) bag of
    Nothing -> error "No consonants left in bag!"
    Just c -> gs
        { rack = rack ++ [c],
          bag = List.delete c bag
        }

takeVowel :: GameState -> GameState
takeVowel gs@GameState {..} = do
  case List.find (`elem` vowels) bag of
    Nothing -> error "No vowels left in bag!"
    Just c -> gs
        { rack = rack ++ [c],
          bag = List.delete c bag
        }
