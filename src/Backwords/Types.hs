module Backwords.Types where
import Data.List ((\\))


-- | The Display type class allows us to render strings without requiring the 
-- laws that a Show instance has.
--
-- That makes it most suitable for rendering user-facing strings.
--
-- In "real" code, this is provided by the 'text-display' package, and has a 
-- slightly more involved definition. This is a simplified version, but just as 
-- effective :)

class Display a where

  -- | Convert a value to a user-facing string.
  display :: a -> String

  -- | A helper function to print out any Displayable value to the console.
  printOut :: a -> IO ()
  printOut = putStrLn . display


-- | A tile may be Used or Unused.
data Tile = Used Char | Unused Char
  deriving (Show, Eq)


-- | A move is one of three things:
--  
-- 1. Play a word.
-- 2. Draw a vowel from the bag.
-- 3. Draw a consonant from the bag.
data Move
  = PlayWord String
  | TakeVowel
  | TakeConsonant


-- Some helpers for knowing which letters are vowels and consonants.
-- We consider 'y' to be a consonant here.
vowels :: [Char]
vowels = "aeiou"

consonants :: [Char]
consonants = ['a'..'z'] \\ vowels

-- The initial bag of letters for a game of Backwords.
-- We use the same distribution as English Scrabble.
-- https://en.wikipedia.org/wiki/Scrabble_letter_distributions
initialBag :: [Char]
initialBag = concat
  [ replicate 12 'e'
  , replicate 9  'a'
  , replicate 9  'i'
  , replicate 8  'o'
  , replicate 6  'n'
  , replicate 6  'r'
  , replicate 6  't'
  , replicate 4  'l'
  , replicate 4  's'
  , replicate 4  'u'
  , replicate 4  'd'
  , replicate 3  'g'
  , replicate 2  'b'
  , replicate 2  'c'
  , replicate 2  'm'
  , replicate 2  'p'
  , replicate 2  'f'
  , replicate 2  'h'
  , replicate 2  'v'
  , replicate 2  'w'
  , replicate 2  'y'
  , replicate 1  'k'
  , replicate 1  'j'
  , replicate 1  'x'
  , replicate 1  'q'
  , replicate 1  'z'
  ]