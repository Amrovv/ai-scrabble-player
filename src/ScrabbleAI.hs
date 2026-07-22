
module ScrabbleAI where

import Backwords.Types
import Backwords.WordList

import Data.List
import Data.Char
import Data.Ratio
import Data.Set (fromList, toList)
import Data.Ord(comparing)
import System.Random(StdGen, mkStdGen, randomR)
import Data.Maybe(fromMaybe)
import Data.Function(on)


-- Render a single letter as a little boxed tile.
instance Display Char where
    display :: Char -> String
    display letter = "+---+\n| " ++ [toUpper letter] ++ " |\n+---+"


-- Render a whole word as a row of boxed tiles.
-- Empty input gives an empty string. The three rows (top, letters, bottom) are
-- built up separately and joined together.
instance Display String where
    display :: String -> String
    display [] = []
    display word = top ++ middle ++ bottom
                where wordLen = length word
                      top = unwords  (replicate  wordLen  "+---+") ++ "\n"
                      middle = unwords [ "| " ++ [toUpper letter] ++ " |" | letter <- word]
                      bottom = "\n" ++ unwords (replicate  wordLen  "+---+")

-- A word counts as valid if it's in the dictionary, ignoring case.
isValidWord :: String -> Bool
isValidWord word = map toLower word `elem` allWords

-- Scrabble letter values.
valueList :: [(Char, Int)]
valueList = [('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1), ('F', 4),
             ('G', 2), ('H', 4), ('I', 1), ('J', 8), ('K', 5), ('L', 1),
             ('M', 3), ('N', 1), ('O', 1), ('P', 3), ('Q', 10), ('R', 1),
             ('S', 1), ('T', 1), ('U', 1), ('V', 4), ('W', 4), ('X', 8),
             ('Y', 4), ('Z', 10)]

-- Value of one letter, or 0 if it isn't a letter.
-- lookup is partial so fromMaybe covers the miss.
letterValue :: Char -> Int
letterValue letter = fromMaybe 0 (lookup (toUpper letter) valueList)

-- Score a word. Each letter is worth double everything that comes after it, so
-- the later letters (and longer words in general) end up worth a lot more.
scoreWord :: [Char] -> Int
scoreWord [] = 0
scoreWord (a:as) = letterValue a + 2 * scoreWord as

-- Every dictionary word that can be spelled from the tiles in the rack.
-- (word \\ rack) removes the rack's letters from the word; if nothing is left
-- then the rack covers the whole word.
possibleWords :: [Char] -> [String]
possibleWords rack = [ word | word <- allWords, null (word \\ rack)]

-- Best scoring word we can play from the rack, or Nothing if none fit.
bestWord :: [Char] -> Maybe String
bestWord rack = case possibleWords rack of
    [] -> Nothing
    words -> Just (maximumBy (compare `on` scoreWord) words)


-- Go through the rack and mark each tile Used or Unused depending on whether the
-- word still needs that letter. Used letters get removed from the word as we go.
useTiles :: [Char] -> String -> [Tile]
useTiles [] _ = []
useTiles (a:as) word = (if inWord then Used a else Unused a) : useTiles as (if inWord then delete a word else word)
    where inWord = a `elem` word

-- Probability of drawing each distinct letter from the bag.
-- Dedupe the bag first, then for each letter count how often it shows up over
-- the total. fromIntegral because length is an Int but % wants Integer.
bagDistribution :: [Char] -> [(Char, Rational)]
bagDistribution bag =
    let
        bagSize = fromIntegral (length bag)
        uniqueBag = toList . fromList $ bag
    in
        [ (letter, fromIntegral (length $ filter (== letter) bag) % bagSize ) | letter <- uniqueBag]



-- The AI player.
--
-- Instead of just grabbing the top scoring word every turn, it looks a few moves
-- ahead using a Monte Carlo idea. For its best few candidate words it runs some
-- random rollouts: play the word, draw replacement tiles from the bag, then keep
-- greedily playing the best word for a few more turns. Whichever candidate has
-- the best average score is the one it plays.
--
-- Three numbers trade speed for score: how many candidate words to try, how many
-- rollouts per candidate, and how many moves deep to look. Rough averages over
-- 50 games from my own testing:
--     depth 1  ~2400 and fast
--     depth 2  ~2600, noticeably slower
--     depth 3  higher again but a lot slower
-- I settled on 7 candidates, 15 rollouts and depth 2 as a decent balance.

-- Draw a random letter from the bag. Gives back the letter and the new generator.
drawTile :: StdGen -> [Char] -> (Char, StdGen)
drawTile gen bag =
    let
        (index, newGen) = randomR (0, length bag - 1) gen
    in
        (bag !! index, newGen)

-- Refill the rack from the bag until it's full (9 tiles), passing the generator
-- through each draw. Stops early if the bag empties.
simulateBag :: StdGen -> [Char] -> [Char] -> ([Char], [Char], StdGen)
simulateBag gen bag rack
    | length rack == 9 = (bag, rack, gen)
    | null bag = (bag, rack, gen)
    | otherwise =
        let
            (newLetter, gen') = drawTile gen bag
        in
            simulateBag gen' (bag \\ [newLetter]) (rack ++ [newLetter])


-- Play out a run of moves and total up the score. depth controls how far ahead
-- to look.
simulateMoves :: StdGen -> String -> Int -> [Char] -> [Char] -> (Int, StdGen)

-- Hit the depth limit, stop here.
simulateMoves gen [] 0 bag rack = (0, gen)

-- No fixed word this step: play the best word in the rack, refill, then recurse.
simulateMoves gen [] depth bag rack =
    case bestWord rack of
        Nothing -> (0, gen)
        Just word ->
            let
                (newBag, newRack, gen') = simulateBag gen bag (rack \\ word)
                (nextScore, gen'') = simulateMoves gen' [] (depth - 1) newBag newRack
            in
                (scoreWord word + nextScore, gen'')


-- First step plays the given starting word, then it carries on greedily.
simulateMoves gen start depth bag rack =
    let
        (newBag, newRack, gen') = simulateBag gen bag rack
        (nextScore, gen'') = simulateMoves gen' [] (depth - 1) newBag newRack
    in
        (scoreWord start + nextScore, gen'')


-- Pick the best word by running rollouts on the top few candidates.
-- Take the top 7 by immediate score, score each one with averageScore, and keep
-- whichever has the best average.
simulateWord :: [Char] -> [Char] -> [Char]
simulateWord bag rack =
    let
        validWords = possibleWords rack
        topWords = take 7 (sortBy (comparing $ negate . scoreWord) validWords)
        gen = mkStdGen 15
        wordScores = map (\word -> (word, averageScore gen bag (rack \\ word) word )) topWords
        bestBranch = case wordScores of
            [] -> error "not valid words"
            _ -> fst (maximumBy (compare `on` snd) wordScores)
    in
        bestBranch


-- Average score of a starting word over 15 rollouts, each seeded differently so
-- the random draws vary between runs.
averageScore :: StdGen -> [Char] -> [Char] -> [Char] -> Int
averageScore gen bag rack word =
    let
        simScores = foldl' (\total seed ->
            let
                (result, _) = simulateMoves (mkStdGen (15 + seed)) word 2 bag rack
            in
                total + result)
         0 [1..15]
    in simScores `div` 15


-- The AI's move for a turn.
-- If the rack is full, play the word with the best simulated average. Otherwise
-- keep drawing, trying to hold at least 4 vowels.
aiMove :: [Char] -> [Char] -> Move
aiMove bag rack = case length rack of
    9 -> PlayWord (simulateWord bag rack)
    _ -> let
             hasVowels = any (`elem` vowels) bag
             hasConsonants = any (`elem` consonants) bag
             needVowels = length (filter (`elem` vowels) rack) < 4
         in
            if needVowels && hasVowels then TakeVowel
            else if hasConsonants then TakeConsonant
            else TakeVowel
