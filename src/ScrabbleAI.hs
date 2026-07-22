
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

-- ID: u5685831

-- Ex. 1:
-- Convert Letter to uppercase and display.
instance Display Char where
    display :: Char -> String
    display letter = "+---+\n| " ++ [toUpper letter] ++ " |\n+---+"


-- Ex. 2:
-- Return empty string for empty input
-- Split the output into 3 sections, each built separately as a list and joined into a single space-separated string
instance Display String where
    display :: String -> String
    display [] = []
    display word = top ++ middle ++ bottom
                where wordLen = length word
                      top = unwords  (replicate  wordLen  "+---+") ++ "\n"
                      middle = unwords [ "| " ++ [toUpper letter] ++ " |" | letter <- word]
                      bottom = "\n" ++ unwords (replicate  wordLen  "+---+")

-- Ex. 3:
-- Convert all letters in the word to lower, check if element of allWords
isValidWord :: String -> Bool
isValidWord word = map toLower word `elem` allWords

-- Ex. 4:
-- List of tuples of (Letter, Score)
valueList :: [(Char, Int)]
valueList = [('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1), ('F', 4),
             ('G', 2), ('H', 4), ('I', 1), ('J', 8), ('K', 5), ('L', 1),
             ('M', 3), ('N', 1), ('O', 1), ('P', 3), ('Q', 10), ('R', 1),
             ('S', 1), ('T', 1), ('U', 1), ('V', 4), ('W', 4), ('X', 8),
             ('Y', 4), ('Z', 10)]

-- Search for a key in valueList and return associated value
-- As lookup is partial, use fromMaybe to extract Just value
letterValue :: Char -> Int
letterValue letter = fromMaybe 0 (lookup (toUpper letter) valueList)

-- Ex. 5:
-- The base case for end of word.
-- The recursive case computes score of each subsequent letter
scoreWord :: [Char] -> Int
scoreWord [] = 0
scoreWord (a:as) = letterValue a + 2 * scoreWord as

-- Ex. 6:
-- Use list comprehension to construct list of all possible words
-- (word \\ rack) removes letters from word that are also in rack, null returns true if empty (all letters used)
possibleWords :: [Char] -> [String]
possibleWords rack = [ word | word <- allWords, null (word \\ rack)]

-- Ex. 7:
-- Return the highest scoring word from the rack using maximumBy, or Nothing if no valid words can be formed.
bestWord :: [Char] -> Maybe String
bestWord rack = case possibleWords rack of
    [] -> Nothing
    words -> Just (maximumBy (compare `on` scoreWord) words)

   
-- Ex. 8:
-- Evaluate each element recursively, creating a list of Used / Unused elements. 
-- Removing each `visited` letter.
useTiles :: [Char] -> String -> [Tile]
useTiles [] _ = []
useTiles (a:as) word = (if inWord then Used a else Unused a) : useTiles as (if inWord then delete a word else word)
    where inWord = a `elem` word

-- Ex. 9:
-- Remove duplicated from bag using toList . fromList and iterate over letters.
-- Create list of tuples, calculating probability by filtering through original bag and counting occurances.
-- fromIntegral used as length returns Int but Integer required for %.
bagDistribution :: [Char] -> [(Char, Rational)]
bagDistribution bag = 
    let 
        bagSize = fromIntegral (length bag)
        uniqueBag = toList . fromList $ bag
    in
        [ (letter, fromIntegral (length $ filter (== letter) bag) % bagSize ) | letter <- uniqueBag]



-- Ex. 10:
{-
For this exercise I inspired my solution on the Monte Carlo method. At the start of each turn, the AI evaluates its rack and 
identifies the top X candidate words by comparing their scores. Taking only the top X candidates reduces the total number of
simulations required, reducing computational load and test time.

For each candidate word, the AI runs N simulations to estimate the best long-term score. In each simulation, the AI plays
one of the candidate words, uses a pseudorandom generator to draw replacement letters from the bag, then repeatedly plays the 
best scoring word (refilling rack each time) for a fixed number of future moves (determined by depth parameter). 
Each simulation takes a different generator seed to ensure varied random outcome.

After all the simulations complete, the AI will select the candidate word with the highest average score.

As this method's scores and running time both depend on X, N and the depth, with depth having the greatest impact, my solution's
performance varies based on these parameters choices. There's a trade-off between speed and score quality:
    - depth 1 averages a score of 2400 but runs quickly.
    - depth 2 averages a score of 2600 but runs with moderate time.
    - depth 3 achieves higher scores but runs significantly longer.

As I am expected to provide a single set of parameters for you (marker) to test, and as recommended by Alex, 
here are different scores for various combinations of parameter choices.
Each combination was run multiple times with `stack test` to find an average for 50 tests:
    - X: 10 N: 100, depth 1 ; yields ~2405 in ~10s
    - X: 6, N: 10, depth 2 ; yields ~2583 in ~250s
    - X: 8, N: 10, depth: 2 ; yields ~2608 in ~335s
    - X: 6, N: 15, depth: 2 ; yields ~2620 in ~368s
    - X: 7 N: 15, depth 2 ; yields ~ 2632 in ~426s
    - X: 6, N: 30, depth 3 ; yields ~2693 in ~1373s
    
These tests were run on a DCS machine in CS.006. 

The combination used for the submission is X: 7, N:15, depth: 2
But I encourage you to test the other parameters.
-}

-- Draw a random letter from the bag using a pseudorandom generator.
-- Returns selected letter and updated generator.
drawTile :: StdGen -> [Char] -> (Char, StdGen)
drawTile gen bag = 
    let
        (index, newGen) = randomR (0, length bag - 1) gen
    in
        (bag !! index, newGen)

-- Fill the rack by repeatedly drawing letter from the bag until full.
-- Pass updated generator through each recursive call.
-- Breaks early if the bag is empty.
simulateBag :: StdGen -> [Char] -> [Char] -> ([Char], [Char], StdGen)
simulateBag gen bag rack
    | length rack == 9 = (bag, rack, gen)
    | null bag = (bag, rack, gen)
    | otherwise =
        let 
            (newLetter, gen') = drawTile gen bag
        in 
            simulateBag gen' (bag \\ [newLetter]) (rack ++ [newLetter])


-- Simulate a series of moves to estimate total score
-- The depth parameter controls how many moves ahead the AI should look.
simulateMoves :: StdGen -> String -> Int -> [Char] -> [Char] -> (Int, StdGen)

-- Base case: simulation depth reached, return 0.
simulateMoves gen [] 0 bag rack = (0, gen)

-- No predetermined word: find and play the highest scoring word in the rack.
-- Refill the rack (using simulateBag), and continue to the next depth.
-- Returns total score (current + further simulations) and updated generator. 
simulateMoves gen [] depth bag rack =
    case bestWord rack of
        Nothing -> (0, gen)
        Just word ->
            let 
                (newBag, newRack, gen') = simulateBag gen bag (rack \\ word)    
                (nextScore, gen'') = simulateMoves gen' [] (depth - 1) newBag newRack
            in 
                (scoreWord word + nextScore, gen'')
     

-- Use predetermined word (starting word for this simulation) and play it.
-- Returns total score (start + further simulations) and updated generator.
simulateMoves gen start depth bag rack =
    let 
        (newBag, newRack, gen') = simulateBag gen bag rack    
        (nextScore, gen'') = simulateMoves gen' [] (depth - 1) newBag newRack
    in 
        (scoreWord start + nextScore, gen'') 
      

-- Determine the best word by running Monte Carlo simulations for top X candidates.
-- Select the top X words by their scoreWord. 
-- Create a pseudorandom generator.
-- Evaluate each by simulating future moves, mapping each word to a tuple: (word, averageScore)
-- Select the word with the highest average score.
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


-- Calculate the average score of a word by running N simulations.
-- Uses fold' to accumulate scores across N simulations, each with a new generator seed
-- to produce varied random outcome.
-- Returns the total accumulated score divided by N for the average.
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


-- Determines AI's next move.
-- If the rack is full, play the word with the highest simulated average score.
-- If the rack is not full, maintain at least 4 vowels.
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