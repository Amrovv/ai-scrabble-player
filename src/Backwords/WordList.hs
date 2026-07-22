{-# LANGUAGE TemplateHaskell #-}

module Backwords.WordList where

import Data.FileEmbed ( embedStringFile )
import Data.List (sort)

{-
In this file we use Template Haskell (some Haskell code run at compile time) to import these word lists and throw a compiler error if we cannot find 
them. 

This means that the compiled program doesn't need these word lists at all, because they are embedded in the code!
-}

-- | The file "assets/answers" includes all words that might be chosen by the 
-- program as answers. 
-- The list is sanitised to include only sensible words.
allWords :: [String]
allWords = lines $(embedStringFile "assets/words.txt")
