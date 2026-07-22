
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
  This module contains the code for the terminal user interface (TUI) for
  Backwords. You do NOT need to worry about the contents of this file, but it
  might be of interest to those of you looking to go deeper into advanced
  Haskell programming.

  Your code should go in src/CourseworkOne.hs.
-}
module Backwords.Frontend.Utils where

import Brick hiding (Down)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad.IO.Class (liftIO)
import Data.List qualified as List
import Data.Ord
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- UTILITY FUNCTIONS

reverseSort :: (Ord a) => [a] -> [a]
reverseSort = List.sortBy (comparing Down)

tryGet :: a -> EventM n m (Maybe a)
tryGet a = liftIO (tryGetIO a)

tryGetOr :: a -> a -> EventM n m a
tryGetOr def a = fromMaybe def <$> tryGet a

tryGetIO :: a -> IO (Maybe a)
tryGetIO a =
  fmap Just (evaluate a) `catch` \(_ :: SomeException) -> pure Nothing

tryGetOrIO :: a -> a -> IO a
tryGetOrIO def a = do
  ma <- tryGetIO a
  pure $ fromMaybe def ma

tryGetUnsafe :: a -> a -> a
tryGetUnsafe def a = fromMaybe def (unsafePerformIO $ tryGetIO a)

instance IsString AttrName where
  fromString :: String -> AttrName
  fromString = attrName
