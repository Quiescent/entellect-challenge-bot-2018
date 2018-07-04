module Bot
  where

import Interpretor (GameState(..),
                    Command(..))
import SearchSpace
import System.Random
import Control.Monad

decide :: RandomGen g => g -> GameState -> Command
decide gen state =
  case msum [fmap fst $ search gen state] of
    Just x  -> x
    Nothing -> NothingCommand
