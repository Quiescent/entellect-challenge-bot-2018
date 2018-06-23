module Bot
  where

import Interpretor (GameStateContainer(..),
                    Command(..))
import SearchSpace
import System.Random
import Control.Monad

decide :: RandomGen g => g -> GameStateContainer -> Command
decide gen (GameStateContainer state details) =
  case msum [fmap fst $ search gen details state] of
    Just x  -> x
    Nothing -> NothingCommand
