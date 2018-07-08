module Bot
  where

import Interpretor (GameState(..),
                    Command(..))
import SearchSpace
import System.Random
import Control.Monad

decide :: RandomGen g => g -> GameState -> IO Command
decide = search
