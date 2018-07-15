module Bot
  where

import Interpretor (GameState(..),
                    Command(..))
import SearchSpace
import System.Random

decide :: RandomGen g => g -> GameState -> IO Command
decide = search
