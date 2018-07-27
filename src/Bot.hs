module Bot
  where

import Interpretor (GameState(..),
                    Command(..))
import SearchSpace
import System.Random

decide :: StdGen -> GameState -> IO Command
decide = search
