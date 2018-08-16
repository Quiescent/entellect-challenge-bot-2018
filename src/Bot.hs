module Bot
  where

import Interpretor (GameState(..))
import SearchSpace
import System.Random

decide :: StdGen -> GameState -> IO ()
decide = search
