module Objective (score, Move(..))
  where

import Interpretor (GameState(..),
                    Command(..))

data Move = Move { myMove       :: Command,
                   oponentsMove :: Command }

score :: (GameState, Move) -> (Float, (GameState, Move))
score x = (0, x)
