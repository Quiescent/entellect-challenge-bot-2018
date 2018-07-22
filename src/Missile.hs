module Missile (Direction(..),
                moveMissile)
  where

import Interpretor (Missile)
import Magic

data Direction = MoveLeft | MoveRight
  deriving (Eq)

moveMissile :: Direction -> Missile -> Missile
moveMissile direction missile =
  let adjustment = if direction == MoveRight then missileSpeed else (-missileSpeed)
  in missile + adjustment
