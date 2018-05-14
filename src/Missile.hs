module Missile (MissilePosition(..), advanceMissiles)
  where

import Interpretor (Missile(..),
                    GameState(..))

data MissilePosition =
  MissilePosition { missileToMove :: Missile,
                    newX          :: Int,
                    newY          :: Int }

advanceMissiles :: GameState -> [MissilePosition]
advanceMissiles state = []
