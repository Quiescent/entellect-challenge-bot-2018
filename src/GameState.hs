module GameState (update, updateMissiles)
  where

import Interpretor (GameState(..),
                    Command(..))
import Missile (MissilePosition(..))

update :: GameState -> Command -> GameState
update state NothingCommand            = state
update state (Command x' y' building') = state

updateMissiles :: GameState -> [MissilePosition] -> GameState
updateMissiles state missiles = foldr (flip updateMissile) state missiles

updateMissile :: GameState -> MissilePosition -> GameState
updateMissile state (MissilePosition missileToMove' newX' newY') = state
