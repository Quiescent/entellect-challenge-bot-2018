module GameDetails
  where

import Interpretor(GameDetails(..),
                   GameState(..),
                   PlayerType(..))
import Collision (CollisionType(..))

incrementPlayerHits :: PlayerType -> GameState -> GameState
incrementPlayerHits A state = state
incrementPlayerHits B state = state

updatePoints :: PlayerType -> CollisionType -> GameState -> GameState
updatePoints A HitBuilding state = state
updatePoints B HitBuilding state = state
updatePoints A HitPlayer   state = state
updatePoints B HitPlayer   state = state
