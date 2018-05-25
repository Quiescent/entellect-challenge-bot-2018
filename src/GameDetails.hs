module GameDetails
  where

import Interpretor(GameDetails(..),
                   GameState(..),
                   PlayerType(..))
import Collision (CollisionType(..))
import Player

incrementPlayerHits :: PlayerType -> GameState -> GameState
incrementPlayerHits A = incrementMyHitsTaken
incrementPlayerHits B = incrementOponentsHitsTaken

updatePoints :: PlayerType -> CollisionType -> GameState -> GameState
updatePoints A HitBuilding state = state
updatePoints B HitBuilding state = state
updatePoints A HitPlayer   state = state
updatePoints B HitPlayer   state = state
