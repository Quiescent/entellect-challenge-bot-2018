module GameDetails (incrementPlayerHits, updatePointsForHits, updatePlayerPointsForEnergy)
  where

import Interpretor(GameDetails(..),
                   GameState(..),
                   PlayerType(..))
import Collision (CollisionType(..))
import Player

incrementPlayerHits :: PlayerType -> GameState -> GameState
incrementPlayerHits A = incrementMyHitsTaken
incrementPlayerHits B = incrementOponentsHitsTaken

updatePointsForHits :: PlayerType -> CollisionType -> Int -> GameState -> GameState
updatePointsForHits A HitBuilding damage state = incrementOponentsPoints damage state
updatePointsForHits B HitBuilding damage state = incrementMyPoints       damage state
updatePointsForHits A HitPlayer   damage state = incrementOponentsPoints (100 * damage) state
updatePointsForHits B HitPlayer   damage state = incrementMyPoints       (100 * damage) state

updatePlayerPointsForEnergy :: PlayerType -> Int -> GameState -> GameState
updatePlayerPointsForEnergy A energy state = state
updatePlayerPointsForEnergy B energy state = state
