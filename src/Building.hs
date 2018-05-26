module Building (tickBuildings)
  where

import Interpretor (GameState(..))

tickBuildings :: GameState -> GameState
tickBuildings = scoreBuildings . generateMissiles . updateBuildingProgress

-- TODO Implement missile generation
generateMissiles :: GameState -> GameState
generateMissiles state = state

-- TODO Implement build progress
updateBuildingProgress :: GameState -> GameState
updateBuildingProgress state = state

-- TODO Score for completed ones
scoreBuildings :: GameState -> GameState
scoreBuildings state = state
