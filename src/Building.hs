module Building (tickBuildings)
  where

import Interpretor (GameState(..), Player(..))

import GameState
import BitSetMap

tickBuildings :: GameState -> GameState
tickBuildings = generateMissilesAndUpdateCooldown . updateBuildingProgress

generateMissilesAndUpdateCooldown :: GameState -> GameState
generateMissilesAndUpdateCooldown =
  mapMyPlayer generateAndUpdateCooldownMissilesOnPlayer .
  mapOponentsPlayer generateAndUpdateCooldownMissilesOnPlayer

generateAndUpdateCooldownMissilesOnPlayer ::  Player -> Player
generateAndUpdateCooldownMissilesOnPlayer
  player@(Player { attack3Towers = attack3Towers',
                   attack2Towers = attack2Towers',
                   attack1Towers = attack1Towers',
                   attack0Towers = attack0Towers',
                   missiles0     = missiles0',
                   missiles1     = missiles1',
                   missiles2     = missiles2',
                   missiles3     = missiles3' }) =
  let missilesToBeGenerated = attack0Towers'
  -- TODO: Select a place to put them based on availability
  in player { attack3Towers = attack0Towers',
              attack2Towers = attack3Towers',
              attack1Towers = attack2Towers',
              attack0Towers = attack1Towers',
              missiles0     = addAllMissiles missilesToBeGenerated missiles0', -- TODO: Fix
              missiles1     = missiles1',
              missiles2     = missiles2',
              missiles3     = missiles3' }

updateBuildingProgress :: GameState -> GameState
updateBuildingProgress =
  mapMyPlayer updateBuildingProgress' . mapOponentsPlayer updateBuildingProgress'

updateBuildingProgress' :: Player -> Player
updateBuildingProgress'
  player@(Player { energyTowersUnderConstruction   = energyTowersUnderConstruction',
                   energyTowers                    = energyTowers',
                   attackTowersUnderConstruction   = attackTowersUnderConstruction',
                   attack0Towers                   = attack0Towers',
                   defenseTowersUnderConstruction2 = defenseTowersUnderConstruction2',
                   defenseTowersUnderConstruction1 = defenseTowersUnderConstruction1',
                   defenseTowersUnderConstruction0 = defenseTowersUnderConstruction0',
                   defense4Towers                  = defense4Towers',
                   teslaTower0ConstructionTime     = teslaTower0ConstructionTime',
                   teslaTower1ConstructionTime     = teslaTower1ConstructionTime' }) =
  player { energyTowersUnderConstruction   = emptyBuildings,
           energyTowers                    = addAllBuildings energyTowersUnderConstruction' energyTowers',
           attackTowersUnderConstruction   = emptyBuildings,
           attack0Towers                   = addAllBuildings attack0Towers' attackTowersUnderConstruction',
           defenseTowersUnderConstruction2 = emptyBuildings,
           defenseTowersUnderConstruction1 = defenseTowersUnderConstruction2',
           defenseTowersUnderConstruction0 = defenseTowersUnderConstruction1',
           defense4Towers                  = addAllBuildings defense4Towers' defenseTowersUnderConstruction0',
           teslaTower0ConstructionTime     = teslaTower0ConstructionTime' - if teslaTower0ConstructionTime' <= 0 then 0 else 1,
           teslaTower1ConstructionTime     = teslaTower1ConstructionTime' - if teslaTower1ConstructionTime' <= 0 then 0 else 1 }
