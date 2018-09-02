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
  let missilesToAddTo0 = attack0Towers'
      missilesOn0      = addAllMissiles missilesToAddTo0 missiles0'
      missilesToAddTo1 = onlyOverlappingMissiles missilesToAddTo0 missiles0'
      missilesOn1      = addAllMissiles missilesToAddTo1 missiles1'
      missilesToAddTo2 = onlyOverlappingMissiles missilesToAddTo1 missiles1'
      missilesOn2      = addAllMissiles missilesToAddTo2 missiles2'
      missilesToAddTo3 = onlyOverlappingMissiles missilesToAddTo2 missiles2'
      missilesOn3      = addAllMissiles missilesToAddTo3 missiles3'
  in player { attack3Towers = attack0Towers',
              attack2Towers = attack3Towers',
              attack1Towers = attack2Towers',
              attack0Towers = attack1Towers',
              missiles0     = missilesOn0,
              missiles1     = missilesOn1,
              missiles2     = missilesOn2,
              missiles3     = missilesOn3 }

updateBuildingProgress :: GameState -> GameState
updateBuildingProgress =
  mapMyPlayer updateBuildingProgress' . mapOponentsPlayer updateBuildingProgress'

updateBuildingProgress' :: Player -> Player
updateBuildingProgress'
  player@(Player { allBuiltTowers                  = allBuiltTowers',
                   energyTowersUnderConstruction   = energyTowersUnderConstruction',
                   energyTowers                    = energyTowers',
                   attackTowersUnderConstruction   = attackTowersUnderConstruction',
                   attack0Towers                   = attack0Towers',
                   defenseTowersUnderConstruction2 = defenseTowersUnderConstruction2',
                   defenseTowersUnderConstruction1 = defenseTowersUnderConstruction1',
                   defenseTowersUnderConstruction0 = defenseTowersUnderConstruction0',
                   defense4Towers                  = defense4Towers' }) =
  player { allBuiltTowers                  = addAllBuildings allBuiltTowers'
                                             (addAllBuildings energyTowersUnderConstruction'
                                              (addAllBuildings attackTowersUnderConstruction'
                                               (addAllBuildings defenseTowersUnderConstruction0' 0))),
           energyTowersUnderConstruction   = emptyBuildings,
           energyTowers                    = addAllBuildings energyTowersUnderConstruction' energyTowers',
           attackTowersUnderConstruction   = emptyBuildings,
           attack0Towers                   = addAllBuildings attack0Towers' attackTowersUnderConstruction',
           defenseTowersUnderConstruction2 = emptyBuildings,
           defenseTowersUnderConstruction1 = defenseTowersUnderConstruction2',
           defenseTowersUnderConstruction0 = defenseTowersUnderConstruction1',
           defense4Towers                  = addAllBuildings defense4Towers' defenseTowersUnderConstruction0' }
