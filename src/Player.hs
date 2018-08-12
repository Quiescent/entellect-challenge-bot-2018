module Player (updateEnergy,
               myPlayer,
               oponentsPlayer,
               myEnergy,
               oponentsEnergy,
               constructionTime,
               myHealth,
               oponentsHealth,
               takeDamage,
               buildingFromStats,
               build,
               updateMove,
               deconstructAt,
               buildOnMap,
               availableCoord,
               collide,
               moveCheckingBoundaries)
  where

import Interpretor (GameState(..),
                    BuildingType(..),
                    Player(..),
                    Building,
                    BuildingType(..))
import Coord
import Magic
import Towers
import Buildings
import EfficientCommand
import BitSetMap


myPlayer :: GameState -> Player
myPlayer = me

oponentsPlayer :: GameState -> Player
oponentsPlayer = oponent

playerEnergy :: (GameState -> Player) -> GameState -> Int
playerEnergy player' = energy . player'

myEnergy :: GameState -> Int
myEnergy = playerEnergy myPlayer

oponentsEnergy :: GameState -> Int
oponentsEnergy = playerEnergy oponentsPlayer

playerHealth :: (GameState -> Player) -> GameState -> Int
playerHealth player' = health . player'

myHealth :: GameState -> Int
myHealth = playerHealth myPlayer

oponentsHealth :: GameState -> Int
oponentsHealth = playerHealth oponentsPlayer

updateEnergy :: Int -> Player -> Player
updateEnergy energyToAdd player@(Player { energy = energy' }) =
  player { energy = energy' + energyToAdd }

takeDamage :: Int -> Player -> Player
takeDamage damage' player'@(Player { health = health' }) =
  player' { health = health' - damage' }

buildingFromStats :: BuildingType -> Building
buildingFromStats TESLA   = Tesla0
buildingFromStats ATTACK  = Attack0
buildingFromStats ENERGY  = EnergyTower
buildingFromStats DEFENSE = Defense4

updateMove :: EfficientCommand -> Player -> Player
-- TODO: Handle deconstruct
updateMove 0       player' = player'
updateMove command player' =
  buildOnMap coord' building' player'
  where
    coord'        = coordOfCommand command
    buildingType' = buildingTypeOfCommand command
    building'     = buildingFromStats buildingType'

constructionTime :: BuildingType -> Int
constructionTime TESLA   = teslaTowerConstructionTime
constructionTime ENERGY  = energyTowerConstructionTime
constructionTime DEFENSE = defenseTowerConstructionTime
constructionTime ATTACK  = attackTowerConstructionTime

collide :: Player -> Player -> (Player, Player)
collide playerWithMissiles@(Player { missilesOtherSide0 = missilesOtherSide0',
                                     missilesOtherSide1 = missilesOtherSide1',
                                     missilesOtherSide2 = missilesOtherSide2',
                                     missilesOtherSide3 = missilesOtherSide3' })
  player@(Player { allTowers                   = allTowers',
                   allBuiltTowers              = allBuiltTowers',
                   energyTowers                = energyTowers',
                   attack3Towers               = attack3Towers',
                   attack2Towers               = attack2Towers',
                   attack1Towers               = attack1Towers',
                   attack0Towers               = attack0Towers',
                   defense4Towers              = defense4Towers',
                   defense3Towers              = defense3Towers',
                   defense2Towers              = defense2Towers',
                   defense1Towers              = defense1Towers',
                   teslaTower0                 = teslaTower0',
                   teslaTower1                 = teslaTower1'
                   -- teslaTower0ConstructionTime = teslaTower0ConstructionTime',
                   -- teslaTower1ConstructionTime = teslaTower1ConstructionTime',
                   -- teslaTower0CooldownTime     = teslaTower0CooldownTime',
                   -- teslaTower1CooldownTime     = teslaTower1CooldownTime'
                 }) =
  -- First round of missiles
  let allPlacements   = allBuiltTowers'
      collided0       = missilesWhichCollided missilesOtherSide0' allPlacements
      missiles0After  = removeAllMissiles  collided0 missilesOtherSide0'
      energyTowers0   = removeAllBuildings collided0 energyTowers'
      attack3Towers0  = removeAllBuildings collided0 attack3Towers'
      attack2Towers0  = removeAllBuildings collided0 attack2Towers'
      attack1Towers0  = removeAllBuildings collided0 attack1Towers'
      attack0Towers0  = removeAllBuildings collided0 attack0Towers'
      defense4Towers0 = removeAllBuildings collided0 defense4Towers'
      defense3Towers0 = addAllBuildings
                        (buildingPlacementDifference defense4Towers' defense4Towers0)
                        (removeAllBuildings collided0 defense3Towers')
      defense2Towers0 = addAllBuildings
                        (buildingPlacementDifference defense3Towers' (removeAllBuildings collided0 defense3Towers'))
                        (removeAllBuildings collided0 defense2Towers')
      defense1Towers0 = addAllBuildings
                        (buildingPlacementDifference defense2Towers' (removeAllBuildings collided0 defense2Towers'))
                        (removeAllBuildings collided0 defense1Towers')
      -- TODO: Reset the cooldown of the tesla tower if it was destroyed
      teslaTower00    = removeAllBuildings collided0 teslaTower0'
      teslaTower10    = removeAllBuildings collided0 teslaTower1'
      -- Second round of missiles
      allPlacements1  = (addAllBuildings (removeAllBuildings collided0 allBuiltTowers')
                         (addAllBuildings defense3Towers0
                          (addAllBuildings defense2Towers0 defense1Towers0)))
      collided1       = missilesWhichCollided missilesOtherSide1' allPlacements1
      missiles1After  = removeAllMissiles  collided1 missilesOtherSide1'
      energyTowers1   = removeAllBuildings collided1 energyTowers0
      attack3Towers1  = removeAllBuildings collided1 attack3Towers0
      attack2Towers1  = removeAllBuildings collided1 attack2Towers0
      attack1Towers1  = removeAllBuildings collided1 attack1Towers0
      attack0Towers1  = removeAllBuildings collided1 attack0Towers0
      defense4Towers1 = removeAllBuildings collided1 defense4Towers0
      defense3Towers1 = addAllBuildings
                        (buildingPlacementDifference defense4Towers0 defense4Towers1)
                        (removeAllBuildings collided1 defense3Towers0)
      defense2Towers1 = addAllBuildings
                        (buildingPlacementDifference defense3Towers0 (removeAllBuildings collided1 defense3Towers0))
                        (removeAllBuildings collided1 defense2Towers0)
      defense1Towers1 = addAllBuildings
                        (buildingPlacementDifference defense2Towers0 (removeAllBuildings collided1 defense2Towers0))
                        (removeAllBuildings collided1 defense1Towers0)
      -- TODO: Reset the cooldown of the tesla tower if it was destroyed
      teslaTower01    = removeAllBuildings collided1 teslaTower00
      teslaTower11    = removeAllBuildings collided1 teslaTower10
      -- Third round of missiles
      allPlacements2  = (addAllBuildings (removeAllBuildings collided1 allPlacements1)
                         (addAllBuildings defense3Towers1
                          (addAllBuildings defense2Towers1 defense1Towers1)))
      collided2       = missilesWhichCollided missilesOtherSide2' allPlacements2
      missiles2After  = removeAllMissiles  collided2 missilesOtherSide2'
      energyTowers2   = removeAllBuildings collided2 energyTowers1
      attack3Towers2  = removeAllBuildings collided2 attack3Towers1
      attack2Towers2  = removeAllBuildings collided2 attack2Towers1
      attack1Towers2  = removeAllBuildings collided2 attack1Towers1
      attack0Towers2  = removeAllBuildings collided2 attack0Towers1
      defense4Towers2 = removeAllBuildings collided2 defense4Towers1
      defense3Towers2 = addAllBuildings
                        (buildingPlacementDifference defense4Towers1 defense4Towers2)
                        (removeAllBuildings collided2 defense3Towers1)
      defense2Towers2 = addAllBuildings
                        (buildingPlacementDifference defense3Towers1 (removeAllBuildings collided2 defense3Towers1))
                        (removeAllBuildings collided2 defense2Towers1)
      defense1Towers2 = addAllBuildings
                        (buildingPlacementDifference defense2Towers1 (removeAllBuildings collided2 defense2Towers1))
                        (removeAllBuildings collided2 defense1Towers1)
      -- TODO: Reset the cooldown of the tesla tower if it was destroyed
      teslaTower02    = removeAllBuildings collided2 teslaTower01
      teslaTower12    = removeAllBuildings collided2 teslaTower11
      -- Fourth round of missiles
      allPlacements3  = (addAllBuildings (removeAllBuildings collided2 allPlacements2)
                         (addAllBuildings defense3Towers2
                          (addAllBuildings defense2Towers2 defense1Towers2)))
      collided3       = missilesWhichCollided missilesOtherSide3' allPlacements3
      missiles3After  = removeAllMissiles  collided3 missilesOtherSide3'
      energyTowers3   = removeAllBuildings collided3 energyTowers2
      attack3Towers3  = removeAllBuildings collided3 attack3Towers2
      attack2Towers3  = removeAllBuildings collided3 attack2Towers2
      attack1Towers3  = removeAllBuildings collided3 attack1Towers2
      attack0Towers3  = removeAllBuildings collided3 attack0Towers2
      defense4Towers3 = removeAllBuildings collided3 defense4Towers2
      defense3Towers3 = addAllBuildings
                        (buildingPlacementDifference defense4Towers2 defense4Towers3)
                        (removeAllBuildings collided3 defense3Towers2)
      defense2Towers3 = addAllBuildings
                        (buildingPlacementDifference defense3Towers2 (removeAllBuildings collided3 defense3Towers2))
                        (removeAllBuildings collided3 defense2Towers2)
      defense1Towers3 = addAllBuildings
                        (buildingPlacementDifference defense2Towers2 (removeAllBuildings collided3 defense2Towers2))
                        (removeAllBuildings collided3 defense1Towers2)
      -- TODO: Reset the cooldown of the tesla tower if it was destroyed
      teslaTower03    = removeAllBuildings collided3 teslaTower02
      teslaTower13    = removeAllBuildings collided3 teslaTower12
      -- Final result
      allPlacements4  = (addAllBuildings (removeAllBuildings collided3 allPlacements3)
                         (addAllBuildings defense3Towers3
                          (addAllBuildings defense2Towers3 defense1Towers3)))
  in (playerWithMissiles { missilesOtherSide0 = missiles0After,
                           missilesOtherSide1 = missiles1After,
                           missilesOtherSide2 = missiles2After,
                           missilesOtherSide3 = missiles3After },
       player { allTowers      = removeAllBuildings
                                 (buildingPlacementDifference allBuiltTowers' allPlacements4)
                                 allTowers',
                allBuiltTowers = allPlacements4,
                energyTowers   = energyTowers3,
                attack3Towers  = attack3Towers3,
                attack2Towers  = attack2Towers3,
                attack1Towers  = attack1Towers3,
                attack0Towers  = attack0Towers3,
                defense4Towers = defense4Towers3,
                defense3Towers = defense3Towers3,
                defense2Towers = defense2Towers3,
                defense1Towers = defense1Towers3,
                teslaTower0    = teslaTower03,
                teslaTower1    = teslaTower13 })

moveCheckingBoundaries :: Player -> Player -> (Player, Player)
moveCheckingBoundaries
  playerToMove@(Player { missiles0          = missiles0',
                         missiles1          = missiles1',
                         missiles2          = missiles2',
                         missiles3          = missiles3',
                         missilesOtherSide0 = missilesOtherSide0',
                         missilesOtherSide1 = missilesOtherSide1',
                         missilesOtherSide2 = missilesOtherSide2',
                         missilesOtherSide3 = missilesOtherSide3'})
  playerToHit@(Player { health = health' }) =
  let missilesAboutToTransfer0  = onlyOverlappingMissiles missilesAboutToTransfer missiles0'
      missilesAboutToTransfer1  = onlyOverlappingMissiles missilesAboutToTransfer missiles1'
      missilesAboutToTransfer2  = onlyOverlappingMissiles missilesAboutToTransfer missiles2'
      missilesAboutToTransfer3  = onlyOverlappingMissiles missilesAboutToTransfer missiles3'
      missilesAboutToHitPlayer0 = onlyOverlappingMissiles missilesAboutToHitPlayer missilesOtherSide0'
      missilesAboutToHitPlayer1 = onlyOverlappingMissiles missilesAboutToHitPlayer missilesOtherSide1'
      missilesAboutToHitPlayer2 = onlyOverlappingMissiles missilesAboutToHitPlayer missilesOtherSide2'
      missilesAboutToHitPlayer3 = onlyOverlappingMissiles missilesAboutToHitPlayer missilesOtherSide3'
      hitsTaken                 = countMissiles missilesAboutToHitPlayer0 +
                                  countMissiles missilesAboutToHitPlayer1 +
                                  countMissiles missilesAboutToHitPlayer2 +
                                  countMissiles missilesAboutToHitPlayer3
      missilesOtherSide0''      = addAllMissiles missilesAboutToTransfer0 $ moveMissilesLeft $ removeAllMissiles missilesAboutToHitPlayer0 missilesOtherSide0'
      missilesOtherSide1''      = addAllMissiles missilesAboutToTransfer1 $ moveMissilesLeft $ removeAllMissiles missilesAboutToHitPlayer1 missilesOtherSide1'
      missilesOtherSide2''      = addAllMissiles missilesAboutToTransfer2 $ moveMissilesLeft $ removeAllMissiles missilesAboutToHitPlayer2 missilesOtherSide2'
      missilesOtherSide3''      = addAllMissiles missilesAboutToTransfer3 $ moveMissilesLeft $ removeAllMissiles missilesAboutToHitPlayer3 missilesOtherSide3'
      missiles0''               = moveMissilesRight $ removeAllMissiles missilesAboutToTransfer0 missiles0'
      missiles1''               = moveMissilesRight $ removeAllMissiles missilesAboutToTransfer1 missiles1'
      missiles2''               = moveMissilesRight $ removeAllMissiles missilesAboutToTransfer2 missiles2'
      missiles3''               = moveMissilesRight $ removeAllMissiles missilesAboutToTransfer3 missiles3'
  in (playerToMove { missiles0          = missiles0'',
                     missiles1          = missiles1'',
                     missiles2          = missiles2'',
                     missiles3          = missiles3'',
                     missilesOtherSide0 = missilesOtherSide0'',
                     missilesOtherSide1 = missilesOtherSide1'',
                     missilesOtherSide2 = missilesOtherSide2'',
                     missilesOtherSide3 = missilesOtherSide3'' },
      playerToHit { health = health' - hitsTaken * missileDamage })


buildOnMap :: Coord -> Building -> Player -> Player
buildOnMap coord building'
  player@(Player { allTowers                       = allTowers',
                   defenseTowersUnderConstruction2 = defenseTowersUnderConstruction2',
                   energyTowersUnderConstruction   = energyTowersUnderConstruction',
                   attackTowersUnderConstruction   = attackTowersUnderConstruction',
                   teslaTower0                     = teslaTower0',
                   teslaTower1                     = teslaTower1',
                   energy                          = energy' }) =
  case building' of
    Defense4    -> player' { defenseTowersUnderConstruction2 = addBuilding coord defenseTowersUnderConstruction2' }
    Attack0     -> player' { attackTowersUnderConstruction   = addBuilding coord attackTowersUnderConstruction' }
    EnergyTower -> player' { energyTowersUnderConstruction   = addBuilding coord energyTowersUnderConstruction' }
    Tesla0      ->
      if teslaTower0' == 0
      then player' { teslaTower0                 = addBuilding coord teslaTower0',
                     teslaTower0ConstructionTime = teslaTowerConstructionTime,
                     teslaTower0CooldownTime     = 0 }
      else player' { teslaTower1                 = addBuilding coord teslaTower1',
                     teslaTower1ConstructionTime = teslaTowerConstructionTime,
                     teslaTower1CooldownTime     = 0 }
    x           -> error $
      "Attempted to build an invalid building state (constructed buildings have zero CD and full health): " ++ show x
  where
    player' = player { energy    = energy' - towerCost building',
                       allTowers = addBuilding coord allTowers' }

-- TODO: Handle the allTowers and allBuiltTowers fields (!!!)
deconstructAt :: Coord -> Player -> Player
deconstructAt coord
  player@(Player { energyTowersUnderConstruction   = energyTowersUnderConstruction',
                   energyTowers                    = energyTowers',
                   attackTowersUnderConstruction   = attackTowersUnderConstruction',
                   attack3Towers                   = attack3Towers',
                   attack2Towers                   = attack2Towers',
                   attack1Towers                   = attack1Towers',
                   attack0Towers                   = attack0Towers',
                   defenseTowersUnderConstruction2 = defenseTowersUnderConstruction2',
                   defenseTowersUnderConstruction1 = defenseTowersUnderConstruction1',
                   defenseTowersUnderConstruction0 = defenseTowersUnderConstruction0',
                   defense4Towers                  = defense4Towers',
                   defense3Towers                  = defense3Towers',
                   defense2Towers                  = defense2Towers',
                   defense1Towers                  = defense1Towers',
                   teslaTower0                     = teslaTower0',
                   teslaTower1                     = teslaTower1' })
  | containsBuildingAt coord energyTowersUnderConstruction'   =
    player { energyTowersUnderConstruction = removeBuilding coord energyTowersUnderConstruction' }
  | containsBuildingAt coord energyTowers'                    =
    player { energyTowers = removeBuilding coord energyTowers' }
  | containsBuildingAt coord attackTowersUnderConstruction'   =
    player { attackTowersUnderConstruction = removeBuilding coord attackTowersUnderConstruction' }
  | containsBuildingAt coord attack3Towers'                   =
    player { attack3Towers = removeBuilding coord attack3Towers' }
  | containsBuildingAt coord attack2Towers'                   =
    player { attack2Towers = removeBuilding coord attack2Towers' }
  | containsBuildingAt coord attack1Towers'                   =
    player { attack1Towers = removeBuilding coord attack1Towers' }
  | containsBuildingAt coord attack0Towers'                   =
    player { attack0Towers = removeBuilding coord attack0Towers' }
  | containsBuildingAt coord defenseTowersUnderConstruction2' =
    player { defenseTowersUnderConstruction2 = removeBuilding coord defenseTowersUnderConstruction2' }
  | containsBuildingAt coord defenseTowersUnderConstruction1' =
    player { defenseTowersUnderConstruction1 = removeBuilding coord defenseTowersUnderConstruction1' }
  | containsBuildingAt coord defenseTowersUnderConstruction0' =
    player { defenseTowersUnderConstruction0 = removeBuilding coord defenseTowersUnderConstruction0' }
  | containsBuildingAt coord defense4Towers'                  =
    player { defense4Towers = removeBuilding coord defense4Towers' }
  | containsBuildingAt coord defense3Towers'                  =
    player { defense3Towers = removeBuilding coord defense3Towers' }
  | containsBuildingAt coord defense2Towers'                  =
    player { defense2Towers = removeBuilding coord defense2Towers' }
  | containsBuildingAt coord defense1Towers'                  =
    player { defense1Towers = removeBuilding coord defense1Towers' }
  | containsBuildingAt coord teslaTower0'                     =
    player { teslaTower0 = removeBuilding coord teslaTower0' }
  | containsBuildingAt coord teslaTower1'                     =
    player { teslaTower1 = removeBuilding coord teslaTower1' }
  | otherwise                                                 = player

availableCoord :: Coord -> Player -> Bool
availableCoord coord (Player { allTowers = allTowers' }) = not $ containsBuildingAt coord allTowers'
