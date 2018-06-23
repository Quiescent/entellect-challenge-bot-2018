module Building (tickBuildings, damageBuilding)
  where

import Interpretor (GameState(..),
                    Building(..),
                    BuildingType(..),
                    GameDetails(..),
                    Player(..),
                    TowerStats(..),
                    Row)
import Row
import Player
import GameMap
import GameDetails
import GameState
import BuildingsUnderConstruction

tickBuildings :: GameDetails -> GameState -> GameState
tickBuildings details = (generateMissiles details) . updateBuildingProgress

generateMissiles :: GameDetails -> GameState -> GameState
generateMissiles details =
  mapMyPlayer (generateMissilesOnPlayer details) . mapOponentsPlayer (generateMissilesOnPlayer details)

generateMissilesOnPlayer :: GameDetails ->  Player -> Player
generateMissilesOnPlayer details player =
  mapFoldIndexed (generateMissilesOnRow details) player playerMap
  where
    playerMap = towerMap player

generateMissilesOnRow :: GameDetails -> Int -> Row -> Player -> Player
generateMissilesOnRow details y' row player =
  rowFoldrIndexed (generateMissilesForBuilding details y') player row

generateMissilesForBuilding :: GameDetails -> Int -> Int -> Building -> Player -> Player
generateMissilesForBuilding details x' y'
                            (Building { weaponCooldownTimeLeft = weaponCooldownTimeLeft',
                                        buildingType           = buildingType' })
                            player =
  if (buildingType' /= ATTACK || weaponCooldownTimeLeft' /= 0)
  then player
  else resetCooldownAndCreateMissile player x' y' weaponCooldownPeriod' weaponDamage' weaponSpeed'
  where
    (TowerStats { weaponCooldownPeriod = weaponCooldownPeriod',
                  weaponDamage         = weaponDamage',
                  weaponSpeed          = weaponSpeed' }) = towerStats buildingType' details

updateBuildingProgress :: GameState -> GameState
updateBuildingProgress =
  mapMyPlayer updateBuildingProgress' . mapOponentsPlayer updateBuildingProgress'

updateBuildingProgress' :: Player -> Player
updateBuildingProgress' player =
  let (constructed, newConstructionQueue) = tickConstruction $ constructionQueue player
      newTowerMap                         = foldr placeBuilding (towerMap player) constructed
  in player { constructionQueue = newConstructionQueue,
              towerMap          = newTowerMap }

damageBuilding :: Int -> Building -> Maybe Building
damageBuilding damage' building' =
  let integrity' = integrity building'
  in if integrity' <= damage'
     then Nothing
     else Just (building' { integrity = integrity' - damage' })
