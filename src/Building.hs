module Building (tickBuildings, damageBuilding)
  where

import Interpretor (GameState(..),
                    Building(..),
                    BuildingType(..),
                    Player(..),
                    Row)
import Row
import Player
import GameMap
import GameState
import BuildingsUnderConstruction
import Magic

tickBuildings :: GameState -> GameState
tickBuildings = (generateMissilesAndUpdateCooldown) . updateBuildingProgress

generateMissilesAndUpdateCooldown :: GameState -> GameState
generateMissilesAndUpdateCooldown =
  mapMyPlayer generateAndUpdateCooldownMissilesOnPlayer . mapOponentsPlayer generateAndUpdateCooldownMissilesOnPlayer

generateAndUpdateCooldownMissilesOnPlayer ::  Player -> Player
generateAndUpdateCooldownMissilesOnPlayer player =
  mapFoldIndexed (generateMissilesAndUpdateCooldownOnRow) player playerMap
  where
    playerMap = towerMap player

generateMissilesAndUpdateCooldownOnRow :: Int -> Row -> Player -> Player
generateMissilesAndUpdateCooldownOnRow y' row player =
  rowFoldrIndexed (generateMissilesAndUpdateCooldownForBuilding y') player row

generateMissilesAndUpdateCooldownForBuilding :: Int -> Int -> Building -> Player -> Player
generateMissilesAndUpdateCooldownForBuilding y' x'
                                             (Building { weaponCooldownTimeLeft = weaponCooldownTimeLeft',
                                                         buildingType           = buildingType' })
                                             player =
  if (buildingType' /= ATTACK || weaponCooldownTimeLeft' /= 0)
  then decrementCooldown x' y' player
  else resetCooldownAndCreateMissile player x' y' attackTowerCooldownTime

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
