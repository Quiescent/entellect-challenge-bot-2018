module Building (tickBuildings, damageBuilding)
  where

import Interpretor (GameState(..),
                    Building(..),
                    BuildingType(..),
                    Player(..))
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
  mapFoldIndexed generateMissilesAndUpdateCooldownForBuilding player playerMap
  where
    playerMap = towerMap player

generateMissilesAndUpdateCooldownForBuilding :: Int -> Building -> Player -> Player
generateMissilesAndUpdateCooldownForBuilding coord
                                             (Building { weaponCooldownTimeLeft = weaponCooldownTimeLeft',
                                                         buildingType           = buildingType' })
                                             player =
  if (buildingType' /= ATTACK || weaponCooldownTimeLeft' /= 0)
  then decrementCooldown coord player
  else resetCooldownAndCreateMissile player coord attackTowerCooldownTime

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
